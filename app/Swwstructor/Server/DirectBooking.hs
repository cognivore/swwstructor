{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Buy a normal transferable ticket and redeem it as one durable operation.
-- Stripe effects are replaceable; the booking store owns the seat decisions.
module Swwstructor.Server.DirectBooking
  ( DirectBooking (..), CheckoutProgress (..), resumeCheckout, applySession
  , confirmPayment, startCheckoutReconciler
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (forever, forM_, void)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Swwstructor.Checkout (CheckoutRequest (..), CheckoutItem (..), CheckoutError (..), Url (..), lineItem)
import Swwstructor.Content (SlotSpec (..))
import Swwstructor.Server.Booking
  ( BookingStore, CheckoutHold, HoldState (..), Ticket, HoldError
  , chCode, chSlot, chPriceId, chProduct, chSeats, chPrice, chExpiresAt, chState
  , attachCheckoutSession, confirmCheckoutHold, releaseCheckoutHold
  , listPendingCheckoutHolds, holdErrorMessage
  )
import Swwstructor.Server.DirectStripe
  ( DirectStripe (..), DirectRequest (..), DirectSession (..), SessionState (..), DirectPayment (..))

data DirectBooking = DirectBooking
  { dbStore :: !BookingStore
  , dbStripe :: !DirectStripe
  , dbBaseUrl :: !Text
  , dbName :: !Text
  }

data CheckoutProgress
  = ReadyToPay !Url
  | BookingPaid !Ticket
  | CheckoutEnded
  | CheckoutPending !Text
  deriving (Eq, Show)

-- This is the immutable wire shape for persisted attempts. Future changes
-- need a stored request version (or drained Creating holds), because Stripe
-- requires identical parameters when an uncertain creation is retried.
requestFor :: DirectBooking -> CheckoutHold -> DirectRequest
requestFor ctx hold =
  let code = chCode hold
      slot = chSlot hold
      label = slotDate slot <> " " <> slotStart slot <> "–" <> slotEnd slot <> " · " <> slotArea slot
      summary = "Your class: " <> label <> ". Your place is reserved when payment succeeds."
      receipt = dbName ctx <> " class " <> label <> ". Your booking: " <> dbBaseUrl ctx <> "/booking/" <> code
      ticketLabel = slotDate slot <> " · " <> slotStart slot <> "–" <> slotEnd slot <> " · " <>
        (if chSeats hold == "1" then "1 person" else chSeats hold <> " people") <> " · " <> chProduct hold
      request = CheckoutRequest
        -- A dated line uses the server-checked price snapshot, so Stripe can
        -- name the chosen class without the gift product's redeem-later copy.
        -- The two-person price is already the total: quantity remains one.
        (ItemAdHoc (lineItem ticketLabel (chPrice hold)) :| [])
        (Url (dbBaseUrl ctx <> "/booking/" <> code))
        (Url (dbBaseUrl ctx <> "/checkout/" <> code))
        [("sww_code", code), ("sww_flow", "dated"), ("sww_product", chProduct hold), ("sww_seats", chSeats hold), ("sww_price_id", chPriceId hold)]
        (Just receipt)
  in DirectRequest request code (chExpiresAt hold) summary

-- | Resume exactly the saved attempt. A failed/uncertain create retains its
-- hold and reuses the same Stripe idempotency key and immutable request.
-- Never reuse an idempotency key after Stripe's 24-hour retention window.
resumeCheckout :: DirectBooking -> CheckoutHold -> IO (Either Text CheckoutProgress)
resumeCheckout ctx hold = case chState hold of
  HoldConfirmed payment -> fmap (fmap (BookingPaid . fst) . mapHoldError) $
    confirmCheckoutHold (dbStore ctx) (chCode hold) payment (chPrice hold)
  HoldReleased -> pure (Right CheckoutEnded)
  HoldCheckout sid _ -> do
    result <- directRetrieve (dbStripe ctx) sid
    either (pure . Left . checkoutErrorMessage) (applySession ctx hold) result
  HoldCreating -> do
    now <- floor <$> getPOSIXTime
    if now > chExpiresAt hold + 22 * 3600
      then pure (Left "Checkout creation still needs reconciliation by the school. Its places remain protected; no new payment was created.")
      else do
        result <- directCreate (dbStripe ctx) (requestFor ctx hold)
        case result of
          Right session -> applySession ctx hold session
          -- A 400 returned for this exact idempotent create is definitive:
          -- Stripe has rejected the request rather than created a session.
          -- 409, 429 and 5xx outcomes remain uncertain and retain capacity.
          Left err@(CheckoutHttpStatus code _) | code == 400 -> do
            released <- releaseCheckoutHold (dbStore ctx) (chCode hold)
            pure $ case released of
              Right _ -> Left (checkoutErrorMessage err <> " No place is reserved; return to the class list to try again.")
              Left e -> Left (holdErrorMessage e)
          Left err -> pure (Left (checkoutErrorMessage err))

-- | Reconcile only the session attached to this attempt. Success can race
-- attachment: confirmCheckoutHold handles both Creating and Open atomically.
applySession :: DirectBooking -> CheckoutHold -> DirectSession -> IO (Either Text CheckoutProgress)
applySession ctx hold session
  | dsCode session /= chCode hold || dsExpiresAt session /= chExpiresAt hold =
      pure (Left "Stripe's checkout reference or expiry differs from the saved booking; its places remain held for reconciliation.")
  | HoldCheckout sid _ <- chState hold, sid /= dsId session =
      pure (Left "Stripe returned a different checkout session; no booking was changed.")
  | otherwise = case dsState session of
      SessionPaid payment amount -> fmap (fmap (BookingPaid . fst) . mapHoldError) $
        confirmCheckoutHold (dbStore ctx) (chCode hold) payment amount
      SessionExpired -> fmap (fmap (const CheckoutEnded) . mapHoldError) $
        releaseCheckoutHold (dbStore ctx) (chCode hold)
      SessionUnpaidComplete -> pure (Right (CheckoutPending "Payment is still being confirmed. Your places remain held; this page will update shortly."))
      SessionOpen url@(Url location) -> do
        saved <- attachCheckoutSession (dbStore ctx) (chCode hold) (dsId session) location
        case saved of
          Left e -> pure (Left (holdErrorMessage e))
          Right fresh -> case chState fresh of
            HoldConfirmed _ -> resumeCheckout ctx fresh
            HoldReleased -> pure (Right CheckoutEnded)
            _ -> do
              now <- floor <$> getPOSIXTime
              if now < dsExpiresAt session
                then pure (Right (ReadyToPay url))
                else do
                  -- Even after the local deadline, Stripe must establish that
                  -- the session is terminal before capacity is released.
                  ended <- directExpire (dbStripe ctx) (dsId session)
                  case ended of
                    Right expired | dsState expired == SessionExpired -> applySession ctx fresh expired
                    Right changed -> case dsState changed of
                      SessionPaid _ _ -> applySession ctx fresh changed
                      _ -> pure (Left "Checkout expiry has not been confirmed. Its places remain held until Stripe confirms the outcome.")
                    Left err -> pure (Left (checkoutErrorMessage err))

confirmPayment :: DirectBooking -> DirectPayment -> IO (Either Text (Ticket, Bool))
confirmPayment ctx payment = mapHoldError <$> confirmCheckoutHold (dbStore ctx) (dpCode payment) (dpId payment) (dpPrice payment)

mapHoldError :: Either HoldError a -> Either Text a
mapHoldError = either (Left . holdErrorMessage) Right

checkoutErrorMessage :: CheckoutError -> Text
checkoutErrorMessage err = case err of
  CheckoutTransport reason -> reason
  CheckoutHttpStatus code context -> "Stripe checkout returned HTTP " <> T.pack (show code) <> ". " <> context
  CheckoutBadResponse context -> context

-- | One bounded network operation at a time; pending state survives restart.
-- The worker neither sends messages nor requires the customer's return page.
startCheckoutReconciler :: DirectBooking -> IO ()
startCheckoutReconciler ctx = void . forkIO . forever $ do
  pending <- listPendingCheckoutHolds (dbStore ctx)
  forM_ pending $ \hold -> do
    result <- try (resumeCheckout ctx hold) :: IO (Either IOException (Either Text CheckoutProgress))
    case result of
      Left _ -> putStrLn "[dated checkout] durable reconciliation failed; places remain held, retrying next cycle. Native filesystem details withheld."
      Right (Left _) -> putStrLn "[dated checkout] payment state is unresolved; places remain held, retrying next cycle."
      Right (Right _) -> pure ()
  threadDelay (60 * 1000000)
