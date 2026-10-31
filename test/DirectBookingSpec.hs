{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, bracket, try)
import Control.Monad (unless)
import Data.Either (isLeft, isRight)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.Time.Clock.POSIX (getPOSIXTime)
import Swwstructor.Content (SlotSpec (..), slotIdOf)
import Swwstructor.Money (Currency (GBP), price)
import Swwstructor.Server.Booking
import Swwstructor.Server.ReservationEmail (emailsDeliveries, parseMailbox)
import System.Directory (createDirectory, getTemporaryDirectory, removeFile, removePathForcibly)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)

check :: String -> Bool -> IO ()
check label ok = unless ok (fail label)

right :: Either HoldError a -> IO a
right = either (fail . T.unpack . holdErrorMessage) pure

main :: IO ()
main = bracket fixture removePathForcibly $ \dir -> do
  now <- floor <$> getPOSIXTime
  let expiry = now + 1900
      one = SlotSpec "2030-01-07" "19:00" "21:00" "Kitchen" 1 0 "Mochi workshop"
      p = price GBP 80
      start store slots cap code slot seats cost = beginCheckoutHold store slots cap code (slotIdOf slot) "price_fixture" "Group class" seats cost expiry
      open name = newBookingStore (dir </> name <> ".json")
      occupied store slot = fmap (map slotTaken) (bookingAvailability store [slot])
      confirmed result = case result of Right (_, created) -> created; Left _ -> False
  store <- open "basic"
  h <- start store [one] 3 "AAAA-2222" one "1" p >>= right
  check "a new checkout owns capacity before any Stripe effect" . (== [1]) =<< occupied store one
  check "a held checkout has not yet issued a paid ticket" . null =<< listTickets store
  same <- start store [one] 3 "AAAA-2222" one "1" p >>= right
  check "an exact request retry returns its original promise" (same == h)
  conflict <- start store [one] 3 "AAAA-2222" one "1" (price GBP 81)
  check "changed parameters cannot reuse a held attempt" (conflict == Left HoldConflict)
  invalid <- start store [one] 3 "BBBB-2222" one "-1" p
  check "invalid seat semantics do not silently turn into a one-seat purchase" (isLeft invalid)
  _ <- issueTicket store "pi_gift" "GGGG-2222" "Gift" "1"
  giftRace <- redeemTicket store [one] 3 "GGGG-2222" "Guest" (slotIdOf one)
  check "old gift tickets observe pending holds" (giftRace == Left SlotFull)
  _ <- attachCheckoutSession store "AAAA-2222" "cs_fixture" "https://checkout.stripe.com/fixture" >>= right
  attached <- attachCheckoutSession store "AAAA-2222" "cs_fixture" "https://checkout.stripe.com/fixture" >>= right
  check "duplicate checkout attachment is harmless" (chState attached == HoldCheckout "cs_fixture" "https://checkout.stripe.com/fixture")
  otherSession <- attachCheckoutSession store "AAAA-2222" "cs_other" "https://checkout.stripe.com/other"
  check "a second Stripe session cannot replace the first" (otherSession == Left HoldConflict)
  wrongAmount <- confirmCheckoutHold store "AAAA-2222" "pi_booking" (price GBP 79)
  check "payment amount mismatch neither consumes the hold nor issues a ticket" (wrongAmount == Left HoldPaymentMismatch)
  recipient <- either (fail . T.unpack) pure (parseMailbox "venue@example.com")
  configureReservationEmails store True [recipient]
  booked <- confirmCheckoutHold store "AAAA-2222" "pi_booking" p
  check "verified payment creates the ticket and redemption together" (confirmed booked)
  (ticket, _) <- right booked
  check "confirmed purchase has the same ordinary ticket semantics and chosen date" (tkSeats ticket == "1" && maybe False ((== slotIdOf one) . redSlotId) (tkRedemption ticket))
  check "conversion from hold to ticket counts the places once" . (== [1]) =<< occupied store one
  check "a confirmed hold leaves no pending recovery job" . null =<< listPendingCheckoutHolds store
  emailBefore <- readReservationEmails store
  check "confirmation queues the configured reservation message atomically" (length (emailsDeliveries emailBefore) == 1)
  duplicate <- confirmCheckoutHold store "AAAA-2222" "pi_booking" p
  check "duplicate payment confirms the existing ticket without reissuing" (duplicate == Right (ticket, False))
  check "duplicate confirmation cannot duplicate notification jobs" . (== emailBefore) =<< readReservationEmails store
  wrongPayment <- confirmCheckoutHold store "AAAA-2222" "pi_other" p
  check "a second payment cannot take over an already confirmed attempt" (wrongPayment == Left HoldPaymentMismatch)
  paidRelease <- releaseCheckoutHold store "AAAA-2222"
  check "confirmed places cannot be released as abandoned checkouts" (paidRelease == Left HoldAlreadyConfirmed)
  renamed <- setPreferredName store "AAAA-2222" "  Momo  " >>= right
  check "a preferred name can be added after the place is already confirmed" (maybe False ((== "Momo") . redName) (tkRedemption renamed))
  unsafeName <- setPreferredName store "AAAA-2222" "A\nB"
  check "control characters cannot enter preferred-name output" (isLeft unsafeName)
  noBookingName <- setPreferredName store "GGGG-2222" "Gift recipient"
  check "an unredeemed gift is not changed by preferred-name editing" (noBookingName == Left HoldTicketNotConfirmed)
  reloaded <- open "basic"
  restored <- lookupCheckoutHold reloaded "AAAA-2222"
  check "confirmed promise survives restart with price and class snapshot" (maybe False (\x -> chSlot x == one && chPrice x == p && chState x == HoldConfirmed "pi_booking") restored)
  check "paid gift and dated tickets both survive restart" . ((== 2) . length) =<< listTickets reloaded

  -- An abandoned attempt is removed only by a terminal result supplied by the
  -- adapter. It never becomes an ordinary gift ticket, and its code is retired.
  abandoned <- open "abandoned"
  _ <- start abandoned [one] 3 "BBBB-2222" one "1" p >>= right
  noGiftBypass <- issueTicket abandoned "pi_bypass" "BBBB-2222" "Group" "1"
  check "generic gift issuance cannot bypass a dated hold" (isLeft noGiftBypass)
  pendingRestart <- open "abandoned"
  check "an uncertain create remains capacity-owning after restart" . (== [1]) =<< occupied pendingRestart one
  staleBytes <- BS.readFile (dir </> "abandoned.json")
  BS.writeFile (dir </> "expired-local.json") (TE.encodeUtf8 (T.replace (T.pack (show expiry)) (T.pack (show (now - 1))) (TE.decodeUtf8 staleBytes)))
  expiredLocal <- open "expired-local"
  check "local expiry alone never frees a checkout that might already be paid" . (== [1]) =<< occupied expiredLocal one
  check "expired unresolved checkout remains in the reconciliation queue" . ((== 1) . length) =<< listPendingCheckoutHolds expiredLocal
  _ <- releaseCheckoutHold pendingRestart "BBBB-2222" >>= right
  check "terminal-unpaid release restores availability" . (== [0]) =<< occupied pendingRestart one
  _ <- releaseCheckoutHold pendingRestart "BBBB-2222" >>= right
  reused <- start pendingRestart [one] 3 "BBBB-2222" one "1" p
  check "a released attempt cannot be resurrected with a stale browser retry" (reused == Left HoldAlreadyReleased)
  late <- confirmCheckoutHold pendingRestart "BBBB-2222" "pi_impossible_late" p
  check "contradictory paid-after-terminal evidence stops instead of overselling" (late == Left HoldAlreadyReleased)

  -- A paid event can arrive before the create response is attached locally.
  early <- open "early"
  _ <- start early [one] 3 "CCCC-2222" one "1" p >>= right
  _ <- confirmCheckoutHold early "CCCC-2222" "pi_early" p >>= right
  earlyAttached <- attachCheckoutSession early "CCCC-2222" "cs_early" "https://checkout.stripe.com/early" >>= right
  check "payment-before-create-response remains confirmed" (chState earlyAttached == HoldConfirmed "pi_early")

  double <- open "double"
  let three = one {slotCap = 3}
  _ <- start double [three] 3 "DDDD-2222" three "2" (price GBP 150) >>= right
  check "a double ticket holds exactly two places" . (== [2]) =<< occupied double three
  _ <- start double [three] 3 "EEEE-2222" three "1" p >>= right
  _ <- attachCheckoutSession double "DDDD-2222" "cs_double" "https://checkout.stripe.com/double" >>= right
  reusedSession <- attachCheckoutSession double "EEEE-2222" "cs_double" "https://checkout.stripe.com/double"
  check "one Stripe session cannot fund two different holds" (reusedSession == Left HoldConflict)
  full <- start double [three] 3 "FFFF-2222" three "1" p
  check "pending single and double purchases share the same capacity" (full == Left HoldSlotFull)
  _ <- confirmCheckoutHold double "DDDD-2222" "pi_double" (price GBP 150) >>= right
  check "paid double plus pending single still occupies three" . (== [3]) =<< occupied double three

  private <- open "private"
  let eight = one {slotCap = 8}
  _ <- start private [eight] 3 "HHHH-2222" eight "all" (price GBP 240) >>= right
  blockedSingle <- start private [eight] 3 "JJJJ-2222" eight "1" p
  check "a whole-class hold excludes every other ticket" (blockedSingle == Left HoldSlotFull)
  _ <- releaseCheckoutHold private "HHHH-2222" >>= right
  _ <- start private [eight] 3 "JJJJ-2222" eight "1" p >>= right
  blockedPrivate <- start private [eight] 3 "KKKK-2222" eight "all" (price GBP 240)
  check "a whole-class hold requires an untouched class" (blockedPrivate == Left HoldSlotFull)

  weekly <- open "weekly"
  let nextDay = eight {slotDate = "2030-01-08"}
  _ <- start weekly [eight, nextDay] 1 "MMMM-2222" eight "1" p >>= right
  newSession <- start weekly [eight, nextDay] 1 "NNNN-2222" nextDay "1" p
  check "an unpaid hold counts towards the weekly session limit" (newSession == Left HoldWeekFull)
  removedAuthored <- start weekly [nextDay] 1 "NNNN-2222" nextDay "1" p
  check "removing a held class from content cannot erase its weekly commitment" (removedAuthored == Left HoldWeekFull)
  _ <- start weekly [eight, nextDay] 1 "PPPP-2222" eight "1" p >>= right
  _ <- releaseCheckoutHold weekly "MMMM-2222" >>= right
  stillCommitted <- start weekly [eight, nextDay] 1 "NNNN-2222" nextDay "1" p
  check "releasing one hold does not remove another student's session commitment" (stillCommitted == Left HoldWeekFull)
  _ <- releaseCheckoutHold weekly "PPPP-2222" >>= right
  _ <- start weekly [eight, nextDay] 1 "NNNN-2222" nextDay "1" p >>= right

  -- Actual competing MVar-backed operations, not a pure-policy imitation.
  race <- open "race"
  (a, b) <- concurrently
    (start race [one] 3 "QQQQ-2222" one "1" p)
    (start race [one] 3 "RRRR-2222" one "1" p)
  check "two concurrent buyers cannot both hold the final place" (length (filter isRight [a, b]) == 1)
  let winner = case a of Right x -> x; Left _ -> case b of Right x -> x; Left _ -> error "winner assertion failed"
  configureReservationEmails race True [recipient]
  (confirmA, confirmB) <- concurrently
    (confirmCheckoutHold race (chCode winner) "pi_race" p)
    (confirmCheckoutHold race (chCode winner) "pi_race" p)
  check "concurrent duplicated webhooks issue exactly once" (length (filter confirmed [confirmA, confirmB]) == 1 && all isRight [confirmA, confirmB])
  check "concurrent duplicated webhooks enqueue exactly once" . ((== 1) . length . emailsDeliveries) =<< readReservationEmails race
  raceGift <- open "race-gift"
  _ <- issueTicket raceGift "pi_legacy" "SSSS-2222" "Gift" "1"
  (heldWon, giftWon) <- concurrently
    (isRight <$> start raceGift [one] 3 "TTTT-2222" one "1" p)
    (isRight <$> redeemTicket raceGift [one] 3 "SSSS-2222" "Guest" (slotIdOf one))
  check "gift redemption and new checkout cannot both win the final place" (heldWon /= giftWon)
  check "the cross-flow race consumes only the one available place" . (== [1]) =<< occupied raceGift one

  broken <- newBookingStore (dir </> "missing" </> "state.json")
  failed <- try (start broken [one] 3 "VVVV-2222" one "1" p) :: IO (Either IOException (Either HoldError CheckoutHold))
  check "a failed durable write fails the hold operation" (isLeft failed)
  check "failed persistence never publishes held capacity in memory" . (== [0]) =<< occupied broken one
  check "failed persistence never publishes a recovery job" . null =<< listPendingCheckoutHolds broken
  failedPaymentStore <- open "failed-payment"
  _ <- start failedPaymentStore [one] 3 "XXXX-2222" one "1" p >>= right
  configureReservationEmails failedPaymentStore True [recipient]
  createDirectory (dir </> "failed-payment.json.tmp")
  failedConfirmation <- try (confirmCheckoutHold failedPaymentStore "XXXX-2222" "pi_retry_after_disk" p) :: IO (Either IOException (Either HoldError (Ticket, Bool)))
  check "failed payment commit tells the caller to retry" (isLeft failedConfirmation)
  check "failed payment commit publishes no ticket" . null =<< listTickets failedPaymentStore
  check "failed payment commit publishes no outbox job" . null . emailsDeliveries =<< readReservationEmails failedPaymentStore
  check "failed payment commit keeps the capacity hold" . (== [1]) =<< occupied failedPaymentStore one
  removePathForcibly (dir </> "failed-payment.json.tmp")
  _ <- confirmCheckoutHold failedPaymentStore "XXXX-2222" "pi_retry_after_disk" p >>= right
  check "retry after storage repair creates exactly one reservation" . ((== 1) . length) =<< listTickets failedPaymentStore
  -- A pre-upgrade booking file remains accepted. Its ticket can still be
  -- claimed once, while malformed new hold state fails closed at startup.
  let legacyPath = dir </> "legacy.json"
  BS.writeFile legacyPath "{\"tickets\":[{\"code\":\"WWWW-2222\",\"product\":\"Gift\",\"paymentRef\":\"pi_old\",\"createdAt\":\"2026-10-01T00:00:00Z\",\"seats\":\"1\"}]}"
  legacy <- newBookingStore legacyPath
  redeemedLegacy <- redeemTicket legacy [one] 3 "WWWW-2222" "Legacy guest" (slotIdOf one)
  check "pre-upgrade purchased tickets remain claimable" (isRight redeemedLegacy)
  BS.writeFile (dir </> "corrupt.json") "{\"tickets\":[],\"checkoutHolds\":[{\"state\":\"unknown\"}]}"
  corrupt <- try (open "corrupt") :: IO (Either IOException BookingStore)
  check "corrupt hold records cannot restart with silently free capacity" (isLeft corrupt)
  putStrLn "Direct booking capacity, persistence, idempotency and compatibility checks passed."

fixture :: IO FilePath
fixture = do
  tmp <- getTemporaryDirectory
  (path, handle) <- openTempFile tmp "sww-direct-booking"
  hClose handle
  removeFile path
  createDirectory path
  pure path

concurrently :: IO a -> IO b -> IO (a, b)
concurrently left rightAction = do
  a <- newEmptyMVar
  b <- newEmptyMVar
  _ <- forkIO (left >>= putMVar a)
  _ <- forkIO (rightAction >>= putMVar b)
  (,) <$> takeMVar a <*> takeMVar b
