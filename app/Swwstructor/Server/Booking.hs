{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The booking store: tickets issued by Stripe checkout and their
-- redemptions (a reserved class slot). Persistence mirrors
-- "Swwstructor.Server.Store" — one JSON file under the data dir, held in an
-- 'MVar', written atomically on every change, surviving restarts.
--
-- Customer email, card and billing details stay with Stripe. This store keeps
-- ticket references, preferred names and invitation referrer labels; Stripe's
-- receipt carries the ticket code to its buyer.
--
-- A 'Ticket' is issued (idempotently, keyed by the Stripe PaymentIntent id)
-- when the payment-succeeded webhook lands; its code was minted before
-- checkout ('newTicketCode') and rode through Stripe as metadata. Cashing it
-- in ('redeemTicket') attaches a 'Redemption': the slot, the preferred name,
-- and — filled in as the side effects complete — the staff calendar event id
-- and the ledger of what has been done (what keeps the loop idempotent).
module Swwstructor.Server.Booking
  ( Ticket (..)
  , Redemption (..)
  , RedeemError (..)
  , BookingStore
  , newBookingStore
  , listTickets
  , newTicketCode
  , isTicketCode
  , issueTicket
  , redeemTicket
  , setEventId
  , claimNote
  , takenFor
  , CheckoutHold
  , CheckoutTarget (..)
  , HoldState (..)
  , HoldError (..)
  , holdErrorMessage
  , chCode
  , chTarget
  , checkoutSlot
  , chInvitation
  , chPriceId
  , chProduct
  , chSeats
  , chPrice
  , chCreatedAt
  , chExpiresAt
  , chState
  , beginCheckoutHold
  , beginInvitationCheckout
  , createInvitation
  , listInvitations
  , lookupInvitation
  , setInvitationEnabled
  , InvitationPurchase (..)
  , listInvitationPurchases
  , attachCheckoutSession
  , confirmCheckoutHold
  , releaseCheckoutHold
  , lookupCheckoutHold
  , listPendingCheckoutHolds
  , bookingAvailability
  , setPreferredName
  , readReservationEmails
  , configureReservationEmails
  , queueTestEmail
  , queueMissingReservationEmails
  , claimReservationEmail
  , finishReservationEmail
  , retryReservationEmail
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Exception (IOException, throwIO, try)
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as BS
import Data.Char (isAscii, isAlphaNum, isControl)
import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Numeric (showHex)
import Swwstructor.Block (mondayOf)
import Swwstructor.Content (SlotSpec (..), slotIdOf)
import Swwstructor.Money (Price (..), Cents (..), currencyCode, currencyOfCode)
import Swwstructor.Server.ReservationEmail
import Swwstructor.Server.Invitation
import StickyWM (JSON (JArr, JNull, JObj, JStr, JNum), decode, encode, jstr, (.:), (.:?))
import System.Directory (doesFileExist, renameFile)

-- | A reserved slot on a ticket.
data Redemption = Redemption
  { redSlotId :: !Text
  , redName :: !Text
  -- ^ the name the student asked to be called — nothing more
  , redCreatedAt :: !Text
  -- ^ ISO-8601 UTC
  , redEventId :: !(Maybe Text)
  -- ^ the staff calendar event id, once created (retried by the loop while
  -- 'Nothing')
  }
  deriving (Eq, Show)

-- | One paid class ticket.
data Ticket = Ticket
  { tkCode :: !Text
  -- ^ the reference code the student types to redeem (unique — the
  -- ticket's key); printed on their Stripe receipt
  , tkProduct :: !Text
  -- ^ display name of what was bought
  , tkPaymentRef :: !Text
  -- ^ Stripe PaymentIntent id (idempotency key for issuance)
  , tkCreatedAt :: !Text
  -- ^ ISO-8601 UTC
  , tkSeats :: !Text
  -- ^ what this ticket occupies when redeemed: @\"1\"@, @\"2\"@… seats, or
  -- @\"all\"@ (a private/whole-class ticket — the slot closes entirely).
  -- Sourced from the Stripe product's @sww_seats@ metadata; default @\"1\"@.
  , tkRedemption :: !(Maybe Redemption)
  , tkNotesSent :: ![Text]
  -- ^ ledger of one-off side effects done (@\"venue\"@: the venue
  -- address update on the staff calendar event)
  }
  deriving (Eq, Show)

-- | Why a redemption was refused (each gets its own user-facing message).
data RedeemError
  = CodeNotFound
  | AlreadyRedeemed !Redemption
  | SlotUnknown
  | SlotFull
  | WeekFull
  deriving (Eq, Show)

data BookingStore = BookingStore
  { bsVar :: !(MVar BookingData)
  , bsFile :: !FilePath
  }

-- | The local booking state owns capacity. Creating and open dated checkouts
-- occupy places until the payment adapter has established a terminal result.
-- A timestamp alone is never evidence that a payment can no longer succeed.
data HoldState
  = HoldCreating
  | HoldCheckout !Text !Text -- ^ Stripe session id and redirect URL
  | HoldConfirmed !Text -- ^ the successful PaymentIntent id
  | HoldReleased
  deriving (Eq)

data CheckoutTarget = TransferableTicket | DatedClass !SlotSpec deriving (Eq, Show)

-- | A checked, immutable purchase promise. Construction is kept in this module:
-- consumers cannot manufacture a hold without the atomic admission operation.
-- The slot snapshot survives subsequent content edits. The ticket code doubles
-- as the stable attempt reference; it is never reassigned after release.
data CheckoutHold = CheckoutHold
  { chCode :: !Text
  , chTarget :: !CheckoutTarget
  , chPriceId :: !Text
  , chProduct :: !Text
  , chSeats :: !Text
  , chPrice :: !Price
  , chCreatedAt :: !Text
  , chExpiresAt :: !Integer
  , chState :: !HoldState
  , chInvitation :: !(Maybe InvitationAttribution)
  }
  deriving (Eq)

checkoutSlot :: CheckoutHold -> Maybe SlotSpec
checkoutSlot h = case chTarget h of TransferableTicket -> Nothing; DatedClass slot -> Just slot

-- Attribution is held once on the immutable purchase promise. The ordinary
-- ticket can subsequently be transferred/redeemed without changing its source.
data InvitationPurchase = InvitationPurchase
  { ipAttribution :: !InvitationAttribution
  , ipTicket :: !Ticket
  , ipTarget :: !CheckoutTarget
  , ipPaidPrice :: !Price
  , ipConfirmedAt :: !Text -- ^ First durable confirmation, not Stripe charge time.
  } deriving (Eq, Show)

-- Expected admission alternatives are separate from storage failures, which
-- propagate to the HTTP/worker recovery owner as IOException without publishing
-- a candidate state. Explanations contain no ticket/session capabilities.
data HoldError
  = HoldNotFound
  | HoldSlotUnknown
  | HoldSlotFull
  | HoldWeekFull
  | HoldInvalidInput !Text
  | HoldConflict
  | HoldAlreadyReleased
  | HoldAlreadyConfirmed
  | HoldPaymentMismatch
  | HoldTicketNotConfirmed
  | HoldInvitationError !InvitationError
  deriving (Eq, Show)

holdErrorMessage :: HoldError -> Text
holdErrorMessage err = case err of
  HoldNotFound -> "This checkout could not be found. No booking was changed. Please return to the class list."
  HoldSlotUnknown -> "This class is no longer offered. No place was held; please choose another date."
  HoldSlotFull -> "There are not enough places left for this booking. No payment was started; please choose another class or ticket option."
  HoldWeekFull -> "That week's available classes are already scheduled. Please join an existing class or choose another week."
  HoldInvalidInput reason -> "Checkout could not be started: " <> reason <> ". No booking was changed."
  HoldConflict -> "This checkout does not match its saved booking. No booking was changed; please contact the school if you have paid."
  HoldAlreadyReleased -> "This checkout has expired. No place is reserved; please choose a class again."
  HoldAlreadyConfirmed -> "This booking is already paid and confirmed. Its place cannot be released as an abandoned checkout."
  HoldPaymentMismatch -> "The payment does not match the saved price or payment reference. Your booking needs to be checked by the school."
  HoldTicketNotConfirmed -> "A confirmed booking is required to change the preferred name. No booking was changed."
  HoldInvitationError err' -> invitationErrorMessage err'

data BookingData = BookingData ![Ticket] !ReservationEmails ![CheckoutHold] !InvitationRegistry deriving (Eq)

-- | Open the store; an absent file means no tickets yet. A present-but-corrupt
-- file prevents startup: neither paid tickets nor the email outbox may be
-- replaced by an empty store after a read/parse failure.
newBookingStore :: FilePath -> IO BookingStore
newBookingStore path = do
  exists <- doesFileExist path
  tickets <-
    if not exists
      then pure (BookingData [] emptyReservationEmails [] emptyInvitationRegistry)
      else do
        r <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
        case r of
          Left e -> throwIO e
          Right bs -> case TE.decodeUtf8' bs of
            Left _ -> fail ("Booking store is not UTF-8: " <> path <> "; refusing to overwrite it.")
            Right text -> case decode (T.unpack text) >>= bookingDataFromJSON of
              Right state -> pure state
              Left err -> fail ("Cannot load booking store " <> path <> ": " <> err <> "; refusing to overwrite it.")
  var <- newMVar tickets
  pure (BookingStore var path)

listTickets :: BookingStore -> IO [Ticket]
listTickets store = do BookingData ts _ _ _ <- readMVar (bsVar store); pure ts

listInvitations :: BookingStore -> IO [Invitation]
listInvitations store = do BookingData _ _ _ invitations <- readMVar (bsVar store); pure (registryInvitations invitations)

lookupInvitation :: BookingStore -> InvitationToken -> IO (Maybe Invitation)
lookupInvitation store token = do BookingData _ _ _ invitations <- readMVar (bsVar store); pure (registryLookupToken invitations token)

createInvitation :: BookingStore -> InvitationDraft -> IO (Either InvitationError Invitation)
createInvitation store draft = do
  bytes <- getRandomBytes 40 :: IO BS.ByteString
  now <- iso8601Now
  let hex = T.pack . concatMap (\byte -> let digits = showHex byte "" in if length digits == 1 then '0' : digits else digits) . BS.unpack
      ident = parseInvitationId ("iv_" <> hex (BS.take 8 bytes))
      token = parseInvitationToken ("in_" <> hex (BS.drop 8 bytes))
  case (ident, token) of
    (Just checkedId, Just checkedToken) -> modifyData store $ \old@(BookingData ts emails holds invitations) ->
      let invitation = invitationFromDraft checkedId checkedToken now draft
       in case registryInsert invitation invitations of
         Left err -> (old, Left err)
         Right updated -> (BookingData ts emails holds updated, Right invitation)
    _ -> fail "Invitation entropy encoding failed; no invitation was created."

setInvitationEnabled :: BookingStore -> InvitationId -> Bool -> IO (Either InvitationError Invitation)
setInvitationEnabled store ident enabled = modifyData store $ \old@(BookingData ts emails holds invitations) ->
  case registrySetEnabled ident enabled invitations of
    Left err -> (old, Left err)
    Right (updated, invitation) -> (BookingData ts emails holds updated, Right invitation)

listInvitationPurchases :: BookingStore -> IO [InvitationPurchase]
listInvitationPurchases store = do
  BookingData tickets _ holds _ <- readMVar (bsVar store)
  pure [InvitationPurchase attribution ticket (chTarget hold) (chPrice hold) (tkCreatedAt ticket)
       | hold <- holds, HoldConfirmed payment <- [chState hold], Just attribution <- [chInvitation hold]
       , ticket <- tickets, tkCode ticket == chCode hold, tkPaymentRef ticket == payment]

-- | Seat usage per slot id: total seats claimed, and whether a whole-class
-- (@\"all\"@) ticket sits on it.
seatUsage :: [Ticket] -> Map Text (Int, Bool)
seatUsage ts =
  M.fromListWith
    (\(a, x) (b, y) -> (a + b, x || y))
    [ (redSlotId r, seatQuantityUsage (tkSeats t))
    | t <- ts
    , Just r <- [tkRedemption t]
    ]

seatQuantityUsage :: Text -> (Int, Bool)
seatQuantityUsage s
  | s == "all" = (0, True)
  | otherwise = (seatCount s, False)

seatCount :: Text -> Int
seatCount s = case reads (T.unpack (T.strip s)) of
  [(n, "")] | n >= 1 -> n
  _ -> 1

-- | Seats effectively taken on a slot (a whole-class booking counts as the
-- full capacity — the slot is simply gone).
takenFor :: [Ticket] -> SlotSpec -> Int
takenFor ts = takenFromUsage (seatUsage ts)

takenFromUsage :: Map Text (Int, Bool) -> SlotSpec -> Int
takenFromUsage usage s = case M.lookup (slotIdOf s) usage of
  Nothing -> 0
  Just (n, exclusive) -> if exclusive then slotCap s else min (slotCap s) n

-- | Distinct slots in the same (Monday-based) week that already have a
-- booking — the teacher's committed sessions for that week.
occupiedWeekSessions :: [Ticket] -> [CheckoutHold] -> [SlotSpec] -> Text -> Int
occupiedWeekSessions ts holds slots wk =
  let usage = occupiedUsage ts holds
   in S.size (S.fromList [slotIdOf s | s <- slots <> mapMaybe checkoutSlot holds, mondayOf (slotDate s) == wk, takenFromUsage usage s > 0])

activeHold :: CheckoutHold -> Bool
activeHold h = case chState h of
  HoldCreating -> True
  HoldCheckout _ _ -> True
  HoldConfirmed _ -> False
  HoldReleased -> False

occupiedUsage :: [Ticket] -> [CheckoutHold] -> Map Text (Int, Bool)
occupiedUsage ts holds =
  M.unionWith combine (seatUsage ts)
    (M.fromListWith combine [(slotIdOf slot, seatQuantityUsage (chSeats h)) | h <- holds, activeHold h, Just slot <- [checkoutSlot h]])
  where combine (a, x) (b, y) = (a + b, x || y)

occupiedFor :: [Ticket] -> [CheckoutHold] -> SlotSpec -> Int
occupiedFor ts holds = takenFromUsage (occupiedUsage ts holds)

-- | One consistent availability snapshot for all renderers. Active checkout
-- holds count exactly like reservations; merely reaching their planned expiry
-- does not remove them. The reconciliation adapter owns terminal evidence.
bookingAvailability :: BookingStore -> [SlotSpec] -> IO [SlotSpec]
bookingAvailability store slots = do
  BookingData ts _ holds _ <- readMVar (bsVar store)
  let usage = occupiedUsage ts holds
  pure [s {slotTaken = takenFromUsage usage s} | s <- slots]

lookupCheckoutHold :: BookingStore -> Text -> IO (Maybe CheckoutHold)
lookupCheckoutHold store code = do
  BookingData _ _ holds _ <- readMVar (bsVar store)
  pure (lookup code [(chCode h, h) | h <- holds])

listPendingCheckoutHolds :: BookingStore -> IO [CheckoutHold]
listPendingCheckoutHolds store = do
  BookingData _ _ holds _ <- readMVar (bsVar store)
  pure (filter activeHold holds)

-- | Hold places before making the external checkout call. The caller resolves
-- the permitted price from the live catalogue and checks its booking cutoff;
-- this operation atomically owns the remaining mutable capacity prerequisite.
-- Repeating the same code and exact offer returns its original promise, even
-- if content has since changed. A changed request never reuses that code.
beginCheckoutHold :: BookingStore -> [SlotSpec] -> Int -> Text -> Text -> Text -> Text -> Text -> Price -> Integer -> IO (Either HoldError CheckoutHold)
beginCheckoutHold store slots maxWeekly code slotId priceId product' seats price' expiry = do
  now <- iso8601Now
  epoch <- floor <$> getPOSIXTime
  modifyData store (admitCheckout now epoch slots maxWeekly code (SlotChoice slotId) priceId product' seats price' expiry Nothing)

-- | Authoritative tier price id and eligible current slots are supplied by the
-- server. Price, single-person entitlement, scope and attribution come only
-- from the saved invitation. Disabling and first admission share this lock.
-- Already saved promises remain resumable after disable or content changes.
beginInvitationCheckout :: BookingStore -> [SlotSpec] -> Int -> Text -> InvitationToken -> Text -> Text -> Integer -> IO (Either HoldError CheckoutHold)
beginInvitationCheckout store slots maxWeekly code token priceId product' expiry = do
  now <- iso8601Now
  epoch <- floor <$> getPOSIXTime
  modifyData store $ \old@(BookingData _ _ holds invitations) ->
    case registryLookupToken invitations token of
      Nothing -> (old, Left (HoldInvitationError InvitationNotFound))
      Just invitation
        | not (invitationEnabled invitation) && not (any (\h -> chCode h == code && maybe False ((== invitationId invitation) . iaInvitationId) (chInvitation h)) holds) ->
            (old, Left (HoldInvitationError InvitationDisabled))
        | otherwise ->
            let target = case invitationScope invitation of GeneralTicket -> TicketChoice; EventTicket slot -> SlotChoice (slotIdOf slot)
             in admitCheckout now epoch slots maxWeekly code target priceId product' "1" (invitationPrice invitation) expiry (Just (attributionFor invitation)) old

data TargetChoice = TicketChoice | SlotChoice !Text

admitCheckout :: Text -> Integer -> [SlotSpec] -> Int -> Text -> TargetChoice -> Text -> Text -> Text -> Price -> Integer -> Maybe InvitationAttribution -> BookingData -> (BookingData, Either HoldError CheckoutHold)
admitCheckout now epoch slots maxWeekly code choice priceId product' seats price' expiry attribution old@(BookingData ts emails holds invitations) =
  let reject err = (old, Left err)
      sameTarget h = case (choice, chTarget h) of
        (TicketChoice, TransferableTicket) -> True
        (SlotChoice ident, DatedClass slot) -> ident == slotIdOf slot
        _ -> False
      matching h = sameTarget h && chPriceId h == priceId && chProduct h == product' && chSeats h == seats && chPrice h == price' && chExpiresAt h == expiry && chInvitation h == attribution
      selected = case choice of TicketChoice -> Right TransferableTicket; SlotChoice ident -> maybe (Left HoldSlotUnknown) (Right . DatedClass) (lookup ident [(slotIdOf s, s) | s <- slots])
      capacity target = case target of
        TransferableTicket -> Right ()
        DatedClass slot ->
          let taken = occupiedFor ts holds slot
              need = if seats == "all" then slotCap slot else seatCount seats
              weekAtCap = maxWeekly > 0 && occupiedWeekSessions ts holds slots (mondayOf (slotDate slot)) >= maxWeekly
           in if
             | (seats == "all" && taken > 0) || need > slotCap slot - taken -> Left HoldSlotFull
             | taken == 0 && weekAtCap -> Left HoldWeekFull
             | otherwise -> Right ()
   in case [h | h <- holds, chCode h == code] of
     h : _
       | not (matching h) -> reject HoldConflict
       | chState h == HoldReleased -> reject HoldAlreadyReleased
       | otherwise -> (old, Right h)
     [] | any ((== code) . tkCode) ts -> reject HoldConflict
     [] -> case selected of
       Left err -> reject err
       Right target -> case checkedHoldFields code target priceId product' seats price' attribution of
         Left why -> reject (HoldInvalidInput (T.pack why))
         Right ()
           | expiry <= epoch || expiry > epoch + 86400 -> reject (HoldInvalidInput "the checkout expiry must be within the next 24 hours")
           | otherwise -> case capacity target of
               Left err -> reject err
               Right () ->
                 let canonical = case target of TransferableTicket -> TransferableTicket; DatedClass slot -> DatedClass slot {slotTaken = 0}
                     h = CheckoutHold code canonical priceId product' seats price' now expiry HoldCreating attribution
                  in (BookingData ts emails (holds <> [h]) invitations, Right h)

-- | The adapter has received a successful Stripe create/retrieve response.
-- A concurrent payment event may already have confirmed the hold; attachment
-- then leaves the confirmed result intact. Session capabilities stay private.
attachCheckoutSession :: BookingStore -> Text -> Text -> Text -> IO (Either HoldError CheckoutHold)
attachCheckoutSession store code sessionId redirectUrl = modifyData store $ \old@(BookingData ts emails holds invitations) ->
  let reject err = (old, Left err)
      otherSession h = chCode h /= code && case chState h of HoldCheckout ident _ -> ident == sessionId; _ -> False
      publish h = (BookingData ts emails (replaceHold h holds) invitations, Right h)
   in if not (validReference "cs_" sessionId) || not ("https://" `T.isPrefixOf` redirectUrl) || T.length redirectUrl > 4096 || T.any isControl redirectUrl
        then reject (HoldInvalidInput "Stripe returned an invalid checkout reference")
        else if any otherSession holds then reject HoldConflict
        else case [h | h <- holds, chCode h == code] of
          [] -> reject HoldNotFound
          h : _ -> case chState h of
            HoldCreating -> publish h {chState = HoldCheckout sessionId redirectUrl}
            HoldCheckout oldId oldUrl | oldId == sessionId && oldUrl == redirectUrl -> (old, Right h)
            HoldCheckout _ _ -> reject HoldConflict
            HoldConfirmed _ -> (old, Right h)
            HoldReleased -> reject HoldAlreadyReleased

-- | A verified payment confirms its saved promise and issues an ordinary
-- ticket. A dated target is redeemed in the same write as the outbox; a
-- transferable target remains available for later ordinary redemption. This
-- does not depend on the browser returning or on mutable catalogue/content.
-- Caller authority: paidPrice and paymentRef are from a verified Stripe event
-- or authenticated API read, never from customer-supplied form fields.
confirmCheckoutHold :: BookingStore -> Text -> Text -> Price -> IO (Either HoldError (Ticket, Bool))
confirmCheckoutHold store code paymentRef paidPrice = do
  now <- iso8601Now
  modifyData store $ \old@(BookingData ts emails holds invitations) ->
    let reject err = (old, Left err)
     in case [h | h <- holds, chCode h == code] of
       [] -> reject HoldNotFound
       h : _
         | not (validReference "pi_" paymentRef) || paidPrice /= chPrice h -> reject HoldPaymentMismatch
         | chState h == HoldReleased -> reject HoldAlreadyReleased
         | otherwise -> case chState h of
             HoldConfirmed prior
               | prior /= paymentRef -> reject HoldPaymentMismatch
               | otherwise -> case [t | t <- ts, tkCode t == code, tkPaymentRef t == paymentRef, ticketMatchesTarget h t] of
                   [t] -> (old, Right (t, False))
                   _ -> reject HoldConflict
             _ | any (\t -> tkCode t == code || tkPaymentRef t == paymentRef) ts -> reject HoldConflict
             _ ->
               let redemption = fmap (\slot -> Redemption (slotIdOf slot) "" now Nothing) (checkoutSlot h)
                   ticket = Ticket code (chProduct h) paymentRef now (chSeats h) redemption []
                   messages = [bookingDelivery now ticket red slot recipient | emailsEnabled emails, recipient <- emailsRecipients emails, Just red <- [redemption], Just slot <- [checkoutSlot h]]
                   emails' = emails {emailsDeliveries = emailsDeliveries emails <> messages}
                   h' = h {chState = HoldConfirmed paymentRef}
                in (BookingData (ts <> [ticket]) emails' (replaceHold h' holds) invitations, Right (ticket, True))

ticketMatchesTarget :: CheckoutHold -> Ticket -> Bool
ticketMatchesTarget hold ticket = tkSeats ticket == chSeats hold && case chTarget hold of
  TransferableTicket -> True -- A bought-ahead ticket may have been redeemed later.
  DatedClass slot -> maybe False ((== slotIdOf slot) . redSlotId) (tkRedemption ticket)

-- | Only call after Stripe has established terminal-unpaid expiry/cancellation
-- or a definitive never-created result. A browser cancel or local timer is NOT
-- that evidence. Paid holds cannot be released through this operation.
releaseCheckoutHold :: BookingStore -> Text -> IO (Either HoldError CheckoutHold)
releaseCheckoutHold store code = modifyHold store code $ \h -> case chState h of
  HoldConfirmed _ -> Left HoldAlreadyConfirmed
  _ -> Right h {chState = HoldReleased}

-- | The bearer of the booking code can supply a preferred name after payment.
-- This does not affect payment, capacity, redemption identity or past messages.
setPreferredName :: BookingStore -> Text -> Text -> IO (Either HoldError Ticket)
setPreferredName store code name =
  modifyData store $ \old@(BookingData ts emails holds invitations) ->
    let reject err = (old, Left err)
        chosen = T.strip name
     in if T.length chosen > 100 || T.any isControl chosen
          then reject (HoldInvalidInput "the preferred name must be at most 100 characters without control characters")
          else case [t | t <- ts, tkCode t == code] of
            [t] | Just red <- tkRedemption t ->
              let t' = t {tkRedemption = Just red {redName = chosen}}
               in (BookingData (replaceTicket t' ts) emails holds invitations, Right t')
            _ -> reject HoldTicketNotConfirmed

modifyHold :: BookingStore -> Text -> (CheckoutHold -> Either HoldError CheckoutHold) -> IO (Either HoldError CheckoutHold)
modifyHold store code change = modifyData store $ \old@(BookingData ts emails holds invitations) ->
  case [h | h <- holds, chCode h == code] of
    [] -> (old, Left HoldNotFound)
    h : _ -> case change h of
      Left err -> (old, Left err)
      Right h' -> (BookingData ts emails (replaceHold h' holds) invitations, Right h')

replaceHold :: CheckoutHold -> [CheckoutHold] -> [CheckoutHold]
replaceHold h' = map (\h -> if chCode h == chCode h' then h' else h)

validReference :: Text -> Text -> Bool
validReference prefix value = prefix `T.isPrefixOf` value && T.length value > T.length prefix && T.length value <= 255 && T.all (\c -> isAscii c && (isAlphaNum c || c == '_')) value

checkedHoldFields :: Text -> CheckoutTarget -> Text -> Text -> Text -> Price -> Maybe InvitationAttribution -> Either String ()
checkedHoldFields code target priceId product' seats price'@(Price (Cents amount) _) attribution
  | not (isTicketCode code) = Left "the ticket reference is invalid"
  | not (validReference "price_" priceId) = Left "the Stripe price reference is invalid"
  | T.null (T.strip product') || T.length product' > 1000 = Left "the ticket name is empty or too long"
  | amount <= 0 = Left "a positive ticket price is required"
  | Just source <- attribution, seats /= "1" || price' /= iaPrice source || not (scopeMatches target (iaScope source)) = Left "the invitation and checkout terms differ"
  | TransferableTicket <- target, attribution == Nothing = Left "a transferable checkout requires an invitation"
  | DatedClass slot <- target, slotCap slot < 1 || slotCap slot > 1000 = Left "the class capacity must be between 1 and 1000"
  | DatedClass slot <- target, (parseTimeM True defaultTimeLocale "%F" (T.unpack (slotDate slot)) :: Maybe Day) == Nothing = Left "the class date is invalid"
  | DatedClass slot <- target, (parseTimeM True defaultTimeLocale "%R" (T.unpack (slotStart slot)) :: Maybe TimeOfDay) == Nothing = Left "the class start time is invalid"
  | DatedClass slot <- target, (parseTimeM True defaultTimeLocale "%R" (T.unpack (slotEnd slot)) :: Maybe TimeOfDay) == Nothing = Left "the class end time is invalid"
  | seats == "all" = Right ()
  | otherwise = case reads (T.unpack seats) :: [(Int, String)] of
      [(n, "")] | n >= 1 && n <= 1000 && T.pack (show n) == seats -> Right ()
      _ -> Left "the ticket must reserve a positive whole number of places or the whole class"

scopeMatches :: CheckoutTarget -> InvitationScope -> Bool
scopeMatches TransferableTicket GeneralTicket = True
scopeMatches (DatedClass slot) (EventTicket invited) = slotIdOf slot == slotIdOf invited
scopeMatches _ _ = False

-- | Issue the ticket for a succeeded payment, under the code minted before
-- checkout. Idempotent on the payment: a redelivered webhook finds the ticket
-- it already made. Returns the ticket and whether this call created it; a
-- code already held by a DIFFERENT payment is refused (never reassigned).
issueTicket :: BookingStore -> Text -> Text -> Text -> Text -> IO (Either Text (Ticket, Bool))
issueTicket store paymentRef code product' seats = do
  now <- iso8601Now
  modifyData store $ \old@(BookingData ts emails holds invitations) ->
    let unchanged result = (old, result)
     in
    case ([t | t <- ts, tkPaymentRef t == paymentRef], [t | t <- ts, tkCode t == code]) of
      (t : _, _) -> unchanged (Right (t, False))
      ([], _ : _) -> unchanged (Left "This ticket code already belongs to another payment.")
      ([], []) | any ((== code) . chCode) holds -> unchanged (Left "This checkout has a saved purchase promise and must be confirmed atomically.")
      ([], []) ->
        let t =
              Ticket
                { tkCode = code
                , tkProduct = product'
                , tkPaymentRef = paymentRef
                , tkCreatedAt = now
                , tkSeats = normSeats seats
                , tkRedemption = Nothing
                , tkNotesSent = []
                }
         in (BookingData (ts <> [t]) emails holds invitations, Right (t, True))
  where
    normSeats s
      | T.toLower (T.strip s) == "all" = "all"
      | otherwise = T.pack (show (seatCount s))

-- | Cash a ticket in for a slot. All checks and the write happen in one
-- atomic step, so two students racing for the last place cannot both win.
-- A whole-class ticket needs the slot untouched; an n-seat ticket needs n
-- free places; and a booking that would OPEN a new session is refused once
-- the week already teaches @maxWeekly@ sessions (0 = no cap).
redeemTicket :: BookingStore -> [SlotSpec] -> Int -> Text -> Text -> Text -> IO (Either RedeemError Ticket)
redeemTicket store slots maxWeekly code name slotId = do
  now <- iso8601Now
  modifyData store $ \(BookingData ts emails holds invitations) ->
    let norm = T.toUpper (T.filter (/= '-') (T.strip code))
        matches t = T.toUpper (T.filter (/= '-') (tkCode t)) == norm
        unchanged result = (BookingData ts emails holds invitations, result)
     in case [t | t <- ts, matches t] of
          [] -> unchanged (Left CodeNotFound)
          (t : _)
            | Just r <- tkRedemption t -> unchanged (Left (AlreadyRedeemed r))
            | otherwise ->
                case [s | s <- slots, slotIdOf s == slotId] of
                  [] -> unchanged (Left SlotUnknown)
                  (s : _) ->
                    let taken = occupiedFor ts holds s
                        free = slotCap s - taken
                        need
                          | tkSeats t == "all" = slotCap s
                          | otherwise = seatCount (tkSeats t)
                        opensSession = taken == 0
                        weekAtCap =
                          maxWeekly > 0
                            && occupiedWeekSessions ts holds slots (mondayOf (slotDate s)) >= maxWeekly
                     in if
                          | tkSeats t == "all" && taken > 0 -> unchanged (Left SlotFull)
                          | need > free -> unchanged (Left SlotFull)
                          | opensSession && weekAtCap -> unchanged (Left WeekFull)
                          | otherwise ->
                              let red = Redemption slotId (T.strip name) now Nothing
                                  t' = t {tkRedemption = Just red}
                                  messages =
                                    [ bookingDelivery now t' red s recipient
                                    | emailsEnabled emails, recipient <- emailsRecipients emails ]
                                  emails' = emails {emailsDeliveries = emailsDeliveries emails <> messages}
                               in (BookingData (replaceTicket t' ts) emails' holds invitations, Right t')

-- | Record the created calendar event id on a redeemed ticket.
setEventId :: BookingStore -> Text -> Text -> IO ()
setEventId store code eventId = do
  _ <- modifyStore store $ \ts ->
    ( [ if tkCode t == code
          then t {tkRedemption = (\r -> r {redEventId = Just eventId}) <$> tkRedemption t}
          else t
      | t <- ts
      ]
    , Right ()
    )
  pure ()

-- | Atomically claim a notification: 'True' means "yours to send" (the note
-- was not in the ledger and now is); 'False' means it was already sent.
claimNote :: BookingStore -> Text -> Text -> IO Bool
claimNote store code note = do
  r <- modifyStore store $ \ts ->
    case [t | t <- ts, tkCode t == code] of
      (t : _)
        | note `elem` tkNotesSent t -> (ts, Right False)
        | otherwise -> (replaceTicket t {tkNotesSent = tkNotesSent t <> [note]} ts, Right True)
      [] -> (ts, Right False)
  pure (either (const False) id r)

replaceTicket :: Ticket -> [Ticket] -> [Ticket]
replaceTicket t' = map (\t -> if tkCode t == tkCode t' then t' else t)

-- ---------------------------------------------------------------------------
-- Ticket codes
-- ---------------------------------------------------------------------------

isTicketCode :: Text -> Bool
isTicketCode code =
  T.length code == 9 && T.index code 4 == '-'
    && T.all (`elem` ("23456789ABCDEFGHJKMNPQRSTVWXYZ" :: String)) (T.take 4 code <> T.drop 5 code)

-- | A fresh 8-character code (Crockford-ish alphabet, no confusable glyphs),
-- shown as @XXXX-XXXX@. ~39 bits — unguessable at booking-form rates; still
-- checked for collision against existing tickets. Minted when checkout starts
-- (an abandoned checkout simply never becomes a ticket).
newTicketCode :: BookingStore -> IO Text
newTicketCode store = do
  BookingData ts _ holds _ <- readMVar (bsVar store)
  let go = do
        bytes <- getRandomBytes 8 :: IO BS.ByteString
        let chars = map pick (BS.unpack bytes)
            code = T.pack (take 4 chars) <> "-" <> T.pack (take 4 (drop 4 chars))
        if any ((== code) . tkCode) ts || any ((== code) . chCode) holds then go else pure code
  go
  where
    alphabet = "23456789ABCDEFGHJKMNPQRSTVWXYZ" :: String
    pick w = alphabet !! (fromIntegral w `mod` length alphabet)

iso8601Now :: IO Text
iso8601Now = do
  now <- getCurrentTime
  pure (T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now))

-- ---------------------------------------------------------------------------
-- Persistence (MVar + atomic file write, mirroring Server.Store)
-- ---------------------------------------------------------------------------

modifyStore :: BookingStore -> ([Ticket] -> ([Ticket], Either e a)) -> IO (Either e a)
modifyStore store f = modifyData store $ \(BookingData ts emails holds invitations) ->
  let (ts', result) = f ts in (BookingData ts' emails holds invitations, result)

modifyData :: BookingStore -> (BookingData -> (BookingData, Either e a)) -> IO (Either e a)
modifyData store f =
  modifyMVar (bsVar store) $ \old -> do
    let (new, result) = f old
    case result of
      Right _ | new /= old -> do
        r <- saveTickets (bsFile store) new
        case r of
          Right () -> pure ()
          Left e -> fail ("Could not save booking and email state to " <> bsFile store <> ": " <> T.unpack e <> "; no change published.")
      _ -> pure ()
    pure (new, result)

saveTickets :: FilePath -> BookingData -> IO (Either Text ())
saveTickets path state = do
  let bytes = TE.encodeUtf8 (T.pack (encode (bookingDataToJSON state)))
      tmp = path <> ".tmp"
  r <- try (BS.writeFile tmp bytes >> renameFile tmp path) :: IO (Either IOException ())
  pure (either (Left . T.pack . show) Right r)

-- ---------------------------------------------------------------------------
-- JSON codec
-- ---------------------------------------------------------------------------

bookingDataToJSON :: BookingData -> JSON
bookingDataToJSON (BookingData ts emails holds invitations) = JObj $
  [("tickets", JArr (map ticketToJSON ts)), ("reservationEmails", emailsToJSON emails), ("checkoutHolds", JArr (map holdToJSON holds))] <>
  [("invitations", invitationRegistryToJSON invitations) | not (null (registryInvitations invitations))]

bookingDataFromJSON :: JSON -> Either String BookingData
bookingDataFromJSON v = do
  tickets <- ticketsFromJSON v
  emails <- case v .:? "reservationEmails" of
    Nothing -> Right emptyReservationEmails
    Just settings -> emailsFromJSON settings
  holds <- case v .:? "checkoutHolds" of
    Nothing -> Right []
    Just (JArr entries) -> mapM holdFromJSON entries
    Just _ -> Left "bookings: 'checkoutHolds' is not an array"
  invitations <- maybe (Right emptyInvitationRegistry) invitationRegistryFromJSON (v .:? "invitations")
  if S.size (S.fromList (map chCode holds)) /= length holds
    then Left "bookings: duplicate checkout attempt references; refusing ambiguous capacity"
    else pure ()
  let sessionIds = [ident | h <- holds, HoldCheckout ident _ <- [chState h]]
  if S.size (S.fromList sessionIds) /= length sessionIds
    then Left "bookings: a checkout session belongs to multiple holds; refusing ambiguous payment ownership"
    else pure ()
  mapM_ (validateHeldTicket tickets) holds
  mapM_ (validateInvitationAttribution invitations) holds
  Right (BookingData tickets emails holds invitations)

validateInvitationAttribution :: InvitationRegistry -> CheckoutHold -> Either String ()
validateInvitationAttribution invitations hold = case chInvitation hold of
  Nothing -> Right ()
  Just attribution -> case registryLookupId invitations (iaInvitationId attribution) of
    Just invitation | attributionFor invitation == attribution -> Right ()
    _ -> Left "bookings: invitation attribution does not match its immutable registered terms"

validateHeldTicket :: [Ticket] -> CheckoutHold -> Either String ()
validateHeldTicket tickets h = case chState h of
  HoldConfirmed paymentRef -> case [t | t <- tickets, tkCode t == chCode h] of
    [t] | tkPaymentRef t == paymentRef, ticketMatchesTarget h t -> Right ()
    _ -> Left "bookings: a confirmed checkout is missing its matching paid ticket; refusing inconsistent ownership"
  _ | any ((== chCode h) . tkCode) tickets -> Left "bookings: an unpaid checkout also has a paid ticket; refusing inconsistent payment ownership"
  _ -> Right ()

readReservationEmails :: BookingStore -> IO ReservationEmails
readReservationEmails store = do BookingData _ emails _ _ <- readMVar (bsVar store); pure emails

modifyEmails :: BookingStore -> (ReservationEmails -> (ReservationEmails, a)) -> IO a
modifyEmails store f = do
  result <- modifyData store $ \(BookingData ts emails holds invitations) -> let (next, value) = f emails in (BookingData ts next holds invitations, Right value)
  either (const (fail "Unexpected reservation email store rejection")) pure result

configureReservationEmails :: BookingStore -> Bool -> [Mailbox] -> IO ()
configureReservationEmails store enabled recipients = modifyEmails store $ \s -> (s {emailsEnabled = enabled, emailsRecipients = recipients}, ())

bookingDelivery :: Text -> Ticket -> Redemption -> SlotSpec -> Mailbox -> Delivery
bookingDelivery now ticket red slot recipient =
  let d = reservationDelivery False (tkCode ticket <> ":" <> mailboxText recipient) now (tkCode ticket) (tkProduct ticket) (tkSeats ticket) slot recipient
      preferred = T.take 200 (T.unwords (T.words (redName red)))
   in d {deliveryBody = deliveryBody d <> "Preferred name: " <> (if T.null preferred then "Not supplied" else preferred) <> "\n"}

-- Explicit admin recovery for a booking made while notifications were off.
-- Existing jobs, including failures/uncertain sends, are never duplicated.
queueMissingReservationEmails :: BookingStore -> [SlotSpec] -> Text -> IO (Either Text Int)
queueMissingReservationEmails store slots code = do
  now <- iso8601Now
  modifyData store $ \old@(BookingData tickets emails holds invitations) ->
    let reject why = (old, Left why)
     in if not (emailsEnabled emails) || null (emailsRecipients emails)
          then reject "Enable reservation notifications and save recipients first. No email was queued."
          else case [t | t <- tickets, tkCode t == code] of
            [t] | Just red <- tkRedemption t -> case [s | s <- slots, slotIdOf s == redSlotId red] of
              s : _ ->
                let existing = map deliveryId (emailsDeliveries emails)
                    missing = [d | recipient <- emailsRecipients emails, let d = bookingDelivery now t red s recipient, deliveryId d `notElem` existing]
                 in (BookingData tickets emails {emailsDeliveries = emailsDeliveries emails <> missing} holds invitations, Right (length missing))
              [] -> reject "The reserved class is no longer defined. Restore it before sending a notification."
            _ -> reject "No redeemed ticket matches that code. No email was queued."

queueTestEmail :: BookingStore -> SlotSpec -> Mailbox -> IO Text
queueTestEmail store slot recipient = do
  nonce <- newTicketCode store
  now <- iso8601Now
  let ident = "test-" <> nonce
      d = reservationDelivery True ident now "TEST-ONLY" "Example double ticket" "2" slot recipient
  modifyEmails store $ \s -> (s {emailsDeliveries = emailsDeliveries s <> [d]}, ident)

-- Claim before network IO and commit the external receipt afterwards. A crash
-- between these steps remains uncertain instead of automatically duplicating.
claimReservationEmail :: BookingStore -> IO (Maybe Delivery)
claimReservationEmail store = modifyEmails store $ \s -> case filter ((== Queued) . deliveryState) (emailsDeliveries s) of
  [] -> (s, Nothing)
  d : _ -> (s {emailsDeliveries = map (\x -> if deliveryId x == deliveryId d then x {deliveryState = Sending} else x) (emailsDeliveries s)}, Just d)

finishReservationEmail :: BookingStore -> Text -> EmailState -> IO ()
finishReservationEmail store ident result = modifyEmails store $ \s ->
  (s {emailsDeliveries = map (\d -> if deliveryId d == ident && deliveryState d == Sending then d {deliveryState = result} else d) (emailsDeliveries s)}, ())

retryReservationEmail :: BookingStore -> Text -> IO Bool
retryReservationEmail store ident = modifyEmails store $ \s ->
  let eligible d = deliveryId d == ident && case deliveryState d of Failed _ -> True; Uncertain _ -> True; _ -> False
   in (s {emailsDeliveries = map (\d -> if eligible d then d {deliveryState = Queued} else d) (emailsDeliveries s)}, any eligible (emailsDeliveries s))

ticketsFromJSON :: JSON -> Either String [Ticket]
ticketsFromJSON v = do
  arr <- case v .: "tickets" of
    Right (JArr xs) -> Right xs
    Right _ -> Left "bookings: 'tickets' is not an array"
    Left e -> Left e
  mapM ticketFromJSON arr

ticketToJSON :: Ticket -> JSON
ticketToJSON t =
  JObj $
    [ ("code", JStr (T.unpack (tkCode t)))
    , ("product", JStr (T.unpack (tkProduct t)))
    , ("paymentRef", JStr (T.unpack (tkPaymentRef t)))
    , ("createdAt", JStr (T.unpack (tkCreatedAt t)))
    , ("seats", JStr (T.unpack (tkSeats t)))
    , ("notesSent", JArr [JStr (T.unpack n) | n <- tkNotesSent t])
    ]
      <> case tkRedemption t of
        Nothing -> []
        Just r ->
          [ ( "redemption"
            , JObj
                [ ("slotId", JStr (T.unpack (redSlotId r)))
                , ("name", JStr (T.unpack (redName r)))
                , ("createdAt", JStr (T.unpack (redCreatedAt r)))
                , ("eventId", maybe JNull (JStr . T.unpack) (redEventId r))
                ]
            )
          ]

ticketFromJSON :: JSON -> Either String Ticket
ticketFromJSON v = do
  code <- txt v "code"
  product' <- txt v "product"
  -- Files from before the PII-free redesign keyed tickets by Checkout
  -- session and kept the buyer's email: the email is not read (and so is
  -- gone at the next write); the session id still serves as the key.
  ref <- either (const (txt v "sessionId")) Right (txt v "paymentRef")
  created <- txt v "createdAt"
  let seats = case v .:? "seats" of
        Just (JStr s) | not (null s) -> T.pack s
        _ -> "1"
      notes = case v .:? "notesSent" of
        Just (JArr xs) -> [T.pack s | JStr s <- xs]
        _ -> []
  red <- case v .:? "redemption" of
    Nothing -> Right Nothing
    Just JNull -> Right Nothing
    Just rv -> do
      slotId <- txt rv "slotId"
      name <- txt rv "name"
      rc <- txt rv "createdAt"
      let ev = case rv .:? "eventId" of
            Just (JStr s) -> Just (T.pack s)
            _ -> Nothing
      pure (Just (Redemption slotId name rc ev))
  pure
    Ticket
      { tkCode = code
      , tkProduct = product'
      , tkPaymentRef = ref
      , tkCreatedAt = created
      , tkSeats = seats
      , tkRedemption = red
      , tkNotesSent = notes
      }
  where
    txt o k = T.pack <$> (jstr =<< o .: k)

-- Holds are optional in the legacy file format. Once present, their entire
-- capacity/price/state contract is checked at load; a corrupt hold must never
-- disappear and make places look free. Unrecognised state variants stop load.
holdToJSON :: CheckoutHold -> JSON
holdToJSON h =
  let Price (Cents amount) currency = chPrice h
      state = case chState h of
        HoldCreating -> JObj [("kind", JStr "creating")]
        HoldCheckout sessionId redirectUrl -> JObj [("kind", JStr "checkout"), ("sessionId", string sessionId), ("url", string redirectUrl)]
        HoldConfirmed paymentRef -> JObj [("kind", JStr "confirmed"), ("paymentRef", string paymentRef)]
        HoldReleased -> JObj [("kind", JStr "released")]
      targetFields = case chTarget h of
        TransferableTicket -> [("target", string "transferable")]
        DatedClass slot -> [("slot", slotToJSON slot)]
   in JObj $
        [("code", string (chCode h))] <> targetFields <>
        [ ("priceId", string (chPriceId h)), ("product", string (chProduct h))
        , ("seats", string (chSeats h)), ("amount", JNum (fromIntegral amount))
        , ("currency", string (currencyCode currency)), ("createdAt", string (chCreatedAt h))
        , ("expiresAt", JStr (show (chExpiresAt h))), ("state", state)
        ] <> [("invitation", attributionToJSON attribution) | Just attribution <- [chInvitation h]]
  where string = JStr . T.unpack

slotToJSON :: SlotSpec -> JSON
slotToJSON slot = JObj
  [ ("date", string (slotDate slot)), ("start", string (slotStart slot))
  , ("end", string (slotEnd slot)), ("area", string (slotArea slot))
  , ("capacity", JNum (fromIntegral (slotCap slot)))
  , ("description", string (slotDescription slot)) ]
  where string = JStr . T.unpack

holdFromJSON :: JSON -> Either String CheckoutHold
holdFromJSON v = do
  code <- txt v "code"
  target <- case v .:? "target" of
    Nothing -> do
      sv <- v .: "slot"
      DatedClass <$> (SlotSpec <$> txt sv "date" <*> txt sv "start" <*> txt sv "end" <*> txt sv "area" <*> int sv "capacity" <*> pure 0 <*> txt sv "description")
    Just (JStr "transferable") | v .:? "slot" == Nothing -> Right TransferableTicket
    _ -> Left "checkout hold target is unsupported or ambiguous"
  priceId <- txt v "priceId"
  product' <- txt v "product"
  seats <- txt v "seats"
  amount <- int v "amount"
  currencyText <- txt v "currency"
  currency <- maybe (Left "checkout hold has an unsupported currency") Right (currencyOfCode currencyText)
  let price' = Price (Cents amount) currency
  attribution <- mapM attributionFromJSON (v .:? "invitation")
  checkedHoldFields code target priceId product' seats price' attribution
  created <- txt v "createdAt"
  expiryText <- txt v "expiresAt"
  expiry <- case reads (T.unpack expiryText) of
    [(n, "")] | n > (0 :: Integer) -> Right n
    _ -> Left "checkout hold expiry is not a positive timestamp"
  stateValue <- v .: "state"
  kind <- txt stateValue "kind"
  state <- case kind of
    "creating" -> Right HoldCreating
    "checkout" -> do
      sessionId <- txt stateValue "sessionId"
      url <- txt stateValue "url"
      if validReference "cs_" sessionId && "https://" `T.isPrefixOf` url && T.length url <= 4096 && not (T.any isControl url)
        then Right (HoldCheckout sessionId url)
        else Left "checkout hold has an invalid Stripe session reference"
    "confirmed" -> do
      paymentRef <- txt stateValue "paymentRef"
      if validReference "pi_" paymentRef then Right (HoldConfirmed paymentRef) else Left "checkout hold has an invalid payment reference"
    "released" -> Right HoldReleased
    _ -> Left "checkout hold has an unsupported state; refusing to assume free capacity"
  pure (CheckoutHold code target priceId product' seats price' created expiry state attribution)
  where
    txt o k = T.pack <$> (jstr =<< o .: k)
    int o k = do
      value <- o .: k
      case value of
        JNum n | n >= 0 && n <= 9007199254740991
               , let whole = floor n :: Integer
               , n == fromInteger whole, whole <= toInteger (maxBound :: Int) -> Right (fromInteger whole)
        _ -> Left ("checkout hold field " <> k <> " must be a nonnegative integer")
