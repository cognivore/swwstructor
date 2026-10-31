{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The booking store: tickets issued by Stripe checkout and their
-- redemptions (a reserved class slot). Persistence mirrors
-- "Swwstructor.Server.Store" — one JSON file under the data dir, held in an
-- 'MVar', written atomically on every change, surviving restarts.
--
-- No personal data lives here: a ticket is a reference code, what was
-- bought, and — once reserved — a slot and the name the student chose to be
-- called. Everything that identifies a person (email, card, billing details)
-- stays with Stripe; the Stripe receipt is what carries the code to them.
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Swwstructor.Block (mondayOf)
import Swwstructor.Content (SlotSpec, slotCap, slotDate, slotIdOf)
import Swwstructor.Server.ReservationEmail
import StickyWM (JSON (JArr, JNull, JObj, JStr), decode, encode, jstr, (.:), (.:?))
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

data BookingData = BookingData ![Ticket] !ReservationEmails deriving (Eq)

-- | Open the store; an absent file means no tickets yet. A present-but-corrupt
-- file prevents startup: neither paid tickets nor the email outbox may be
-- replaced by an empty store after a read/parse failure.
newBookingStore :: FilePath -> IO BookingStore
newBookingStore path = do
  exists <- doesFileExist path
  tickets <-
    if not exists
      then pure (BookingData [] emptyReservationEmails)
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
listTickets store = do BookingData ts _ <- readMVar (bsVar store); pure ts

-- | Seat usage per slot id: total seats claimed, and whether a whole-class
-- (@\"all\"@) ticket sits on it.
seatUsage :: [Ticket] -> Map Text (Int, Bool)
seatUsage ts =
  M.fromListWith
    (\(a, x) (b, y) -> (a + b, x || y))
    [ (redSlotId r, usage (tkSeats t))
    | t <- ts
    , Just r <- [tkRedemption t]
    ]
  where
    usage s
      | s == "all" = (0, True)
      | otherwise = (seatCount s, False)

seatCount :: Text -> Int
seatCount s = case reads (T.unpack (T.strip s)) of
  [(n, "")] | n >= 1 -> n
  _ -> 1

-- | Seats effectively taken on a slot (a whole-class booking counts as the
-- full capacity — the slot is simply gone).
takenFor :: [Ticket] -> SlotSpec -> Int
takenFor ts s = case M.lookup (slotIdOf s) (seatUsage ts) of
  Nothing -> 0
  Just (n, exclusive) -> if exclusive then slotCap s else min (slotCap s) n

-- | Distinct slots in the same (Monday-based) week that already have a
-- booking — the teacher's committed sessions for that week.
weekSessions :: [Ticket] -> [SlotSpec] -> Text -> Int
weekSessions ts slots wk =
  length [() | s <- slots, mondayOf (slotDate s) == wk, takenFor ts s > 0]

-- | Issue the ticket for a succeeded payment, under the code minted before
-- checkout. Idempotent on the payment: a redelivered webhook finds the ticket
-- it already made. Returns the ticket and whether this call created it; a
-- code already held by a DIFFERENT payment is refused (never reassigned).
issueTicket :: BookingStore -> Text -> Text -> Text -> Text -> IO (Either Text (Ticket, Bool))
issueTicket store paymentRef code product' seats = do
  now <- iso8601Now
  modifyStore store $ \ts ->
    case ([t | t <- ts, tkPaymentRef t == paymentRef], [t | t <- ts, tkCode t == code]) of
      (t : _, _) -> (ts, Right (t, False))
      ([], _ : _) -> (ts, Left ("code " <> code <> " already belongs to another payment"))
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
         in (ts <> [t], Right (t, True))
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
  modifyData store $ \(BookingData ts emails) ->
    let norm = T.toUpper (T.filter (/= '-') (T.strip code))
        matches t = T.toUpper (T.filter (/= '-') (tkCode t)) == norm
        unchanged result = (BookingData ts emails, result)
     in case [t | t <- ts, matches t] of
          [] -> unchanged (Left CodeNotFound)
          (t : _)
            | Just r <- tkRedemption t -> unchanged (Left (AlreadyRedeemed r))
            | otherwise ->
                case [s | s <- slots, slotIdOf s == slotId] of
                  [] -> unchanged (Left SlotUnknown)
                  (s : _) ->
                    let taken = takenFor ts s
                        free = slotCap s - taken
                        need
                          | tkSeats t == "all" = slotCap s
                          | otherwise = seatCount (tkSeats t)
                        opensSession = taken == 0
                        weekAtCap =
                          maxWeekly > 0
                            && weekSessions ts slots (mondayOf (slotDate s)) >= maxWeekly
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
                               in (BookingData (replaceTicket t' ts) emails', Right t')

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
  ts <- listTickets store
  let go = do
        bytes <- getRandomBytes 8 :: IO BS.ByteString
        let chars = map pick (BS.unpack bytes)
            code = T.pack (take 4 chars) <> "-" <> T.pack (take 4 (drop 4 chars))
        if any ((== code) . tkCode) ts then go else pure code
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
modifyStore store f = modifyData store $ \(BookingData ts emails) ->
  let (ts', result) = f ts in (BookingData ts' emails, result)

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
bookingDataToJSON (BookingData ts emails) = JObj [("tickets", JArr (map ticketToJSON ts)), ("reservationEmails", emailsToJSON emails)]

bookingDataFromJSON :: JSON -> Either String BookingData
bookingDataFromJSON v = BookingData <$> ticketsFromJSON v <*> case v .:? "reservationEmails" of
  Nothing -> Right emptyReservationEmails
  Just settings -> emailsFromJSON settings

readReservationEmails :: BookingStore -> IO ReservationEmails
readReservationEmails store = do BookingData _ emails <- readMVar (bsVar store); pure emails

modifyEmails :: BookingStore -> (ReservationEmails -> (ReservationEmails, a)) -> IO a
modifyEmails store f = do
  result <- modifyData store $ \(BookingData ts emails) -> let (next, value) = f emails in (BookingData ts next, Right value)
  either (const (fail "Unexpected reservation email store rejection")) pure result

configureReservationEmails :: BookingStore -> Bool -> [Mailbox] -> IO ()
configureReservationEmails store enabled recipients = modifyEmails store $ \s -> (s {emailsEnabled = enabled, emailsRecipients = recipients}, ())

bookingDelivery :: Text -> Ticket -> Redemption -> SlotSpec -> Mailbox -> Delivery
bookingDelivery now ticket red slot recipient =
  let d = reservationDelivery False (tkCode ticket <> ":" <> mailboxText recipient) now (tkCode ticket) (tkProduct ticket) (tkSeats ticket) slot recipient
   in d {deliveryBody = deliveryBody d <> "Preferred name: " <> T.take 200 (T.unwords (T.words (redName red))) <> "\n"}

-- Explicit admin recovery for a booking made while notifications were off.
-- Existing jobs, including failures/uncertain sends, are never duplicated.
queueMissingReservationEmails :: BookingStore -> [SlotSpec] -> Text -> IO (Either Text Int)
queueMissingReservationEmails store slots code = do
  now <- iso8601Now
  modifyData store $ \old@(BookingData tickets emails) ->
    let reject why = (old, Left why)
     in if not (emailsEnabled emails) || null (emailsRecipients emails)
          then reject "Enable reservation notifications and save recipients first. No email was queued."
          else case [t | t <- tickets, tkCode t == code] of
            [t] | Just red <- tkRedemption t -> case [s | s <- slots, slotIdOf s == redSlotId red] of
              s : _ ->
                let existing = map deliveryId (emailsDeliveries emails)
                    missing = [d | recipient <- emailsRecipients emails, let d = bookingDelivery now t red s recipient, deliveryId d `notElem` existing]
                 in (BookingData tickets emails {emailsDeliveries = emailsDeliveries emails <> missing}, Right (length missing))
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
