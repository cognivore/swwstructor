{-# LANGUAGE OverloadedStrings #-}

-- | The booking store: tickets issued by Stripe checkout and their
-- redemptions (a reserved class slot). Persistence mirrors
-- "Swwstructor.Server.Store" — one JSON file under the data dir, held in an
-- 'MVar', written atomically on every change, surviving restarts.
--
-- A 'Ticket' is issued (idempotently, keyed by the Stripe Checkout session id)
-- when a payment lands — by the webhook, by the success page, whichever runs
-- first. Cashing it in ('redeemTicket') attaches a 'Redemption': the slot, the
-- student's name, and — filled in as the side effects complete — the Google
-- Calendar event id and the ledger of notifications already sent (what makes
-- the reminder loop idempotent, NLCC-style).
module Swwstructor.Server.Booking
  ( Ticket (..)
  , Redemption (..)
  , RedeemError (..)
  , BookingStore
  , newBookingStore
  , listTickets
  , issueTicket
  , redeemTicket
  , setEventId
  , claimNote
  , takenCounts
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Exception (SomeException, try)
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Swwstructor.Content (SlotSpec, slotCap, slotIdOf)
import StickyWM (JSON (JArr, JNull, JNum, JObj, JStr), decode, encode, jstr, (.:), (.:?))
import System.Directory (doesFileExist, renameFile)
import System.IO (hPutStrLn, stderr)

-- | A reserved slot on a ticket.
data Redemption = Redemption
  { redSlotId :: !Text
  , redName :: !Text
  -- ^ the student's name, as they entered it
  , redCreatedAt :: !Text
  -- ^ ISO-8601 UTC
  , redEventId :: !(Maybe Text)
  -- ^ the Google Calendar event id, once created (retried by the reminder
  -- loop while 'Nothing')
  }
  deriving (Eq, Show)

-- | One paid class ticket.
data Ticket = Ticket
  { tkCode :: !Text
  -- ^ the human code the student types to redeem (unique — the ticket's key)
  , tkEmail :: !Text
  -- ^ buyer email from checkout, lower-cased
  , tkProduct :: !Text
  -- ^ display name of what was bought
  , tkSessionId :: !Text
  -- ^ Stripe Checkout session id (idempotency key for issuance)
  , tkCreatedAt :: !Text
  -- ^ ISO-8601 UTC
  , tkRedemption :: !(Maybe Redemption)
  , tkNotesSent :: ![Text]
  -- ^ notification ledger (@\"ticket\"@, @\"confirm\"@, @\"t7\"@, @\"day0\"@) —
  -- claimed BEFORE sending, so a once-a-minute loop can never double-send
  }
  deriving (Eq, Show)

-- | Why a redemption was refused (each gets its own user-facing message).
data RedeemError
  = CodeNotFound
  | EmailMismatch
  | AlreadyRedeemed !Redemption
  | SlotUnknown
  | SlotFull
  deriving (Eq, Show)

data BookingStore = BookingStore
  { bsVar :: !(MVar [Ticket])
  , bsFile :: !FilePath
  }

-- | Open the store; an absent file means no tickets yet. A present-but-corrupt
-- file fails loud and starts empty (never silently drops paid tickets on a
-- write, though — the first change would persist the empty list, so we warn).
newBookingStore :: FilePath -> IO BookingStore
newBookingStore path = do
  exists <- doesFileExist path
  tickets <-
    if not exists
      then pure []
      else do
        r <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
        case r of
          Left e -> do
            hPutStrLn stderr ("[bookings] WARNING: cannot read " <> path <> ": " <> show e <> " — starting with no tickets.")
            pure []
          Right bs -> case decode (T.unpack (TE.decodeUtf8 bs)) >>= ticketsFromJSON of
            Right ts -> pure ts
            Left err -> do
              hPutStrLn stderr ("[bookings] WARNING: " <> path <> " did not parse (" <> err <> ") — starting with no tickets. Inspect the file before the next booking overwrites it.")
              pure []
  var <- newMVar tickets
  pure (BookingStore var path)

listTickets :: BookingStore -> IO [Ticket]
listTickets = readMVar . bsVar

-- | Reservations per slot id, across all redeemed tickets.
takenCounts :: [Ticket] -> Map Text Int
takenCounts ts =
  M.fromListWith (+) [(redSlotId r, 1) | t <- ts, Just r <- [tkRedemption t]]

-- | Issue a ticket for a paid checkout session. Idempotent on the session id:
-- the webhook and the success page can both call this and exactly one ticket
-- exists afterwards. Returns the ticket and whether this call created it.
issueTicket :: BookingStore -> Text -> Text -> Text -> IO (Either Text (Ticket, Bool))
issueTicket store sessionId email product' = do
  now <- iso8601Now
  code <- freshCode store
  modifyStore store $ \ts ->
    case [t | t <- ts, tkSessionId t == sessionId] of
      (t : _) -> (ts, Right (t, False))
      [] ->
        let t =
              Ticket
                { tkCode = code
                , tkEmail = T.toLower (T.strip email)
                , tkProduct = product'
                , tkSessionId = sessionId
                , tkCreatedAt = now
                , tkRedemption = Nothing
                , tkNotesSent = []
                }
         in (ts <> [t], Right (t, True))

-- | Cash a ticket in for a slot. All checks and the write happen in one
-- atomic step, so two students racing for the last place cannot both win.
redeemTicket :: BookingStore -> [SlotSpec] -> Text -> Text -> Text -> Text -> IO (Either RedeemError Ticket)
redeemTicket store slots code email name slotId = do
  now <- iso8601Now
  modifyStore store $ \ts ->
    let norm = T.toUpper (T.filter (/= '-') (T.strip code))
        matches t = T.toUpper (T.filter (/= '-') (tkCode t)) == norm
     in case [t | t <- ts, matches t] of
          [] -> (ts, Left CodeNotFound)
          (t : _)
            | T.toLower (T.strip email) /= tkEmail t -> (ts, Left EmailMismatch)
            | Just r <- tkRedemption t -> (ts, Left (AlreadyRedeemed r))
            | otherwise ->
                case [s | s <- slots, slotIdOf s == slotId] of
                  [] -> (ts, Left SlotUnknown)
                  (s : _)
                    | M.findWithDefault 0 slotId (takenCounts ts) >= slotCap s -> (ts, Left SlotFull)
                    | otherwise ->
                        let red = Redemption slotId (T.strip name) now Nothing
                            t' = t {tkRedemption = Just red}
                         in (replaceTicket t' ts, Right t')

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

-- | A fresh 8-character code (Crockford-ish alphabet, no confusable glyphs),
-- shown as @XXXX-XXXX@. ~39 bits — unguessable at booking-form rates; still
-- checked for collision against existing tickets.
freshCode :: BookingStore -> IO Text
freshCode store = do
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
modifyStore store f =
  modifyMVar (bsVar store) $ \old -> do
    let (new, result) = f old
    case result of
      Right _ | new /= old -> do
        r <- saveTickets (bsFile store) new
        case r of
          Right () -> pure ()
          Left e -> hPutStrLn stderr ("[bookings] WARNING: could not persist " <> bsFile store <> ": " <> T.unpack e)
      _ -> pure ()
    pure (new, result)

saveTickets :: FilePath -> [Ticket] -> IO (Either Text ())
saveTickets path ts = do
  let bytes = TE.encodeUtf8 (T.pack (encode (ticketsToJSON ts)))
      tmp = path <> ".tmp"
  r <- try (BS.writeFile tmp bytes >> renameFile tmp path) :: IO (Either SomeException ())
  pure (either (Left . T.pack . show) Right r)

-- ---------------------------------------------------------------------------
-- JSON codec
-- ---------------------------------------------------------------------------

ticketsToJSON :: [Ticket] -> JSON
ticketsToJSON ts = JObj [("tickets", JArr (map ticketToJSON ts))]

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
    , ("email", JStr (T.unpack (tkEmail t)))
    , ("product", JStr (T.unpack (tkProduct t)))
    , ("sessionId", JStr (T.unpack (tkSessionId t)))
    , ("createdAt", JStr (T.unpack (tkCreatedAt t)))
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
  email <- txt v "email"
  product' <- txt v "product"
  sess <- txt v "sessionId"
  created <- txt v "createdAt"
  let notes = case v .:? "notesSent" of
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
      , tkEmail = email
      , tkProduct = product'
      , tkSessionId = sess
      , tkCreatedAt = created
      , tkRedemption = red
      , tkNotesSent = notes
      }
  where
    txt o k = T.pack <$> (jstr =<< o .: k)
