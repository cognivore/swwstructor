{-# LANGUAGE OverloadedStrings #-}

-- | The venue address book: the places classes/events can actually happen.
-- Owner-managed through the admin (name, street address, the area label it
-- belongs to, an optional map link, notes, active flag) and stored
-- server-side only — a venue's address never enters site content, so it
-- cannot leak onto a public page.
--
-- What a venue is CALLED is the deployment's business: the booking config
-- carries a \"venue noun\" (default @venue@; a pastry school says @kitchen@,
-- a draft club would say @hall@) that all student-facing copy uses.
--
-- The booking flow consumes the book through 'venueForArea': when a class
-- slot's area matches exactly ONE active venue, the week-before reminder can
-- tell the student the real address (and the calendar event is updated);
-- with zero or several matches the copy stays vague and the school announces
-- the venue by hand, as before.
module Swwstructor.Server.Venues
  ( Venue (..)
  , VenueStore
  , newVenueStore
  , listVenues
  , upsertVenue
  , deleteVenue
  , venueForArea
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Exception (SomeException, try)
import Crypto.Random (getRandomBytes)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import StickyWM (JSON (JArr, JBool, JObj, JStr), decode, encode, jstr, (.:), (.:?))
import System.Directory (doesFileExist, renameFile)
import System.IO (hPutStrLn, stderr)

data Venue = Venue
  { vnId :: !Text
  -- ^ stable random id (survives renames)
  , vnName :: !Text
  , vnAddress :: !Text
  , vnArea :: !Text
  -- ^ the area label this venue serves — matched against a slot's area
  , vnMapUrl :: !Text
  -- ^ optional map link ("" = none)
  , vnNotes :: !Text
  , vnActive :: !Bool
  -- ^ only active venues are considered for address announcements
  }
  deriving (Eq, Show)

data VenueStore = VenueStore
  { vsVar :: !(MVar [Venue])
  , vsFile :: !FilePath
  }

newVenueStore :: FilePath -> IO VenueStore
newVenueStore path = do
  exists <- doesFileExist path
  venues <-
    if not exists
      then pure []
      else do
        r <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
        case r of
          Left e -> do
            hPutStrLn stderr ("[venues] WARNING: cannot read " <> path <> ": " <> show e)
            pure []
          Right bs -> case decode (T.unpack (TE.decodeUtf8 bs)) >>= venuesFromJSON of
            Right vs -> pure vs
            Left err -> do
              hPutStrLn stderr ("[venues] WARNING: " <> path <> " did not parse (" <> err <> ") — starting empty.")
              pure []
  var <- newMVar venues
  pure (VenueStore var path)

listVenues :: VenueStore -> IO [Venue]
listVenues = readMVar . vsVar

-- | Insert or update. An empty id means \"new\" (one is minted); a known id
-- updates in place; an unknown non-empty id appends (idempotent restore).
upsertVenue :: VenueStore -> Venue -> IO ()
upsertVenue store v0 = do
  v <-
    if T.null (T.strip (vnId v0))
      then do
        bytes <- getRandomBytes 3 :: IO BS.ByteString
        pure v0 {vnId = TE.decodeUtf8 (convertToBase Base16 bytes)}
      else pure v0
  modifyStore store $ \vs ->
    if any ((== vnId v) . vnId) vs
      then [if vnId x == vnId v then v else x | x <- vs]
      else vs <> [v]

deleteVenue :: VenueStore -> Text -> IO ()
deleteVenue store vid = modifyStore store (filter ((/= vid) . vnId))

-- | The venue to announce for a slot area: exactly one active venue with
-- that precise area label — anything ambiguous stays a manual announcement.
venueForArea :: [Venue] -> Text -> Maybe Venue
venueForArea vs area = case [v | v <- vs, vnActive v, T.strip (vnArea v) == T.strip area] of
  [v] -> Just v
  _ -> Nothing

-- ---------------------------------------------------------------------------
-- Persistence + codec (the Booking-store pattern)
-- ---------------------------------------------------------------------------

modifyStore :: VenueStore -> ([Venue] -> [Venue]) -> IO ()
modifyStore store f =
  modifyMVar (vsVar store) $ \old -> do
    let new = f old
    if new /= old
      then do
        r <- saveVenues (vsFile store) new
        case r of
          Right () -> pure ()
          Left e -> hPutStrLn stderr ("[venues] WARNING: could not persist: " <> T.unpack e)
      else pure ()
    pure (new, ())

saveVenues :: FilePath -> [Venue] -> IO (Either Text ())
saveVenues path vs = do
  let bytes = TE.encodeUtf8 (T.pack (encode (venuesToJSON vs)))
      tmp = path <> ".tmp"
  r <- try (BS.writeFile tmp bytes >> renameFile tmp path) :: IO (Either SomeException ())
  pure (either (Left . T.pack . show) Right r)

venuesToJSON :: [Venue] -> JSON
venuesToJSON vs =
  JObj
    [ ( "venues"
      , JArr
          [ JObj
              [ ("id", JStr (T.unpack (vnId v)))
              , ("name", JStr (T.unpack (vnName v)))
              , ("address", JStr (T.unpack (vnAddress v)))
              , ("area", JStr (T.unpack (vnArea v)))
              , ("mapUrl", JStr (T.unpack (vnMapUrl v)))
              , ("notes", JStr (T.unpack (vnNotes v)))
              , ("active", JBool (vnActive v))
              ]
          | v <- vs
          ]
      )
    ]

venuesFromJSON :: JSON -> Either String [Venue]
venuesFromJSON v = do
  arr <- case v .: "venues" of
    Right (JArr xs) -> Right xs
    Right _ -> Left "venues: not an array"
    Left e -> Left e
  mapM one arr
  where
    one vv = do
      vid <- txt vv "id"
      name <- txt vv "name"
      addr <- txt vv "address"
      area <- txt vv "area"
      let mapU = opt vv "mapUrl"
          notes = opt vv "notes"
          active = case vv .:? "active" of
            Just (JBool b) -> b
            _ -> True
      pure (Venue vid name addr area mapU notes active)
    txt o k = T.pack <$> (jstr =<< o .: k)
    opt o k = case o .:? k of
      Just (JStr s) -> T.pack s
      _ -> ""
