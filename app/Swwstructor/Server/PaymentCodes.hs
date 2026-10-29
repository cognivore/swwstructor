{-# LANGUAGE OverloadedStrings #-}

-- | Paid references with no lesson entitlement. These deliberately live in a
-- separate file and use separate Stripe metadata from lesson tickets: even an
-- older server cannot turn them into a reservation after a rollback.
module Swwstructor.Server.PaymentCodes
  ( PaymentCode (..)
  , PaymentCodeStore
  , newPaymentCodeStore
  , newPaymentCode
  , isPaymentCode
  , lookupPaymentCode
  , issuePaymentCode
  , selectPaymentPreview
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Exception (IOException, try)
import Crypto.Random (getRandomBytes)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import StickyWM (JSON (JArr, JObj, JStr), decode, encode, jstr, (.:), (.:?))
import System.Directory (doesFileExist, renameFile)

data PaymentCode = PaymentCode
  { pcCode :: !Text
  , pcProduct :: !Text
  , pcPaymentRef :: !Text
  , pcCreatedAt :: !Text
  , pcPreviewSlot :: !(Maybe Text)
  -- ^ A calendar preview, never a redemption or a claim on capacity.
  }
  deriving (Eq, Show)

data PaymentCodeStore = PaymentCodeStore !(MVar [PaymentCode]) !FilePath

newPaymentCodeStore :: FilePath -> IO PaymentCodeStore
newPaymentCodeStore path = do
  exists <- doesFileExist path
  records <-
    if not exists
      then pure []
      else do
        bytes <- BS.readFile path
        case decode (T.unpack (TE.decodeUtf8 bytes)) >>= recordsFromJSON of
          Right xs -> pure xs
          Left err -> ioError (userError ("Cannot open payment-code records at " <> path <> ": " <> err <> "; existing records have not been replaced."))
  PaymentCodeStore <$> newMVar records <*> pure path

alphabet :: String
alphabet = "23456789ABCDEFGHJKMNPQRSTVWXYZ"

isPaymentCode :: Text -> Bool
isPaymentCode code = case T.stripPrefix "PAY-" code of
  Just c -> T.length c == 9 && T.index c 4 == '-' && T.all (`elem` alphabet) (T.take 4 c <> T.drop 5 c)
  Nothing -> False

normalise :: Text -> Text
normalise = T.toUpper . T.filter (/= '-') . T.strip

lookupPaymentCode :: PaymentCodeStore -> Text -> IO (Maybe PaymentCode)
lookupPaymentCode (PaymentCodeStore var _) code = do
  records <- readMVar var
  pure $ case [p | p <- records, normalise (pcCode p) == normalise code] of
    p : _ -> Just p
    [] -> Nothing

newPaymentCode :: PaymentCodeStore -> IO Text
newPaymentCode store = do
  bytes <- getRandomBytes 8 :: IO BS.ByteString
  let chars = T.pack [alphabet !! (fromIntegral b `mod` length alphabet) | b <- BS.unpack bytes]
      code = "PAY-" <> T.take 4 chars <> "-" <> T.drop 4 chars
  found <- lookupPaymentCode store code
  maybe (pure code) (const (newPaymentCode store)) found

issuePaymentCode :: PaymentCodeStore -> Text -> Text -> Text -> IO (Either Text (PaymentCode, Bool))
issuePaymentCode store paymentRef code productName
  | not (isPaymentCode code) = pure (Left "Payment-only reference has an invalid code; no record was written.")
  | otherwise = do
      now <- T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" <$> getCurrentTime
      modifyRecords store $ \records ->
        case ([p | p <- records, pcPaymentRef p == paymentRef], [p | p <- records, pcCode p == code]) of
          (p : _, _) -> Right (records, (p, False))
          ([], _ : _) -> Left "Payment-only code already belongs to a different payment; no record was changed."
          ([], []) -> let p = PaymentCode code productName paymentRef now Nothing in Right (records <> [p], (p, True))

selectPaymentPreview :: PaymentCodeStore -> Text -> Text -> IO (Either Text PaymentCode)
selectPaymentPreview store code slotId = modifyRecords store $ \records ->
  case [p | p <- records, normalise (pcCode p) == normalise code] of
    [] -> Left "Payment-only code was not found; no calendar preview was saved."
    p : _ ->
      let updated = p {pcPreviewSlot = Just slotId}
       in Right ([if pcCode x == pcCode p then updated else x | x <- records], updated)

-- Publish memory only after the atomic file replacement succeeds. A failed
-- webhook write is retryable; it must not become a false acknowledgement.
modifyRecords :: PaymentCodeStore -> ([PaymentCode] -> Either Text ([PaymentCode], a)) -> IO (Either Text a)
modifyRecords (PaymentCodeStore var path) f = modifyMVar var $ \old -> case f old of
  Left err -> pure (old, Left err)
  Right (new, result) | new == old -> pure (old, Right result)
  Right (new, result) -> do
    written <- try (BS.writeFile (path <> ".tmp") (TE.encodeUtf8 (T.pack (encode (recordsToJSON new)))) >> renameFile (path <> ".tmp") path) :: IO (Either IOException ())
    pure $ case written of
      Left err -> (old, Left ("Could not save payment-code records at " <> T.pack path <> ": " <> T.pack (show err) <> "; retry after storage recovers."))
      Right () -> (new, Right result)

recordsToJSON :: [PaymentCode] -> JSON
recordsToJSON ps = JObj [("paymentCodes", JArr (map one ps))]
  where
    one p =
      JObj $
        [("code", JStr (T.unpack (pcCode p))), ("product", JStr (T.unpack (pcProduct p))), ("paymentRef", JStr (T.unpack (pcPaymentRef p))), ("createdAt", JStr (T.unpack (pcCreatedAt p)))]
          <> [("previewSlot", JStr (T.unpack s)) | Just s <- [pcPreviewSlot p]]

recordsFromJSON :: JSON -> Either String [PaymentCode]
recordsFromJSON value = do
  arr <- value .: "paymentCodes"
  case arr of
    JArr ps -> mapM one ps
    _ -> Left "paymentCodes must be an array"
  where
    txt value' key = T.pack <$> (jstr =<< value' .: key)
    one p = do
      code <- txt p "code"
      if not (isPaymentCode code)
        then Left "payment-only code has an invalid format"
        else do
          productName <- txt p "product"
          ref <- txt p "paymentRef"
          created <- txt p "createdAt"
          preview <- case p .:? "previewSlot" of
            Nothing -> Right Nothing
            Just (JStr s) -> Right (Just (T.pack s))
            _ -> Left "previewSlot must be a string when present"
          pure (PaymentCode code productName ref created preview)
