{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The secret store: the owner enters their Stripe keys through the admin
-- WebUI; this module encrypts them at rest with AES-256-GCM under a per-instance
-- master key (delivered to the service at boot, e.g. age-decrypted into
-- @SWW_MASTER_KEY@). Plaintext keys never touch disk, the Nix store, logs, git,
-- or the JS bundle. 'SecretValue' has a redacting 'Show' so a key can never be
-- printed by accident.
--
-- On-disk format: base64 of @nonce(12) ++ tag(16) ++ ciphertext@, where the
-- plaintext is the JSON encoding of a 'SecretBundle'. The master key is a
-- 32-byte value supplied as hex.
module Swwstructor.Server.SecretStore
  ( SecretValue (..)
  , secretText
  , MasterKey
  , parseMasterKeyHex
  , genMasterKey
  , genAdminPassword
  , genSessionSeed
  , SecretBundle (..)
  , emptyBundle
  , bundleHasCheckout
  , loadBundle
  , saveBundle
  , hmacTokenHex
  ) where

import Control.Exception (SomeException, try)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types
  ( AEADMode (AEAD_GCM)
  , AuthTag (AuthTag)
  , aeadDecrypt
  , aeadEncrypt
  , aeadFinalize
  , aeadInit
  , cipherInit
  )
import Crypto.Error (eitherCryptoError)
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC (hmacGetDigest), hmac)
import Crypto.Random (getRandomBytes)
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (Base (Base16, Base64), convertFromBase, convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import StickyWM (JSON (JNull, JObj, JStr), decode, encode, jstr, (.:?))
import System.Directory (doesFileExist, renameFile)
import System.IO (hPutStrLn, stderr)

-- | A resolved secret. The 'Show' instance redacts, so it is safe to log a
-- record containing one.
newtype SecretValue = SecretValue Text

instance Show SecretValue where
  show _ = "<redacted>"

secretText :: SecretValue -> Text
secretText (SecretValue t) = t

-- | The 32-byte AES-256 master key.
newtype MasterKey = MasterKey ByteString

instance Show MasterKey where
  show _ = "<master-key>"

-- | Parse a 64-hex-character master key. Returns 'Left' with a reason on a bad
-- length or non-hex input.
parseMasterKeyHex :: Text -> Either Text MasterKey
parseMasterKeyHex t =
  case convertFromBase Base16 (TE.encodeUtf8 (T.strip t)) of
    Left e -> Left ("master key is not valid hex: " <> T.pack e)
    Right (bs :: ByteString)
      | BS.length bs == 32 -> Right (MasterKey bs)
      | otherwise -> Left ("master key must be 32 bytes (64 hex chars), got " <> T.pack (show (BS.length bs)))

-- | Generate a fresh random 32-byte master key (for dev when @SWW_MASTER_KEY@
-- is unset — secrets entered then won't survive a restart).
genMasterKey :: IO MasterKey
genMasterKey = MasterKey <$> (getRandomBytes 32 :: IO ByteString)

-- | Generate a random hex admin password (dev fallback when
-- @SWW_ADMIN_PASSWORD@ is unset).
genAdminPassword :: IO Text
genAdminPassword = do
  bs <- getRandomBytes 9 :: IO ByteString
  pure (TE.decodeUtf8 (convertToBase Base16 bs))

-- | A random integer seed for the admin session epoch, so session tokens differ
-- across process restarts (logout bumps the epoch to revoke within a run).
genSessionSeed :: IO Integer
genSessionSeed = do
  bs <- getRandomBytes 8 :: IO ByteString
  pure (BS.foldl' (\a w -> a * 256 + fromIntegral w) 0 bs)

-- | The owner-entered Stripe secrets. 'SecretValue' fields keep them un-loggable.
data SecretBundle = SecretBundle
  { sbStripePk :: !(Maybe SecretValue)
  -- ^ publishable key (browser-safe)
  , sbStripeSk :: !(Maybe SecretValue)
  -- ^ secret key (backend only)
  , sbStripeWebhook :: !(Maybe SecretValue)
  -- ^ webhook signing secret (backend only)
  }
  deriving (Show)

emptyBundle :: SecretBundle
emptyBundle = SecretBundle Nothing Nothing Nothing

-- | Whether the bundle has enough to run checkout (a secret key).
bundleHasCheckout :: SecretBundle -> Bool
bundleHasCheckout b = case sbStripeSk b of
  Just _ -> True
  Nothing -> False

bundleToJSON :: SecretBundle -> JSON
bundleToJSON b =
  JObj
    [ ("stripePk", enc (sbStripePk b))
    , ("stripeSk", enc (sbStripeSk b))
    , ("stripeWebhook", enc (sbStripeWebhook b))
    ]
  where
    enc Nothing = JNull
    enc (Just (SecretValue t)) = JStr (T.unpack t)

bundleFromJSON :: JSON -> SecretBundle
bundleFromJSON v =
  SecretBundle (field "stripePk") (field "stripeSk") (field "stripeWebhook")
  where
    field k = case v .:? k of
      Just j -> case jstr j of
        Right s | not (null s) -> Just (SecretValue (T.pack s))
        _ -> Nothing
      Nothing -> Nothing

-- ---------------------------------------------------------------------------
-- Encryption
-- ---------------------------------------------------------------------------

nonceLen :: Int
nonceLen = 12

tagLen :: Int
tagLen = 16

-- | Encrypt a bundle, returning the base64 blob to persist.
encryptBundle :: MasterKey -> SecretBundle -> IO (Either Text ByteString)
encryptBundle (MasterKey key) bundle = do
  nonce <- getRandomBytes nonceLen :: IO ByteString
  let plaintext = TE.encodeUtf8 (T.pack (encode (bundleToJSON bundle)))
  pure $ do
    cipher <- mapErr (eitherCryptoError (cipherInit key)) :: Either Text AES256
    aead <- mapErr (eitherCryptoError (aeadInit AEAD_GCM cipher nonce))
    let (ct, aeadFinal) = aeadEncrypt aead plaintext
        AuthTag tag = aeadFinalize aeadFinal tagLen
        blob = nonce <> BA.convert tag <> ct
    Right (convertToBase Base64 blob)
  where
    mapErr = either (Left . T.pack . show) Right

-- | Decrypt a base64 blob back to a bundle.
decryptBundle :: MasterKey -> ByteString -> Either Text SecretBundle
decryptBundle (MasterKey key) b64 = do
  blob <- either (Left . T.pack) Right (convertFromBase Base64 b64) :: Either Text ByteString
  let (nonce, rest) = BS.splitAt nonceLen blob
      (tag, ct) = BS.splitAt tagLen rest
  if BS.length nonce /= nonceLen || BS.length tag /= tagLen
    then Left "secret store: truncated ciphertext"
    else do
      cipher <- mapErr (eitherCryptoError (cipherInit key)) :: Either Text AES256
      aead <- mapErr (eitherCryptoError (aeadInit AEAD_GCM cipher nonce))
      let (pt, aeadFinal) = aeadDecrypt aead ct
          AuthTag tag' = aeadFinalize aeadFinal tagLen
      if BA.constEq tag (BA.convert tag' :: ByteString)
        then case decode (T.unpack (TE.decodeUtf8 pt)) of
          Right j -> Right (bundleFromJSON j)
          Left e -> Left ("secret store: bad plaintext json: " <> T.pack e)
        else Left "secret store: authentication failed (wrong key or tampered file)"
  where
    mapErr = either (Left . T.pack . show) Right

-- ---------------------------------------------------------------------------
-- Persistence
-- ---------------------------------------------------------------------------

-- | Load the bundle from disk; an absent file means "no secrets entered yet".
loadBundle :: FilePath -> MasterKey -> IO SecretBundle
loadBundle path key = do
  exists <- doesFileExist path
  if not exists
    then pure emptyBundle
    else do
      r <- try (BS.readFile path) :: IO (Either SomeException ByteString)
      case r of
        Left e -> do
          hPutStrLn stderr ("[secret-store] WARNING: cannot read " <> path <> ": " <> show e <> " — running with no secrets.")
          pure emptyBundle
        Right b64 -> case decryptBundle key (BS.filter (/= 0x0a) b64) of
          Right b -> pure b
          Left err -> do
            -- Fail LOUD: a present-but-undecryptable store means a wrong
            -- SWW_MASTER_KEY or a tampered file. Don't silently look empty.
            hPutStrLn stderr ("[secret-store] WARNING: " <> path <> " exists but did not decrypt (" <> T.unpack err <> "). Running as if no secrets are set; check SWW_MASTER_KEY (re-saving will overwrite this file).")
            pure emptyBundle

-- | Save the bundle, written atomically (temp file + rename).
saveBundle :: FilePath -> MasterKey -> SecretBundle -> IO (Either Text ())
saveBundle path key bundle = do
  eblob <- encryptBundle key bundle
  case eblob of
    Left e -> pure (Left e)
    Right blob -> do
      r <- try (BS.writeFile tmp blob >> renameFile tmp path) :: IO (Either SomeException ())
      pure (either (Left . T.pack . show) Right r)
  where
    tmp = path <> ".tmp"

-- | A hex HMAC-SHA256 of a message under the master key — used for the
-- stateless admin session token (tied to the key, so no server-side session
-- store is needed).
hmacTokenHex :: MasterKey -> ByteString -> Text
hmacTokenHex (MasterKey key) msg =
  let h = hmac key msg :: HMAC SHA256
      digest = BA.convert (hmacGetDigest h) :: ByteString
   in TE.decodeUtf8 (convertToBase Base16 digest)
