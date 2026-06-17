{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The live Stripe interpreter: the @http-client@-backed implementation of the
-- checkout effect, plus webhook signature verification. Kept out of the pure
-- library so the engine + templates stay dependency-light. Mirrors
-- @Okashi.Checkout@'s real interpreter.
module Swwstructor.Server.Stripe
  ( stripeCreate
  , verifyStripeSig
  , peekEventType
  ) where

import Control.Exception (SomeException, try)
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC (hmacGetDigest), hmac)
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
  ( Manager
  , Request
  , Response
  , RequestBody (RequestBodyBS)
  , httpLbs
  , method
  , parseRequest
  , requestBody
  , requestHeaders
  , responseBody
  , responseStatus
  )
import Network.HTTP.Types.Status (statusCode)
import Swwstructor.Checkout
  ( CheckoutError (CheckoutBadResponse, CheckoutHttpStatus, CheckoutTransport)
  , CheckoutRequest
  , CheckoutSession (CheckoutSession)
  , Url (Url)
  , encodeForm
  , stripeFormParams
  )
import Swwstructor.Server.SecretStore (SecretValue, secretText)
import StickyWM (decode, jstr, (.:))

-- | Create a hosted Checkout session. @POST /v1/checkout/sessions@,
-- form-encoded, bearer auth; parse @url@ from the JSON response.
stripeCreate :: Manager -> SecretValue -> CheckoutRequest -> IO (Either CheckoutError CheckoutSession)
stripeCreate mgr sk req = do
  let bearer = TE.encodeUtf8 ("Bearer " <> secretText sk)
      body = TE.encodeUtf8 (encodeForm (stripeFormParams req))
  ereq <- try (parseRequest "https://api.stripe.com/v1/checkout/sessions") :: IO (Either SomeException Request)
  case ereq of
    Left e -> pure (Left (CheckoutTransport (T.pack (show e))))
    Right base -> do
      let httpReq =
            base
              { method = "POST"
              , requestBody = RequestBodyBS body
              , requestHeaders =
                  [ ("Authorization", bearer)
                  , ("Content-Type", "application/x-www-form-urlencoded")
                  ]
              }
      res <- try (httpLbs httpReq mgr) :: IO (Either SomeException (Response BL.ByteString))
      pure $ case res of
        Left (e :: SomeException) -> Left (CheckoutTransport (T.pack (show e)))
        Right resp ->
          let code = statusCode (responseStatus resp)
              bodyStr = lbsToString (responseBody resp)
           in if code >= 200 && code < 300
                then parseSession bodyStr
                else Left (CheckoutHttpStatus code (T.pack (take 400 bodyStr)))

parseSession :: String -> Either CheckoutError CheckoutSession
parseSession bodyStr =
  case parsed of
    Right u -> Right (CheckoutSession (Url (T.pack u)))
    Left err -> Left (CheckoutBadResponse (T.pack err))
  where
    parsed = do
      v <- decode bodyStr
      u <- v .: "url"
      jstr u

lbsToString :: BL.ByteString -> String
lbsToString = T.unpack . TE.decodeUtf8 . BL.toStrict

-- ---------------------------------------------------------------------------
-- Webhook signature verification
-- ---------------------------------------------------------------------------

-- | Verify a @Stripe-Signature@ header against the raw request body and the
-- webhook signing secret. The header looks like @t=1610000000,v1=abc123…@; the
-- signed payload is @"{t}.{body}"@ and @v1@ is its hex HMAC-SHA256. Comparison
-- is constant-time. We accept if ANY @v1@ entry matches (Stripe may send
-- several during key rotation).
verifyStripeSig :: Text -> ByteString -> Text -> Bool
verifyStripeSig secret rawBody sigHeader =
  case lookup "t" parts of
    Nothing -> False
    Just t ->
      let signedPayload = TE.encodeUtf8 t <> "." <> rawBody
          expected = hmacHex (TE.encodeUtf8 secret) signedPayload
          v1s = [v | (k, v) <- parts, k == "v1"]
       in any (constEqText expected) v1s
  where
    parts =
      [ (T.strip k, T.strip (T.drop 1 rest))
      | kv <- T.splitOn "," sigHeader
      , let (k, rest) = T.breakOn "=" kv
      , not (T.null rest)
      ]

hmacHex :: ByteString -> ByteString -> Text
hmacHex key msg =
  let h = hmac key msg :: HMAC SHA256
      digest = BA.convert (hmacGetDigest h) :: ByteString
   in TE.decodeUtf8 (convertToBase Base16 digest)

constEqText :: Text -> Text -> Bool
constEqText a b = BA.constEq (TE.encodeUtf8 a) (TE.encodeUtf8 b)

-- | Best-effort extraction of a webhook event's @type@ (for logging / routing).
peekEventType :: ByteString -> Text
peekEventType raw =
  case decode (BC.unpack raw) of
    Right v -> case v .: "type" of
      Right tv -> either (const "unknown") T.pack (jstr tv)
      Left _ -> "unknown"
    Left _ -> "unknown"
