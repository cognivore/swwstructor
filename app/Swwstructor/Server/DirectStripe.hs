{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The small Stripe capability needed by dated bookings. Only checkout
-- identity, our own reference, payment and expiry facts cross this boundary;
-- customer/card data and response bodies are never retained or logged.
module Swwstructor.Server.DirectStripe
  ( DirectStripe (..), DirectRequest (..), DirectSession (..), SessionState (..)
  , DirectPayment (..), liveDirectStripe, directPaymentOf, directSessionOf
  , isDatedPayment, directFormParams
  ) where

import Control.Exception (try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
  ( Manager, Request (..), RequestBody (RequestBodyBS), HttpException (..)
  , HttpExceptionContent (..), parseRequest, withResponse, brReadSome
  , responseStatus, responseBody, responseHeaders, responseTimeoutMicro
  )
import Network.HTTP.Types.Status (statusCode)
import Swwstructor.Checkout (CheckoutRequest, CheckoutError (..), Url (..), stripeFormParams, encodeForm)
import Swwstructor.Money (Price (..), Cents (..), currencyOfCode)
import Swwstructor.Server.SecretStore (SecretValue, secretText)
import Swwstructor.Server.Booking (isTicketCode)
import StickyWM (JSON (..), decode, (.:?))

data DirectRequest = DirectRequest
  { drCheckout :: !CheckoutRequest
  , drCode :: !Text
  , drExpiresAt :: !Integer
  , drSummary :: !Text
  }

data SessionState
  = SessionOpen !Url
  | SessionPaid !Text !Price
  | SessionUnpaidComplete
  | SessionExpired
  deriving (Eq, Show)

data DirectSession = DirectSession
  { dsId :: !Text
  , dsCode :: !Text
  , dsExpiresAt :: !Integer
  , dsState :: !SessionState
  } deriving (Eq, Show)

data DirectPayment = DirectPayment
  { dpId :: !Text, dpCode :: !Text, dpPrice :: !Price
  } deriving (Eq, Show)

-- Test interpreters substitute the remote effects, not booking decisions.
data DirectStripe = DirectStripe
  { directCreate :: DirectRequest -> IO (Either CheckoutError DirectSession)
  , directRetrieve :: Text -> IO (Either CheckoutError DirectSession)
  , directExpire :: Text -> IO (Either CheckoutError DirectSession)
  }

directFormParams :: DirectRequest -> [(Text, Text)]
directFormParams req = stripeFormParams (drCheckout req) <>
  [ ("expires_at", T.pack (show (drExpiresAt req)))
  , ("payment_method_types[0]", "card")
  , ("custom_text[submit][message]", drSummary req)
  ]

liveDirectStripe :: Manager -> IO (Maybe SecretValue) -> DirectStripe
liveDirectStripe manager loadKey = DirectStripe
  { directCreate = \req -> call "POST" "checkout/sessions"
      (Just ("dated-booking-" <> drCode req)) (directFormParams req)
  , directRetrieve = \sid -> withSession sid (call "GET" ("checkout/sessions/" <> sid) Nothing [])
  , directExpire = \sid -> withSession sid (call "POST" ("checkout/sessions/" <> sid <> "/expire") Nothing [])
  }
  where
    withSession sid action
      | validId "cs_" sid = action
      | otherwise = pure (Left (CheckoutBadResponse "Cannot retrieve checkout: the saved Stripe session identifier is invalid."))
    call verb path idempotency fields = do
      key <- loadKey
      case key of
        Nothing -> pure (Left (CheckoutTransport "Stripe is not configured; no checkout request was sent."))
        Just sk -> do
          base <- parseRequest ("https://api.stripe.com/v1/" <> T.unpack path)
          let req = base
                { method = verb
                , requestBody = RequestBodyBS (TE.encodeUtf8 (encodeForm fields))
                , requestHeaders = [("Authorization", TE.encodeUtf8 ("Bearer " <> secretText sk))]
                    <> [("Content-Type", "application/x-www-form-urlencoded") | verb == "POST"]
                    <> [("Idempotency-Key", TE.encodeUtf8 k) | Just k <- [idempotency]]
                , responseTimeout = responseTimeoutMicro (20 * 1000000)
                }
          result <- try $ withResponse req manager $ \response -> do
            bytes <- brReadSome (responseBody response) 262145
            let code = statusCode (responseStatus response)
                requestId = maybe "unavailable" (TE.decodeUtf8With (\_ _ -> Just '?')) (lookup "Request-Id" (responseHeaders response))
                context = "Stripe " <> TE.decodeUtf8 verb <> " " <> path <> "; request " <> T.take 100 requestId
            pure $
              if BL.length bytes > 262144
                then Left (CheckoutBadResponse (context <> ": response exceeds the 256 KiB limit; outcome requires reconciliation."))
                else if code < 200 || code >= 300
                  then if code == 400 && not (definitiveRejection bytes)
                    then Left (CheckoutBadResponse (context <> ": request identity or outcome is uncertain; preserve its hold and reconcile before retrying."))
                    else Left (CheckoutHttpStatus code (context <> ": request rejected; response body withheld to protect customer and credential data."))
                  else case TE.decodeUtf8' (BL.toStrict bytes) of
                    Left _ -> Left (CheckoutBadResponse (context <> ": response is not UTF-8; outcome requires reconciliation."))
                    Right txt -> case decode (T.unpack txt) >>= sessionFromJSON of
                      Left why -> Left (CheckoutBadResponse (context <> ": " <> T.pack why))
                      Right session -> Right session
          pure $ case result of
            Left (e :: HttpException) -> Left (CheckoutTransport ("Stripe " <> TE.decodeUtf8 verb <> " " <> path <> ": " <> transportReason e <> "; outcome is uncertain and must be reconciled using the same request identity."))
            Right response -> response

-- A mismatched idempotency request can return HTTP 400 even though an older
-- request DID create a session. It must never release a capacity hold.
definitiveRejection :: BL.ByteString -> Bool
definitiveRejection bytes = fromMaybe False $ do
  txt <- either (const Nothing) Just (TE.decodeUtf8' (BL.toStrict bytes))
  obj <- either (const Nothing) Just (decode (T.unpack txt))
  err <- obj .:? "error"
  pure (stringField err "type" == Just "invalid_request_error")

transportReason :: HttpException -> Text
transportReason (InvalidUrlException _ _) = "invalid request URL (value withheld)"
transportReason (HttpExceptionRequest _ e) = case e of
  ResponseTimeout -> "response timeout"
  ConnectionTimeout -> "connection timeout"
  ConnectionFailure _ -> "connection failure (native details withheld)"
  NoResponseDataReceived -> "connection closed without a response"
  ResponseBodyTooShort _ _ -> "incomplete response body"
  _ -> "HTTP transport failure (request and native details withheld)"

validId :: Text -> Text -> Bool
validId prefix v = prefix `T.isPrefixOf` v && T.length v <= 255 && T.all (\c -> c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '_') v

stringField :: JSON -> String -> Maybe Text
stringField obj key = case obj .:? key of Just (JStr t) -> Just (T.pack t); _ -> Nothing

integerField :: JSON -> String -> Maybe Integer
integerField obj key = case obj .:? key of
  Just (JNum n) | not (isNaN n || isInfinite n), n >= 0, n <= 9007199254740991, fromInteger (round n) == n -> Just (round n)
  _ -> Nothing

priceFields :: JSON -> String -> Maybe Price
priceFields obj amountKey = do
  amount <- integerField obj amountKey
  if amount > fromIntegral (maxBound :: Int) then Nothing else do
    cur <- stringField obj "currency" >>= currencyOfCode
    pure (Price (Cents (fromInteger amount)) cur)

metadata :: JSON -> JSON
metadata obj = fromMaybe (JObj []) (obj .:? "metadata")

sessionFromJSON :: JSON -> Either String DirectSession
sessionFromJSON obj = do
  sid <- required "Checkout session has no valid identifier." (stringField obj "id" >>= check (validId "cs_"))
  code <- required "Checkout session has no recognised dated-booking reference." (stringField (metadata obj) "sww_code" >>= check isTicketCode)
  if stringField (metadata obj) "sww_flow" /= Just "dated" then Left "Checkout session belongs to a different purchase flow." else pure ()
  expiry <- required "Checkout session has no valid expiry time." (integerField obj "expires_at")
  state <- case (stringField obj "status", stringField obj "payment_status") of
    (Just "complete", Just "paid") -> do
      payment <- required "Paid checkout has no payment identifier." (stringField obj "payment_intent" >>= check (validId "pi_"))
      amount <- required "Paid checkout has no supported total and currency." (priceFields obj "amount_total")
      pure (SessionPaid payment amount)
    (Just "open", Just "unpaid") -> do
      url <- required "Open checkout has no secure Stripe URL." (stringField obj "url" >>= check (T.isPrefixOf "https://checkout.stripe.com/"))
      pure (SessionOpen (Url url))
    (Just "expired", Just "unpaid") -> pure SessionExpired
    (Just "complete", Just "unpaid") -> pure SessionUnpaidComplete
    _ -> Left "Checkout state is not supported; keep its places held until payment can be reconciled."
  pure (DirectSession sid code expiry state)
  where
    required message = maybe (Left message) Right
    check predicate value = if predicate value then Just value else Nothing

eventObject :: BS.ByteString -> Maybe JSON
eventObject raw = do
  utf8 <- either (const Nothing) Just (TE.decodeUtf8' raw)
  value <- either (const Nothing) Just (decode (T.unpack utf8))
  payload <- value .:? "data"
  payload .:? "object"

isDatedPayment :: BS.ByteString -> Bool
isDatedPayment raw = (eventObject raw >>= (\obj -> stringField (metadata obj) "sww_flow")) == Just "dated"

directPaymentOf :: BS.ByteString -> Maybe DirectPayment
directPaymentOf raw = do
  obj <- eventObject raw
  if stringField obj "status" /= Just "succeeded" || stringField (metadata obj) "sww_flow" /= Just "dated" then Nothing else do
    payment <- stringField obj "id"
    code <- stringField (metadata obj) "sww_code"
    amount <- priceFields obj "amount_received"
    if validId "pi_" payment && isTicketCode code then Just (DirectPayment payment code amount) else Nothing

directSessionOf :: BS.ByteString -> Maybe DirectSession
directSessionOf raw = eventObject raw >>= either (const Nothing) Just . sessionFromJSON
