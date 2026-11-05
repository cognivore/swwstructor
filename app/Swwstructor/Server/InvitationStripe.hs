{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Private prices are resolved by configured identifier, never by public
-- catalogue membership or anything supplied by the visitor.
module Swwstructor.Server.InvitationStripe
  ( InvitationPrice (..), fetchInvitationPrice, validateInvitationPrice ) where

import Control.Exception (try)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
  ( Manager, HttpException, parseRequest, withResponse, brReadSome
  , requestHeaders, responseBody, responseStatus, responseHeaders
  , responseTimeout, responseTimeoutMicro )
import Network.HTTP.Types.Status (statusCode)
import Swwstructor.Server.Invitation (InvitationTier (..))
import Swwstructor.Server.SecretStore (SecretValue, secretText)
import StickyWM (JSON (..), decode, (.:?))

data InvitationPrice = InvitationPrice
  { ipId :: !Text
  , ipProduct :: !Text
  } deriving (Eq, Show)

fetchInvitationPrice :: Manager -> IO (Maybe SecretValue) -> InvitationTier -> Maybe Text -> IO (Either Text InvitationPrice)
fetchInvitationPrice manager loadKey tier configured = case configured of
  Nothing -> pure (Left "Invitation prices have not been configured.")
  Just ident | not (validPriceId ident) -> pure (Left "The configured invitation price identifier is invalid.")
  Just ident -> do
    key <- loadKey
    case key of
      Nothing -> pure (Left "Stripe is not configured.")
      Just sk -> do
        base <- parseRequest ("https://api.stripe.com/v1/prices/" <> T.unpack ident <> "?expand%5B%5D=product")
        let req = base
              { requestHeaders = [("Authorization", TE.encodeUtf8 ("Bearer " <> secretText sk))]
              , responseTimeout = responseTimeoutMicro (20 * 1000000)
              }
        result <- try $ withResponse req manager $ \response -> do
          bytes <- brReadSome (responseBody response) 65537
          let code = statusCode (responseStatus response)
              requestId = maybe "unavailable" (T.take 100 . T.filter (\c -> c >= ' ' && c <= '~') . TE.decodeUtf8With (\_ _ -> Just '?')) (lookup "Request-Id" (responseHeaders response))
              context = "Stripe GET invitation price; request " <> requestId
          pure $ if BL.length bytes > 65536
            then Left (context <> ": response exceeds the 64 KiB limit.")
            else if code < 200 || code >= 300
              then Left (context <> ": HTTP " <> T.pack (show code) <> "; response body withheld.")
              else case TE.decodeUtf8' (BL.toStrict bytes) of
                Left _ -> Left (context <> ": response is not UTF-8.")
                Right body -> case decode (T.unpack body) of
                  Left _ -> Left (context <> ": response is invalid.")
                  Right obj -> validateInvitationPrice tier ident obj
        pure $ case result of
          Left (_ :: HttpException) -> Left "Stripe invitation price could not be checked; request and native details withheld."
          Right value -> value

validPriceId :: Text -> Bool
validPriceId ident = "price_" `T.isPrefixOf` ident && T.length ident <= 255 &&
  T.all (\c -> c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '_') ident

-- This is deliberately stricter than the public catalogue. Metadata is not
-- authority on its own: the exact configured id, amount and currency agree.
validateInvitationPrice :: InvitationTier -> Text -> JSON -> Either Text InvitationPrice
validateInvitationPrice tier ident obj = case obj .:? "product" of
  Just productObj
    | str obj "id" == Just ident
    , obj .:? "active" == Just (JBool True)
    , str obj "type" == Just "one_time"
    , str obj "billing_scheme" == Just "per_unit"
    , str obj "currency" == Just "gbp"
    , obj .:? "unit_amount" == Just (JNum amount)
    , productObj .:? "active" == Just (JBool True)
    , meta productObj "sww_visibility" == Just "private"
    , meta productObj "sww_purpose" == Just "invitation"
    , meta productObj "sww_seats" == Just "1"
    , meta obj "sww_tier" == Just tierName
    , meta obj "sww_fee_minor" == Just fee
    , Just name <- str productObj "name"
    , not (T.null (T.strip name)) -> Right (InvitationPrice ident name)
  _ -> Left "The invitation price no longer matches its agreed offer. No checkout was started."
  where
    (amount, tierName, fee) = case tier of
      Friend -> (6500, "friend", "0")
      Partner -> (7500, "partner", "500")
    str value field = case value .:? field of Just (JStr txt) -> Just (T.pack txt); _ -> Nothing
    meta value field = value .:? "metadata" >>= (\md -> str md field)
