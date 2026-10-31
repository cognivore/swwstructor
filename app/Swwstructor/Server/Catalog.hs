{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The live Stripe catalogue: fetch the owner's prices (with their products
-- and photos) and cache them briefly, so pages render the catalogue
-- with zero site-side maintenance and without hitting Stripe per request.
-- Failures degrade softly: serve the last good catalogue if there is one,
-- else none (the template renders its book-by-email fallback).
module Swwstructor.Server.Catalog
  ( CatalogCache
  , newCatalogCache
  , getCatalog
  , fetchCatalog
  ) where

import Control.Exception (try)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Network.HTTP.Client
  ( Manager
  , HttpException (..)
  , HttpExceptionContent (..)
  , withResponse
  , brReadSome
  , method
  , parseRequest
  , requestHeaders
  , responseBody
  , responseStatus
  , responseHeaders
  , responseTimeout
  , responseTimeoutMicro
  )
import Network.HTTP.Types.Status (statusCode)
import Swwstructor.Plugins.StripeCatalog (CatalogItem, parsePriceCatalogJSON)
import Swwstructor.Server.SecretStore (SecretValue, secretText)

-- | The last good catalogue and when it was fetched.
newtype CatalogCache = CatalogCache (IORef (Maybe (UTCTime, [CatalogItem])))

newCatalogCache :: IO CatalogCache
newCatalogCache = CatalogCache <$> newIORef Nothing

ttl :: NominalDiffTime
ttl = 300 -- seconds; the dashboard is the source of truth, five minutes is plenty

-- | The current catalogue. Fresh cache -> serve it; stale/empty -> re-fetch
-- (loading the secret key lazily, only when a refresh is actually due);
-- fetch failure -> last good value (or none). Never throws.
getCatalog :: CatalogCache -> Manager -> IO (Maybe SecretValue) -> IO [CatalogItem]
getCatalog (CatalogCache ref) mgr loadSk = do
  now <- getCurrentTime
  cached <- readIORef ref
  case cached of
    Just (at, items) | diffUTCTime now at < ttl -> pure items
    _ -> do
      msk <- loadSk
      case msk of
        Nothing -> pure (maybe [] snd cached)
        Just sk -> do
          fetched <- fetchCatalog mgr sk
          case fetched of
            Right items -> do
              writeIORef ref (Just (now, items))
              pure items
            Left err -> do
              putStrLn ("[catalog] fetch failed: " <> T.unpack err)
              pure (maybe [] snd cached)

fetchCatalog :: Manager -> SecretValue -> IO (Either Text [CatalogItem])
fetchCatalog mgr sk = do
  -- Inline prices created for dated checkouts are excluded by Stripe's price
  -- list. Fetching products would let those generated products crowd out the
  -- gift catalogue. A complete bounded response is required by the parser.
  result <- try $ do
    base <- parseRequest "https://api.stripe.com/v1/prices?active=true&type=one_time&limit=100&expand%5B%5D=data.product"
    let httpReq = base
          { method = "GET"
          , requestHeaders = [("Authorization", TE.encodeUtf8 ("Bearer " <> secretText sk))]
          , responseTimeout = responseTimeoutMicro (20 * 1000000)
          }
    withResponse httpReq mgr $ \response -> do
      bytes <- brReadSome (responseBody response) 1048577
      let code = statusCode (responseStatus response)
          requestId = maybe "unavailable" (T.take 100 . T.filter (\c -> c >= ' ' && c <= '~') . TE.decodeUtf8With (\_ _ -> Just '?')) (lookup "Request-Id" (responseHeaders response))
          context = "Stripe GET prices; request " <> requestId
      pure $
        if BL.length bytes > 1048576
          then Left (context <> ": catalogue exceeds the 1 MiB response limit.")
          else if code < 200 || code >= 300
            then Left (context <> ": HTTP " <> T.pack (show code) <> "; response body withheld.")
            else case TE.decodeUtf8' (BL.toStrict bytes) of
              Left _ -> Left (context <> ": catalogue response is not UTF-8.")
              Right txt -> case parsePriceCatalogJSON (T.unpack txt) of
                Left _ -> Left (context <> ": catalogue is incomplete or invalid; response content withheld.")
                Right items -> Right items
  pure $ case result of
    Left (err :: HttpException) -> Left ("Stripe GET prices: " <> transportReason err <> "; request and native details withheld.")
    Right value -> value

transportReason :: HttpException -> Text
transportReason (InvalidUrlException _ _) = "invalid request URL"
transportReason (HttpExceptionRequest _ reason) = case reason of
  ResponseTimeout -> "response timeout"
  ConnectionTimeout -> "connection timeout"
  ConnectionFailure _ -> "connection failure"
  _ -> "HTTP transport failure"
