{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The live Stripe catalogue: fetch the owner's products (with their default
-- prices and photos) and cache them briefly, so pages render the catalogue
-- with zero site-side maintenance and without hitting Stripe per request.
-- Failures degrade softly: serve the last good catalogue if there is one,
-- else none (the template renders its book-by-email fallback).
module Swwstructor.Server.Catalog
  ( CatalogCache
  , newCatalogCache
  , getCatalog
  ) where

import Control.Exception (SomeException, try)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Network.HTTP.Client
  ( Manager
  , Request
  , Response
  , httpLbs
  , method
  , parseRequest
  , requestHeaders
  , responseBody
  , responseStatus
  )
import Network.HTTP.Types.Status (statusCode)
import Swwstructor.Plugins.StripeCatalog (CatalogItem, parseCatalogJSON)
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
  -- NB. http-client refuses literal '[' ']' in a URL — percent-encode the
  -- expand[] parameter (Stripe accepts either form).
  ereq <-
    try (parseRequest "https://api.stripe.com/v1/products?active=true&limit=100&expand%5B%5D=data.default_price")
      :: IO (Either SomeException Request)
  case ereq of
    Left e -> pure (Left (T.pack (show e)))
    Right base -> do
      let httpReq =
            base
              { method = "GET"
              , requestHeaders = [("Authorization", TE.encodeUtf8 ("Bearer " <> secretText sk))]
              }
      res <- try (httpLbs httpReq mgr) :: IO (Either SomeException (Response BL.ByteString))
      pure $ case res of
        Left (e :: SomeException) -> Left (T.pack (show e))
        Right resp ->
          let code = statusCode (responseStatus resp)
              bodyStr = T.unpack (TE.decodeUtf8 (BL.toStrict (responseBody resp)))
           in if code >= 200 && code < 300
                then either (Left . T.pack) Right (parseCatalogJSON bodyStr)
                else Left ("HTTP " <> T.pack (show code) <> ": " <> T.pack (take 300 bodyStr))
