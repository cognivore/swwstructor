{-# LANGUAGE OverloadedStrings #-}

-- | The runtime content store. The constructor's whole point is that content is
-- /edited at runtime/, not baked read-only into the Nix store — so the live site
-- is a writable @site.json@ held in an 'MVar', seeded once from the deployed
-- template, and saved atomically on every edit. Reads are lock-free-fast; edits
-- are serialised and persisted, surviving restarts.
module Swwstructor.Server.Store
  ( SiteStore
  , storeContentFile
  , newSiteStore
  , readSite
  , modifySite
  , saveSiteFile
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Swwstructor.Content (SiteSpec, siteSpecFromJSON, siteSpecToJSON)
import Swwstructor.Server.Loader (loadSite)
import StickyWM (decode, encode)
import System.Directory (doesFileExist, renameFile)

data SiteStore = SiteStore
  { ssVar :: !(MVar SiteSpec)
  , ssFile :: !FilePath
  }

storeContentFile :: SiteStore -> FilePath
storeContentFile = ssFile

-- | Open the store. If the writable content file exists, load it (the live,
-- owner-edited content); otherwise seed from the deployed template directory
-- (JSON or YAML) and write the content file. Returns 'Left' only if there is no
-- content at all to start from.
newSiteStore :: FilePath -> FilePath -> IO (Either String SiteStore)
newSiteStore contentFile seedDir = do
  exists <- doesFileExist contentFile
  eSite <-
    if exists
      then loadContentFile contentFile
      else loadSite seedDir
  case eSite of
    Left e -> pure (Left e)
    Right site -> do
      _ <- saveSiteFile contentFile site -- ensure the writable copy exists
      var <- newMVar site
      pure (Right (SiteStore var contentFile))

loadContentFile :: FilePath -> IO (Either String SiteSpec)
loadContentFile path = do
  eb <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
  pure $ case eb of
    Left e -> Left ("cannot read " <> path <> ": " <> show e)
    Right bs -> decode (T.unpack (TE.decodeUtf8 bs)) >>= siteSpecFromJSON

-- | The current live site.
readSite :: SiteStore -> IO SiteSpec
readSite = readMVar . ssVar

-- | Apply a pure edit, persist it, and return the persistence result. The
-- in-memory site is updated regardless (so the running server reflects the edit
-- even if the disk write fails — which is then surfaced as 'Left').
modifySite :: SiteStore -> (SiteSpec -> SiteSpec) -> IO (Either Text ())
modifySite store f =
  modifyMVar (ssVar store) $ \old -> do
    let new = f old
    r <- saveSiteFile (ssFile store) new
    pure (new, r)

-- | Atomically write a site to a JSON file (temp + rename).
saveSiteFile :: FilePath -> SiteSpec -> IO (Either Text ())
saveSiteFile path site = do
  let bytes = TE.encodeUtf8 (T.pack (encode (siteSpecToJSON site)))
      tmp = path <> ".tmp"
  r <- try (BS.writeFile tmp bytes >> renameFile tmp path) :: IO (Either SomeException ())
  pure (either (Left . T.pack . show) Right r)
