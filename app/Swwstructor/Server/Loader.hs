{-# LANGUAGE OverloadedStrings #-}

-- | Loading a site from a content directory. A site is a single file —
-- @site.json@ or @site.yaml@ — in the constructor's content schema. YAML is
-- converted to the engine's JSON value (via HsYAML) and then decoded by the same
-- 'siteSpecFromJSON' codec, so JSON and YAML are two spellings of one schema.
module Swwstructor.Server.Loader
  ( loadSite
  , loadSiteFile
  , yamlToJSON
  ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.YAML as Y
import Swwstructor.Content (SiteSpec, siteSpecFromJSON)
import StickyWM (JSON (..), decode)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- | Load a site from a content directory: tries @site.json@ then @site.yaml@
-- then @site.yml@.
loadSite :: FilePath -> IO (Either String SiteSpec)
loadSite dir = go ["site.json", "site.yaml", "site.yml"]
  where
    go [] = pure (Left ("no site.json/site.yaml found in " <> dir))
    go (f : fs) = do
      let path = dir </> f
      exists <- doesFileExist path
      if exists then loadSiteFile path else go fs

-- | Load a site from a specific file, dispatching on extension.
loadSiteFile :: FilePath -> IO (Either String SiteSpec)
loadSiteFile path = do
  eb <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
  case eb of
    Left e -> pure (Left ("cannot read " <> path <> ": " <> show e))
    Right bytes
      | isYaml path -> pure (yamlToJSON (BL.fromStrict bytes) >>= siteSpecFromJSON)
      | otherwise -> pure (decode (T.unpack (TE.decodeUtf8 bytes)) >>= siteSpecFromJSON)
  where
    isYaml p = ".yaml" `T.isSuffixOf` T.pack p || ".yml" `T.isSuffixOf` T.pack p

-- | Convert a YAML document to the engine's 'JSON' value, so the one JSON codec
-- can decode it.
yamlToJSON :: BL.ByteString -> Either String JSON
yamlToJSON bs =
  case Y.decode1 bs of
    Left (pos, msg) -> Left ("yaml parse error at " <> show pos <> ": " <> msg)
    Right node -> Right (nodeToJSON node)

nodeToJSON :: Y.Node Y.Pos -> JSON
nodeToJSON node = case node of
  Y.Scalar _ s -> scalarToJSON s
  Y.Mapping _ _ m -> JObj [(keyStr k, nodeToJSON v) | (k, v) <- Map.toList m]
  Y.Sequence _ _ xs -> JArr (map nodeToJSON xs)
  Y.Anchor _ _ n -> nodeToJSON n

scalarToJSON :: Y.Scalar -> JSON
scalarToJSON s = case s of
  Y.SNull -> JNull
  Y.SBool b -> JBool b
  Y.SFloat d -> JNum d
  Y.SInt i -> JNum (fromIntegral i)
  Y.SStr t -> JStr (T.unpack t)
  Y.SUnknown _ t -> JStr (T.unpack t)

keyStr :: Y.Node Y.Pos -> String
keyStr (Y.Scalar _ (Y.SStr t)) = T.unpack t
keyStr (Y.Scalar _ (Y.SInt i)) = show i
keyStr (Y.Scalar _ (Y.SBool b)) = if b then "true" else "false"
keyStr _ = ""
