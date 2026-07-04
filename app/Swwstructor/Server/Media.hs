{-# LANGUAGE OverloadedStrings #-}

-- | The media library's IO: save (multi-)uploads into the instance's writable
-- data dir, list them for the admin, delete, and resolve a public name to a
-- file on disk. Uploaded bytes are sniffed ("Swwstructor.Media") and stored
-- under a content-hashed name, so uploads are idempotent and @\/media\/*@ can
-- be cached immutably. Everything stays inside @\<data dir>\/uploads@ — names
-- are validated with 'isSafeMediaName' before any path is formed.
module Swwstructor.Server.Media
  ( mediaDirOf
  , MediaEntry (..)
  , listMedia
  , saveMedia
  , deleteMedia
  , resolveMedia
  , maxUploadBytes
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (filterM, forM)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteArray.Encoding as BAE
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (sortOn)
import Data.Ord (Down (Down))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime)
import Swwstructor.Media (MediaKind, isSafeMediaName, mediaFileName, sniffKind)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getFileSize
  , getModificationTime
  , listDirectory
  , removeFile
  , renameFile
  )
import System.FilePath ((</>))

-- | Where an instance's uploads live: a sibling of @secrets.enc@ and the
-- writable @site.json@, i.e. state — never the (read-only) site template dir.
mediaDirOf :: FilePath -> FilePath
mediaDirOf dataDir = dataDir </> "uploads"

-- | One stored upload, as the admin library shows it.
data MediaEntry = MediaEntry
  { meName :: !Text
  , meBytes :: !Integer
  , meModified :: !UTCTime
  }
  deriving (Eq, Show)

-- | Refuse uploads past this size (per file, after client-side compression).
maxUploadBytes :: Int
maxUploadBytes = 15 * 1024 * 1024

-- | The library, newest first. Ignores anything that is not a safe media name
-- (dotfiles, temp files, strays someone scp'd in).
listMedia :: FilePath -> IO [MediaEntry]
listMedia dataDir = do
  let dir = mediaDirOf dataDir
  createDirectoryIfMissing True dir
  names <- filter (isSafeMediaName . T.pack) <$> listDirectory dir
  present <- filterM (doesFileExist . (dir </>)) names
  entries <- forM present $ \n -> do
    size <- getFileSize (dir </> n)
    mtime <- getModificationTime (dir </> n)
    pure (MediaEntry (T.pack n) size mtime)
  pure (sortOn (Down . meModified) entries)

-- | Store one uploaded file: sniff the type (images only), bound the size,
-- name it @\<slug>-\<sha256 prefix>.\<ext>@, and write atomically (dot-prefixed
-- temp + rename; the dot keeps half-written files out of 'listMedia').
-- Returns the stored (public) name.
saveMedia :: FilePath -> Text -> ByteString -> IO (Either Text Text)
saveMedia dataDir origName bytes
  | BS.length bytes > maxUploadBytes =
      pure (Left (shortName <> ": larger than 15 MB — resize it first"))
  | BS.null bytes = pure (Left (shortName <> ": empty file"))
  | otherwise = case sniffKind bytes of
      Nothing -> pure (Left (shortName <> ": not a recognised image (jpeg, png, gif, webp or svg)"))
      Just kind -> do
        let dir = mediaDirOf dataDir
            name = mediaFileName origName (hashPrefix bytes) kind
            path = dir </> T.unpack name
            tmp = dir </> ("." <> T.unpack name <> ".tmp")
        createDirectoryIfMissing True dir
        r <- try (BS.writeFile tmp bytes >> renameFile tmp path) :: IO (Either SomeException ())
        pure $ case r of
          Left e -> Left (shortName <> ": could not save (" <> T.pack (show e) <> ")")
          Right () -> Right name
  where
    shortName = T.take 80 origName

-- | 12 hex chars of the content's SHA-256 — enough to make names unique per
-- content and stable across re-uploads.
hashPrefix :: ByteString -> Text
hashPrefix bytes =
  let digest = hash bytes :: Digest SHA256
      hex = TE.decodeUtf8 (BAE.convertToBase BAE.Base16 digest)
   in T.take 12 hex

-- | Delete a stored upload by its public name.
deleteMedia :: FilePath -> Text -> IO Bool
deleteMedia dataDir name
  | not (isSafeMediaName name) = pure False
  | otherwise = do
      let path = mediaDirOf dataDir </> T.unpack name
      r <- try (removeFile path) :: IO (Either SomeException ())
      pure (either (const False) (const True) r)

-- | Resolve a public @\/media\/:file@ name to a file on disk, iff it is a safe
-- name and the file exists.
resolveMedia :: FilePath -> Text -> IO (Maybe FilePath)
resolveMedia dataDir name
  | not (isSafeMediaName name) = pure Nothing
  | otherwise = do
      let path = mediaDirOf dataDir </> T.unpack name
      ok <- doesFileExist path
      pure (if ok then Just path else Nothing)
