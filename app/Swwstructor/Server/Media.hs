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
  , listAssets
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
import Swwstructor.Media (MediaKind, isHeicBytes, isSafeMediaName, mediaFileName, sniffKind)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getFileSize
  , getModificationTime
  , listDirectory
  , removeFile
  , renameFile
  )
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

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
  listImagesIn dir

-- | The photos that SHIPPED with the site (the read-only @assets/@ of the
-- deployed content template, served at @\/assets\/*@) — shown in the admin
-- library so the owner can browse and reuse them, but not delete them.
listAssets :: FilePath -> IO [MediaEntry]
listAssets siteDir = do
  let dir = siteDir </> "assets"
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      r <- try (listImagesIn dir) :: IO (Either SomeException [MediaEntry])
      pure (either (const []) id r)

listImagesIn :: FilePath -> IO [MediaEntry]
listImagesIn dir = do
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
-- iPhone HEIC photos — which no browser can display — are transcoded to JPEG
-- first (ImageMagick), then stored through the same path.
-- Returns the stored (public) name.
saveMedia :: FilePath -> Text -> ByteString -> IO (Either Text Text)
saveMedia dataDir origName bytes
  | BS.length bytes > maxUploadBytes =
      pure (Left (shortName <> ": larger than 15 MB — resize it first"))
  | BS.null bytes = pure (Left (shortName <> ": empty file"))
  | otherwise = case sniffKind bytes of
      Just kind -> store kind bytes
      Nothing
        | isHeicBytes bytes -> do
            converted <- heicToJpeg dataDir bytes
            case converted of
              Right jpegBytes | Just kind <- sniffKind jpegBytes -> store kind jpegBytes
              Right _ -> pure (Left (shortName <> ": conversion produced something unexpected"))
              Left e -> pure (Left (shortName <> ": " <> e))
        | otherwise ->
            pure (Left (shortName <> ": not a recognised image (jpeg, png, gif, webp, svg, avif — or an iPhone heic, which is converted automatically)"))
  where
    shortName = T.take 80 origName
    store kind payload = do
      let dir = mediaDirOf dataDir
          name = mediaFileName origName (hashPrefix payload) kind
          path = dir </> T.unpack name
          tmp = dir </> ("." <> T.unpack name <> ".tmp")
      createDirectoryIfMissing True dir
      r <- try (BS.writeFile tmp payload >> renameFile tmp path) :: IO (Either SomeException ())
      pure $ case r of
        Left e -> Left (shortName <> ": could not save (" <> T.pack (show e) <> ")")
        Right () -> Right name

-- | Transcode a HEIC payload to JPEG with ImageMagick: bounded run time and
-- memory, auto-orientation (iPhone rotation lives in metadata), metadata
-- stripped, capped at 2000px on the long edge — the same budget the browser
-- compressor uses. Requires @magick@ on PATH (the NixOS module provides it).
heicToJpeg :: FilePath -> ByteString -> IO (Either Text ByteString)
heicToJpeg dataDir bytes = do
  let dir = mediaDirOf dataDir
      stamp = T.unpack (hashPrefix bytes)
      tmpIn = dir </> ("." <> stamp <> ".heic")
      tmpOut = dir </> ("." <> stamp <> ".jpg")
      cleanup = do
        _ <- try (removeFile tmpIn) :: IO (Either SomeException ())
        _ <- try (removeFile tmpOut) :: IO (Either SomeException ())
        pure ()
  createDirectoryIfMissing True dir
  r <- try $ do
    BS.writeFile tmpIn bytes
    res <-
      timeout (30 * 1000000) $
        readProcessWithExitCode
          "magick"
          [ tmpIn
          , "-limit", "memory", "512MiB"
          , "-limit", "map", "1GiB"
          , "-auto-orient"
          , "-strip"
          , "-resize", "2000x2000>"
          , "-quality", "85"
          , tmpOut
          ]
          ""
    case res of
      Nothing -> pure (Left "converting the photo took too long")
      Just (ExitSuccess, _, _) -> Right <$> BS.readFile tmpOut
      Just (_, _, err) -> pure (Left ("could not convert this iPhone photo (HEIC) — export it as JPEG and upload that. (" <> T.pack (take 120 err) <> ")"))
  cleanup
  pure $ case (r :: Either SomeException (Either Text ByteString)) of
    Left e
      | "does not exist" `T.isInfixOf` T.pack (show e) ->
          Left "this server cannot convert HEIC yet (converter missing) — export the photo as JPEG and upload that"
      | otherwise -> Left ("could not convert this iPhone photo (HEIC): " <> T.pack (take 120 (show e)))
    Right out -> out

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
