{-# LANGUAGE OverloadedStrings #-}

-- | The pure media-library core: filename hygiene, image-type sniffing (magic
-- bytes, never the client's say-so), and content types for serving. The IO —
-- writing files, hashing bytes into names, listing the library — lives in the
-- server ("Swwstructor.Server.Media"); everything here is a total function,
-- offline-testable like the rest of the constructor core.
module Swwstructor.Media
  ( MediaKind (..)
  , sniffKind
  , kindExtension
  , kindContentType
  , contentTypeFor
  , sanitizeBaseName
  , mediaFileName
  , isSafeMediaName
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum, isAscii, isDigit, toLower)
import Data.Text (Text)
import qualified Data.Text as T

-- | The closed set of media types the library accepts — images only (this is
-- the site's photo library, not a file share). Detected by content, so a
-- renamed @.exe@ is refused no matter what its name claims.
data MediaKind = Jpeg | Png | Gif | Webp | Svg
  deriving (Eq, Show, Enum, Bounded)

-- | Detect the media kind from the first bytes of the payload.
sniffKind :: ByteString -> Maybe MediaKind
sniffKind bs
  | BS.take 3 bs == BS.pack [0xFF, 0xD8, 0xFF] = Just Jpeg
  | BS.take 8 bs == BS.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A] = Just Png
  | BS.take 4 bs == "GIF8" = Just Gif
  | BS.take 4 bs == "RIFF" && BS.take 4 (BS.drop 8 bs) == "WEBP" = Just Webp
  | isSvgish (BS.take 1024 bs) = Just Svg
  | otherwise = Nothing
  where
    -- an XML-looking text head that names an <svg> root
    isSvgish h = "<svg" `BS.isInfixOf` h || "<SVG" `BS.isInfixOf` h

-- | The canonical extension a stored file of this kind gets.
kindExtension :: MediaKind -> Text
kindExtension k = case k of
  Jpeg -> "jpg"
  Png -> "png"
  Gif -> "gif"
  Webp -> "webp"
  Svg -> "svg"

-- | The @Content-Type@ a stored file of this kind is served with.
kindContentType :: MediaKind -> Text
kindContentType k = case k of
  Jpeg -> "image/jpeg"
  Png -> "image/png"
  Gif -> "image/gif"
  Webp -> "image/webp"
  Svg -> "image/svg+xml"

-- | The @Content-Type@ for a stored file name, by its extension (the inverse
-- of 'kindExtension', plus the legacy @.jpeg@ spelling for site assets).
contentTypeFor :: Text -> Maybe Text
contentTypeFor name = case T.toLower (T.takeWhileEnd (/= '.') name) of
  "jpg" -> Just "image/jpeg"
  "jpeg" -> Just "image/jpeg"
  "png" -> Just "image/png"
  "gif" -> Just "image/gif"
  "webp" -> Just "image/webp"
  "svg" -> Just "image/svg+xml"
  _ -> Nothing

-- | Reduce an owner's file name (minus extension) to a safe, readable slug:
-- ASCII alphanumerics and dashes only, collapsed, bounded, never empty.
sanitizeBaseName :: Text -> Text
sanitizeBaseName raw =
  let noExt = dropExt (T.strip raw)
      slug =
        T.dropWhileEnd (== '-')
          . T.dropWhile (== '-')
          . collapseDashes
          . T.map dashify
          $ T.toLower noExt
   in if T.null slug then "img" else T.take 48 slug
  where
    dropExt t =
      let (stem, ext) = T.breakOnEnd "." t
       in if T.null stem || T.any (not . extish) ext then t else T.dropWhileEnd (== '.') stem
    extish c = isAscii c && (isAlphaNum c || isDigit c)
    dashify c
      | isAscii c && isAlphaNum c = toLower c
      | otherwise = '-'
    collapseDashes = T.intercalate "-" . filter (not . T.null) . T.splitOn "-"

-- | The stored name for an upload: @\<slug>-\<content-hash-prefix>.\<ext>@.
-- The hash prefix makes the name unique per content (and idempotent: the same
-- photo uploaded twice lands on the same name), which is what lets @\/media\/*@
-- be served with immutable caching.
mediaFileName :: Text -> Text -> MediaKind -> Text
mediaFileName origName hashPrefix kind =
  sanitizeBaseName origName <> "-" <> T.filter hexish (T.toLower hashPrefix) <> "." <> kindExtension kind
  where
    hexish c = isDigit c || (c >= 'a' && c <= 'f')

-- | What the public @\/media\/:file@ route (and delete) will touch: a single
-- flat, visible file name in the library's character set — no separators, no
-- dotfiles, no traversal, bounded length.
isSafeMediaName :: Text -> Bool
isSafeMediaName name =
  not (T.null name)
    && T.length name <= 120
    && not ("." `T.isPrefixOf` name)
    && not (".." `T.isInfixOf` name)
    && T.all ok name
  where
    ok c = (isAscii c && isAlphaNum c) || c == '-' || c == '_' || c == '.'
