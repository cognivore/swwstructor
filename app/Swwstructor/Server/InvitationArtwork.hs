{-# LANGUAGE OverloadedStrings #-}

-- | A local export boundary. Only our bounded SVG template reaches librsvg;
-- neither the invitation URL nor an owner-supplied file is fetched. qrencode
-- receives the secret URL on stdin and returns an independently parsed matrix.
module Swwstructor.Server.InvitationArtwork
  ( ArtworkRenderer
  , ArtifactFormat (..)
  , InvitationStyle (..)
  , InvitationEventDetails (..)
  , ArtworkError (..)
  , newArtworkRenderer
  , renderInvitationArtifact
  , QrMatrix
  , parseQrMatrix
  , invitationSvg
  , wrapInvitationName
  ) where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, tryTakeMVar)
import Control.Exception (IOException, finally, onException, try)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Char (GeneralCategory (..), generalCategory, isControl, isHexDigit, ord)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Swwstructor.Html (elAttr, esc, renderHtml)
import Swwstructor.Money (Price, formatPrice)
import Swwstructor.Theme (Color (..), Theme (..), sanitizeCssValue)
import System.Directory (createDirectoryIfMissing, getFileSize, removeFile)
import System.Exit (ExitCode (..))
import System.IO (hClose, openBinaryTempFile)
import System.IO.Error (isDoesNotExistError)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

data ArtifactFormat = ArtifactPng | ArtifactPdf deriving (Eq, Show)

-- The same invitation link can be shared by a co-host or addressed personally.
-- Style changes presentation only; it never changes the offer or QR payload.
data InvitationStyle = SharedInvitation | PersonalInvitation deriving (Eq, Show)

data InvitationEventDetails = InvitationEventDetails
  { ieDateLabel :: Text
  , ieLocation :: Text
  } deriving (Eq, Show)

-- No input names, tokens, command arguments or converter stderr are retained:
-- those may disclose the bearer link. These constructors retain the safe
-- operation, exit status, output contract and cleanup outcome for the owner.
data ArtworkError
  = ArtworkBusy
  | ArtworkInvalidInput Text
  | ArtworkToolUnavailable Text
  | ArtworkTimedOut Text
  | ArtworkToolFailed Text Int
  | ArtworkInvalidOutput Text
  | ArtworkFileFailure Text
  | ArtworkCleanupFailed (Maybe ArtworkError)
  deriving (Eq, Show)

data ArtworkRenderer = ArtworkRenderer !FilePath !(MVar ())

newArtworkRenderer :: FilePath -> IO ArtworkRenderer
newArtworkRenderer dir = ArtworkRenderer dir <$> newMVar ()

-- Opaque: every row has the same valid QR version width, with one Boolean per
-- module. The four-module quiet zone is added by the SVG renderer, not guessed
-- from the encoder's whitespace.
newtype QrMatrix = QrMatrix [[Bool]]

parseQrMatrix :: Text -> Either ArtworkError QrMatrix
parseQrMatrix encoded
  | T.length encoded > 64000 = invalid
  | side < 21 || side > 177 || (side - 21) `mod` 4 /= 0 = invalid
  | any ((/= 2 * side) . T.length) rows = invalid
  | otherwise = QrMatrix <$> traverse (traverse cell . T.chunksOf 2) rows
  where
    rows = T.lines encoded
    side = length rows
    invalid = Left (ArtworkInvalidOutput "QR encoder returned an invalid square matrix")
    cell "##" = Right True
    cell "  " = Right False
    cell _ = Left (ArtworkInvalidOutput "QR encoder returned an unknown module")

-- | One export may run at a time; public request pressure cannot create an
-- unbounded converter queue. All mutable file/process work lives here, while
-- the presentation below is a pure value interpreter.
renderInvitationArtifact :: ArtworkRenderer -> ArtifactFormat -> InvitationStyle -> Theme -> Text -> Text -> Price -> Maybe InvitationEventDetails -> IO (Either ArtworkError ByteString)
renderInvitationArtifact (ArtworkRenderer dir gate) format style theme name url ticketPrice event =
  case admissible of
    Left err -> pure (Left err)
    Right () -> do
      admitted <- tryTakeMVar gate
      case admitted of
        Nothing -> pure (Left ArtworkBusy)
        Just () -> export `finally` putMVar gate ()
  where
    admissible
      | T.null (T.strip name) || T.length name > 100 || T.any isControl name = Left (ArtworkInvalidInput "Inviter name must contain 1 to 100 visible characters")
      | BS.length (TE.encodeUtf8 url) > 1024 || T.any isControl url || not ("https://" `T.isPrefixOf` url || "http://" `T.isPrefixOf` url) = Left (ArtworkInvalidInput "Invitation URL must be a bounded absolute HTTP URL")
      | maybe False (\v -> invalidText 100 (ieDateLabel v) || invalidText 400 (ieLocation v)) event = Left (ArtworkInvalidInput "Event date and location must contain 1 to 100 and 1 to 400 visible characters respectively")
      | otherwise = Right ()
    invalidText limit value = T.null (T.strip value) || T.length value > limit || T.any isControl value
    export = do
      qr <- runTool "qrencode" ["-t", "ASCII", "-m", "0", "-l", "M", "-o", "-"] (T.unpack url)
      case qr >>= parseQrMatrix . T.pack of
        Left err -> pure (Left err)
        Right matrix -> do
          created <- try (createDirectoryIfMissing True dir >> openBinaryTempFile dir ".invitation-export")
          case created of
            Left (_ :: IOException) -> pure (Left (ArtworkFileFailure "Could not allocate a private invitation export file"))
            Right (path, handle) -> do
              let abandon = do
                    _ <- try (hClose handle) :: IO (Either IOException ())
                    _ <- try (removeFile path) :: IO (Either IOException ())
                    pure ()
              result <- (hClose handle >> renderTo path (invitationSvg style theme name ticketPrice event matrix)) `onException` abandon
              cleaned <- try (removeFile path)
              pure $ case cleaned of
                Left (_ :: IOException) -> Left (ArtworkCleanupFailed (either Just (const Nothing) result))
                Right () -> result
    renderTo path svg = do
      converted <- runTool "rsvg-convert" (["--format", extension, "--output", path] <> dimensions) (T.unpack svg)
      case converted of
        Left err -> pure (Left err)
        Right _ -> do
          loaded <- try $ do
            size <- getFileSize path
            if size <= 0 || size > 16 * 1024 * 1024
              then pure (Left (ArtworkInvalidOutput "Invitation export exceeded its 16 MiB output limit or was empty"))
              else do
                bytes <- BS.readFile path
                pure $ if signature `BS.isPrefixOf` bytes
                  then Right bytes
                  else Left (ArtworkInvalidOutput "Converter output did not match the requested file format")
          pure $ either (\(_ :: IOException) -> Left (ArtworkFileFailure "Could not read the completed invitation export")) id loaded
    extension = case format of ArtifactPng -> "png"; ArtifactPdf -> "pdf"
    dimensions = case format of
      ArtifactPng -> ["--width", "1575", "--height", "2220"]
      ArtifactPdf -> ["--width", "105mm", "--height", "148mm", "--page-width", "105mm", "--page-height", "148mm"]
    signature = case format of
      ArtifactPng -> BS.pack [137,80,78,71,13,10,26,10]
      ArtifactPdf -> "%PDF-"

-- The packaged tools have bounded output for our bounded inputs. readProcess
-- owns/terminates its child on an async timeout; no shell interprets arguments.
runTool :: Text -> [String] -> String -> IO (Either ArtworkError String)
runTool tool args input = do
  attempted <- try (timeout (15 * 1000000) (readProcessWithExitCode (T.unpack tool) args input))
  pure $ case attempted of
    Left (err :: IOException)
      | isDoesNotExistError err -> Left (ArtworkToolUnavailable tool)
      | otherwise -> Left (ArtworkFileFailure ("Could not run " <> tool <> "; input and native diagnostics withheld because they may contain a private invitation"))
    Right Nothing -> Left (ArtworkTimedOut tool)
    Right (Just (ExitFailure code, _, _)) -> Left (ArtworkToolFailed tool code)
    Right (Just (ExitSuccess, out, _))
      | length out > 64000 -> Left (ArtworkInvalidOutput (tool <> " exceeded its output contract"))
      | otherwise -> Right out

-- | Conservative character-cell wrapping, retaining combining marks with their
-- base. CJK/wide codepoints count as two cells. A single unbroken name wraps
-- too; owner text is never cropped or interpreted as markup.
wrapInvitationName :: Int -> Text -> [Text]
wrapInvitationName cells input = finish (foldl addWord ([], "", 0) pieces)
  where
    pieces = concatMap (finish . foldl step ([], "", 0) . clusters . T.unpack) (T.words input)
    addWord (done, line, used) word
      | T.null line = (done, word, units word)
      | used + 1 + units word > cells = (done <> [line], word, units word)
      | otherwise = (done, line <> " " <> word, used + 1 + units word)
    step (done, line, used) cluster
      | used > 0 && used + width cluster > cells = (done <> [line], cluster, width cluster)
      | otherwise = (done, line <> cluster, used + width cluster)
    finish (done, line, _) = filter (not . T.null) (done <> [T.strip line])
    clusters [] = []
    clusters (c:cs) = let (marks, rest) = span combining cs in T.pack (c:marks) : clusters rest
    combining c = generalCategory c `elem` [NonSpacingMark, SpacingCombiningMark, EnclosingMark] || ord c == 0xfe0f
    units = sum . map width . clusters . T.unpack
    width t = case T.uncons t of
      Nothing -> 0
      Just (c, _) | ord c >= 0x2e80 -> 2
      Just _ -> 1

invitationSvg :: InvitationStyle -> Theme -> Text -> Price -> Maybe InvitationEventDetails -> QrMatrix -> Text
invitationSvg style theme name ticketPrice event (QrMatrix matrix) =
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"105mm\" height=\"148mm\" viewBox=\"0 0 1050 1480\">"
  <> "<rect width=\"1050\" height=\"1480\" fill=\"" <> background <> "\"/>"
  <> "<path d=\"M72 72 H978 M72 1408 H978\" stroke=\"" <> accent <> "\" stroke-width=\"2\"/>"
  <> textAt 179 77 mast "600" foreground "Okashi School"
  <> T.concat [textAt (nameTop + fromIntegral i * nameLineHeight) nameSize display "500" foreground line | (i,line) <- zip [0 :: Int ..] nameLines]
  <> "<path d=\"M460 378 H590\" stroke=\"" <> accent <> "\" stroke-width=\"2\"/>"
  <> qrSvg 424
  <> textAt 759 22 body "500" accent "Scan to book your place"
  <> textAt 793 25 body "500" foreground ("Ticket price: " <> formatPrice ticketPrice <> " per person")
  <> textAt 857 39 display "500" foreground invitationLine
  <> textAt 946 83 display "600" foreground "mochi-making"
  <> textAt 1020 83 display "600" foreground "class"
  <> T.concat [textAt (eventTop + fromIntegral i * 30) 24 body "500" accent line | (i,line) <- zip [0 :: Int ..] eventLines]
  <> T.concat [textAt (locationTop + fromIntegral i * locationLineHeight) locationSize body "400" foreground line | (i,line) <- zip [0 :: Int ..] locationLines]
  <> textAt 1286 17 body "500" foreground "JAPANESE PASTRY & WAGASHI · LONDON"
  <> textAt 1331 18 body "400" foreground "okashi-school.com"
  <> "</svg>"
  where
    -- SVG paint values must not admit url(...), including local file paints.
    -- The site's canonical palette is hexadecimal; invalid owner values get
    -- the brand's safe literal fallback rather than an external reference.
    cleanColor fallback (Color value) = case T.uncons value of
      Just ('#', digits) | T.length digits `elem` [3,6,8] && T.all isHexDigit digits -> value
      _ -> fallback
    background = cleanColor "#fff5f3" (themeBg theme)
    accent = cleanColor "#791a3e" (themeAccent theme)
    foreground = cleanColor "#2d2926" (themeFg theme)
    mast = sanitizeCssValue (themeMastFont theme)
    display = sanitizeCssValue (themeDisplayFont theme)
    body = sanitizeCssValue (themeBodyFont theme)
    recipient = case style of
      SharedInvitation -> "and " <> name
      PersonalInvitation -> "Dear " <> name <> ","
    invitationLine = case style of
      SharedInvitation -> "invite you to a"
      PersonalInvitation -> "you are invited to a"
    nameLines = wrapInvitationName 42 recipient
    -- The name owns a bounded 135px area; long names wrap and shrink together,
    -- rather than pushing the QR or the class details off the printed card.
    nameSize = minimum [62, 820 / maximum (1 : map lineUnits nameLines), 135 / (fromIntegral (max 1 (length nameLines)) * 1.18)]
    lineUnits = sum . map glyphUnits . T.unpack
    glyphUnits c
      | generalCategory c `elem` [NonSpacingMark, SpacingCombiningMark, EnclosingMark] = 0
      | ord c >= 0x2e80 || c `elem` ("MW@%&—" :: String) = 1
      | c `elem` ("ilIjtfr., '’\x2011-" :: String) = 0.4
      | otherwise = 0.7
    nameLineHeight = nameSize * 1.18
    nameHeight = fromIntegral (max 1 (length nameLines)) * nameLineHeight
    nameTop = 284 - nameHeight / 2 + nameSize * 0.8
    eventLines = maybe ["Choose your evening"] (wrapInvitationName 57 . ieDateLabel) event
    eventTop = 1108
    locationLines = maybe [] (wrapInvitationName 62 . ieLocation) event
    locationTop = eventTop + fromIntegral (length eventLines) * 30 + 16
    locationSize = min 23 ((1238 - locationTop) / (fromIntegral (max 1 (length locationLines)) * 1.3))
    locationLineHeight = locationSize * 1.3
    qrSize = 292
    number :: Double -> Text
    number = T.pack . show
    textAt y size family weight fill content = renderHtml $
      elAttr "text" [("x","525"),("y",number y),("text-anchor","middle"),("font-family",family),("font-size",number size),("font-weight",weight),("fill",fill)] (esc content)
    side = length matrix
    scale = qrSize / fromIntegral (side + 8) :: Double
    qrSvg y = "<g transform=\"translate(" <> number ((1050-qrSize)/2) <> " " <> number y <> ") scale(" <> number scale <> ")\">"
      <> "<rect width=\"" <> T.pack (show (side+8)) <> "\" height=\"" <> T.pack (show (side+8)) <> "\" fill=\"white\"/>"
      <> "<path fill=\"black\" d=\""
      <> T.concat ["M" <> T.pack (show (x+4)) <> " " <> T.pack (show (row+4)) <> "h1v1h-1z" | (row,cells) <- zip [0 :: Int ..] matrix, (x,black) <- zip [0 :: Int ..] cells, black]
      <> "\"/></g>"
