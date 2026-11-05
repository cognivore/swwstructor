{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Swwstructor.Server.InvitationArtwork
import Swwstructor.Theme
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))

main :: IO ()
main = do
  let check label condition = unless condition (die label)
      validRows = T.unlines (replicate 21 (T.replicate 21 "##"))
  check "Reject nonsquare QR output" (isLeft (parseQrMatrix "##  \n"))
  check "Reject invalid QR version width" (isLeft (parseQrMatrix (T.unlines (replicate 22 (T.replicate 22 "##")))))
  check "Reject nonbinary QR cells" (isLeft (parseQrMatrix (T.replace "##" "xx" validRows)))
  check "Long names wrap at word boundaries" (wrapInvitationName 18 "Translator Magazine" == ["Translator", "Magazine"])
  check "Unbroken names wrap without dropping characters" (T.concat (wrapInvitationName 30 (T.replicate 100 "W")) == T.replicate 100 "W")
  check "Combining accent remains with base" (wrapInvitationName 1 "e\x301\&e\x301" == ["e\x301", "e\x301"])
  case parseQrMatrix validRows of
    Left err -> die (show err)
    Right matrix -> do
      let svg = invitationSvg brand "<Emilie & 友達>" (Just "18 November 2026") matrix
      check "Name is escaped in SVG" ("&lt;Emilie &amp; 友達&gt;" `T.isInfixOf` svg)
      check "Date appears in event invitation" ("18 November 2026" `T.isInfixOf` svg)
      check "QR has a white quiet zone" ("width=\"29\" height=\"29\" fill=\"white\"" `T.isInfixOf` svg)
      check "Palette cannot request a local paint URL" (not ("url(" `T.isInfixOf` invitationSvg (brand {themeBg = Color "url(/etc/passwd)"}) "Friend" Nothing matrix))
      check "QR precedes invitation copy" (T.length (fst (T.breakOn "<path fill=\"black\"" svg)) < T.length (fst (T.breakOn "invite you to a" svg)))
  args <- getArgs
  case args of
    ["--render", dir] -> renderFixtures dir
    [] -> pure ()
    _ -> die "Usage: invitation-artwork-test [--render DIRECTORY]"
  putStrLn "Invitation artwork checks passed"

brand :: Theme
brand = defaultTheme
  { themeBg = Color "#fff5f3", themeFg = Color "#2d2926", themeAccent = Color "#791a3e"
  , themeMastFont = "'Cormorant Garamond', 'Noto Serif JP', serif"
  , themeDisplayFont = "'Cormorant Garamond', 'Noto Serif JP', serif"
  , themeBodyFont = "'Inter', 'Noto Serif JP', sans-serif" }

renderFixtures :: FilePath -> IO ()
renderFixtures dir = do
  createDirectoryIfMissing True dir
  renderer <- newArtworkRenderer (dir </> "private-tmp")
  let url = "https://okashi-school.com/invite/fixture-qr-verification"
  TIO.writeFile (dir </> "expected-qr.txt") url
  mapM_ (render renderer url) fixtures
  rejected <- renderInvitationArtifact renderer ArtifactPng brand (T.replicate 101 "W") url Nothing
  unless (isLeft rejected) (die "Oversized name reached export")
  where
    fixtures :: [(FilePath, Text, Maybe Text)]
    fixtures =
      [ ("partner-event", "UKIYO", Just "18 November 2026 · 19:00–21:00")
      , ("friend-unicode", "Émilie & 友達", Nothing)
      , ("long-name", "Translator Magazine and the Society of Friends of Japanese Sweets, Tea and Handcrafted Delights", Just "Wednesday 18 November 2026 · 19:00–21:00")
      , ("wide-name", T.replicate 100 "W", Nothing)
      , ("japanese-name", T.replicate 50 "友達", Just "18 November 2026")
      ]
    render renderer url (stem, name, event) = mapM_ (one renderer url stem name event) [(ArtifactPng,"png"),(ArtifactPdf,"pdf")]
    one renderer url stem name event (format, suffix) = do
      rendered <- renderInvitationArtifact renderer format brand name url event
      case rendered of
        Left err -> die (show err)
        Right bytes -> BS.writeFile (dir </> stem <> "." <> suffix) bytes
