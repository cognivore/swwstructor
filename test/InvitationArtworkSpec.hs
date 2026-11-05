{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Swwstructor.Money (Currency (GBP), Price, price)
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
      let details = Just (InvitationEventDetails "18 November 2026" "Ukiyo, 8 Slingsby Place, London WC2E 9AB")
          svg = invitationSvg SharedInvitation brand "<Emilie & 友達>" (price GBP 65) details matrix
          personal = invitationSvg PersonalInvitation brand "Tess" (price GBP 65) details matrix
          shared = invitationSvg SharedInvitation brand "Tess" (price GBP 65) details matrix
      check "Name is escaped in SVG" ("&lt;Emilie &amp; 友達&gt;" `T.isInfixOf` svg)
      check "Date appears in event invitation" ("18 November 2026" `T.isInfixOf` svg)
      check "Location appears in both variants" (all (T.isInfixOf "Ukiyo, 8 Slingsby Place, London WC2E 9AB") [shared, personal])
      check "QR has a white quiet zone" ("width=\"29\" height=\"29\" fill=\"white\"" `T.isInfixOf` svg)
      check "Palette cannot request a local paint URL" (not ("url(" `T.isInfixOf` invitationSvg SharedInvitation (brand {themeBg = Color "url(/etc/passwd)"}) "Friend" (price GBP 65) Nothing matrix))
      check "Complete headline precedes QR" (T.length (fst (T.breakOn "to a mochi-making class" svg)) < T.length (fst (T.breakOn "<path fill=\"black\"" svg)))
      check "Personal variant addresses the recipient" (">Dear Tess,<" `T.isInfixOf` personal && ">you are invited to a<" `T.isInfixOf` personal)
      check "Shared headline reads as one invitation" (">and Tess invite you<" `T.isInfixOf` shared && ">to a mochi-making class<" `T.isInfixOf` shared)
      check "Invitation has only three text sizes" (all ((== 3) . length . nub . textSizes) [shared, personal])
      check "Invitation has no middle-dot separators" (all (not . T.isInfixOf "·") [shared, personal])
      check "Both variants retain the exact same QR matrix" (qrFragment personal == qrFragment shared)
      check "Compact export uses A6 portrait" ("width=\"105mm\" height=\"148mm\"" `T.isInfixOf` personal)
      mapM_ (\style -> mapM_ (\(ticketPrice, label) -> do
        let priced = invitationSvg style brand "Tess" ticketPrice details matrix
        check "Each invitation shows its supplied per-person price" (("Ticket price: " <> label <> " per person") `T.isInfixOf` priced)
        check "Booking CTA does not imply an account" ("Scan to book your place" `T.isInfixOf` priced)
        check "Price does not alter the QR payload" (qrFragment priced == qrFragment shared))
        [(price GBP 65, "£65"), (price GBP 75, "£75")]) [SharedInvitation, PersonalInvitation]
  args <- getArgs
  case args of
    ["--render", dir] -> renderFixtures False dir
    ["--render-prices", dir] -> renderFixtures True dir
    [] -> pure ()
    _ -> die "Usage: invitation-artwork-test [--render DIRECTORY | --render-prices DIRECTORY]"
  putStrLn "Invitation artwork checks passed"

brand :: Theme
brand = defaultTheme
  { themeBg = Color "#fff5f3", themeFg = Color "#2d2926", themeAccent = Color "#791a3e"
  , themeMastFont = "'Cormorant Garamond', 'Noto Serif JP', serif"
  , themeDisplayFont = "'Cormorant Garamond', 'Noto Serif JP', serif"
  , themeBodyFont = "'Inter', 'Noto Serif JP', sans-serif" }

renderFixtures :: Bool -> FilePath -> IO ()
renderFixtures pricesOnly dir = do
  createDirectoryIfMissing True dir
  renderer <- newArtworkRenderer (dir </> "private-tmp")
  let url = "https://okashi-school.com/invite/in_0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
  TIO.writeFile (dir </> "expected-qr.txt") url
  mapM_ (render renderer url) (if pricesOnly then take 2 fixtures else fixtures)
  rejected <- renderInvitationArtifact renderer ArtifactPng SharedInvitation brand (T.replicate 101 "W") url (price GBP 65) Nothing
  unless (isLeft rejected) (die "Oversized name reached export")
  where
    fixtures :: [(FilePath, Text, Price, Maybe InvitationEventDetails)]
    fixtures =
      [ ("tess", "Tess", price GBP 65, atUkiyo "18 November 2026, 19:00–21:00")
      , ("partner-event", "UKIYO", price GBP 75, atUkiyo "18 November 2026, 19:00–21:00")
      , ("friend-unicode", "Émilie & 友達", price GBP 65, Nothing)
      , ("long-name", "Translator Magazine and the Society of Friends of Japanese Sweets, Tea and Handcrafted Delights", price GBP 65, Just (InvitationEventDetails "Wednesday 18 November 2026, 19:00–21:00" "Ukiyo, The Workshop and Japanese Tea Room, Courtyard Entrance, 8 Slingsby Place, Covent Garden, London WC2E 9AB, United Kingdom"))
      , ("wide-name", T.replicate 100 "W", price GBP 65, Nothing)
      , ("japanese-name", T.replicate 50 "友達", price GBP 65, atUkiyo "18 November 2026")
      ]
    atUkiyo date = Just (InvitationEventDetails date "Ukiyo, 8 Slingsby Place, London WC2E 9AB")
    render renderer url (stem, name, ticketPrice, event) = mapM_ (\(style, styleName) -> mapM_ (one renderer url (stem <> "-" <> styleName) name ticketPrice event style) [(ArtifactPng,"png"),(ArtifactPdf,"pdf")]) [(SharedInvitation,"shared"),(PersonalInvitation,"personal")]
    one renderer url stem name ticketPrice event style (format, suffix) = do
      rendered <- renderInvitationArtifact renderer format style brand name url ticketPrice event
      case rendered of
        Left err -> die (show err)
        Right bytes -> BS.writeFile (dir </> stem <> "." <> suffix) bytes

qrFragment :: Text -> Text
qrFragment = fst . T.breakOn "</g>" . snd . T.breakOn "<g transform="

textSizes :: Text -> [Text]
textSizes = map (T.takeWhile (/= '"')) . drop 1 . T.splitOn "font-size=\""
