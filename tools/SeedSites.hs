{-# LANGUAGE OverloadedStrings #-}

-- | A one-shot generator for the example site content files. Defines the demo
-- sites with the Content constructors (so they are guaranteed to type-check and
-- round-trip through the codec) and writes their canonical @site.json@. Run via:
--
-- @
-- ghc -XGHC2021 -isrc -i<engine>/src -o /tmp/seed tools/SeedSites.hs && /tmp/seed
-- @
module Main (main) where

import qualified Data.Text as T
import Swwstructor.Block (BuyTarget (BuyTarget), NavLink (NavLink))
import Swwstructor.Content
import qualified Swwstructor.Money as Money
import Swwstructor.Theme (nytTheme, okashiTheme)
import StickyWM (encode)

img :: T.Text -> Double -> Double -> ImageRef
img cap a c = ImageRef Nothing cap a c

-- ---------------------------------------------------------------------------
-- The New York Times benchmark front (content data only)
-- ---------------------------------------------------------------------------

nytSite :: SiteSpec
nytSite =
  SiteSpec
    { siteTitle = "The New York Times"
    , siteDescription = "Live news, investigations, opinion, photos and video."
    , siteBaseUrl = Just "https://times.example"
    , siteTheme = nytTheme
    , siteNav = nav
    , siteFooter = Just "\xA9 2026 swwstructor demo \xB7 not affiliated with The New York Times"
    , sitePages = [PageSpec "/" "The New York Times \x2014 Breaking News, World News & Multimedia" front]
    , sitePartials = []
    }
  where
    nav =
      [ NavLink "U.S." "/us"
      , NavLink "World" "/world"
      , NavLink "Business" "/business"
      , NavLink "Arts" "/arts"
      , NavLink "Opinion" "/opinion"
      , NavLink "Cooking" "/cooking"
      ]
    front =
      [ Masthead "The New York Times" (Just "Tuesday, June 16, 2026")
      , NavStrip nav True
      , Ribbon
          [ NavLink "LIVE \xB7 War in the Middle East" "/live/war"
          , NavLink "World Cup: France vs. Senegal" "/live/worldcup"
          ]
      , FeatureSplit 0.62 28 mainCol rail
      ]
    mainCol =
      [ (story "After a Bitter Split, European Leaders Play Nice With Trump")
          { storyHref = Just "/world/g7"
          , storyDek = Just "A peace framework with Iran, and hope for cooperation with Ukraine, softened the tone at a Group of 7 gathering in France."
          , storyTimestamp = Just "5 MIN READ"
          , storyImage = Just (img "Leaders at the G7 round table" 1.3 430)
          , storyImageSticky = True
          , storyBody = Just "The shift in mood was striking after months of acrimony, with leaders publicly praising one another's resolve while privately conceding how far apart they remain on trade and security policy heading into a fraught autumn."
          }
      , (story "How Ukraine Uses A.I. to Knock Deadly Russian Drones Out of the Skies")
          { storyHref = Just "/world/ukraine-ai"
          , storyDek = Just "Interceptors show Ukraine's embrace of autonomous technologies trained on immense troves of wartime data."
          , storyTimestamp = Just "5 MIN READ"
          }
      , (story "Ukraine Targets Moscow Oil Facility With Drones")
          {storyHref = Just "/world/moscow-oil", storyTimestamp = Just "2 MIN READ"}
      , (story "The War in Iran Has Permanently Altered the Global Economy")
          { storyKicker = Just "Analysis"
          , storyHref = Just "/business/iran-economy"
          , storyDek = Just "Despite a framework deal, the war has set in motion changes that will be hard to reverse."
          , storyImage = Just (img "Crowds with flags in the street" 0.74 540)
          , storyImageSticky = True
          , storyBody = Just "Energy markets, shipping lanes and fertiliser prices have all moved, and economists say the new normal will outlast any ceasefire that diplomats manage to broker in the coming months."
          }
      , (story "War Hangs Over American Farmers as Fertilizer Prices Rise")
          {storyHref = Just "/business/farmers", storyTimestamp = Just "5 MIN READ"}
      ]
    rail =
      [ (story "Here Are the 2026 James Beard Restaurant Award Winners")
          { storyHref = Just "/food/james-beard"
          , storyDek = Just "Kalaya took the outstanding restaurant award and Michael Tusk took home the outstanding chef honor."
          , storyTimestamp = Just "4 MIN READ"
          , storyImage = Just (img "An awarded chef in the kitchen" 1.4 260)
          }
      , (story "Dakota Johnson Finds a Buyer for Her Los Angeles House")
          {storyHref = Just "/realestate/dakota", storyTimestamp = Just "4 MIN READ"}
      , (story "My Tween Daughter's Friend Is a Mean Girl. Should I Tell Her Mother?")
          {storyHref = Just "/style/mean-girl", storyTimestamp = Just "5 MIN READ"}
      , (story "Iran Found Trump's Bone Spur")
          { storyKicker = Just "Opinion \xB7 Bret Stephens"
          , storyHref = Just "/opinion/bone-spur"
          , storyTimestamp = Just "4 MIN READ"
          }
      , (story "Graham Platner, Jon Ossoff and the New Rules of Political Attention")
          { storyKicker = Just "The Ezra Klein Show"
          , storyHref = Just "/opinion/ezra-klein"
          , storyImage = Just (img "A podcast host at the microphone" 1.5 220)
          }
      ]

-- ---------------------------------------------------------------------------
-- The okashi storefront (commerce: products + Stripe buy buttons)
-- ---------------------------------------------------------------------------

okashiSite :: SiteSpec
okashiSite =
  SiteSpec
    { siteTitle = "Okashi School"
    , siteDescription = "Japanese pastry & wagashi classes, taught the slow way."
    , siteBaseUrl = Just "https://okashi.example"
    , siteTheme = okashiTheme
    , siteNav =
        [ NavLink "Classes" "/#s4-h"
        , NavLink "About" "/#s5-h"
        , NavLink "Visit" "/#s6-h"
        ]
    , siteFooter = Just "Okashi School \xB7 made with swwstructor"
    , sitePages = [PageSpec "/" "Okashi School \x2014 Japanese sweets classes" page]
    , sitePartials = []
    }
  where
    page =
      [ Masthead "Okashi School" Nothing
      , NavStrip
          [ NavLink "Classes" "/#s4-h"
          , NavLink "About" "/#s5-h"
          , NavLink "Visit" "/#s6-h"
          ]
          True
      , Ribbon [NavLink "Now booking: summer wagashi season" "/#s4-h"]
      , Hero
          HeroContent
            { heroKicker = Just "Workshops in Kyoto"
            , heroHeadline = "The quiet craft of Japanese sweets"
            , heroDek = Just "Small hands-on classes in wagashi and everyday okashi."
            , heroBody = Just "Okashi School is a working confectionery studio. You make real sweets with your own hands \x2014 seasonal nerikiri, daifuku, dorayaki \x2014 and take them home in a paper box tied with string."
            , heroImage = Just (img "Seasonal nerikiri on a lacquer tray" 1.5 380)
            , heroCta = Just (Cta "See the classes" "/#s4-h")
            }
      , ProductGrid
          (Just "Classes")
          (Just "Book a seat. Every class is three hours and includes tools, ingredients, and tea.")
          [ Product "Nerikiri Wagashi" "Shape seasonal nerikiri by hand." (Money.price Money.EUR 85) (Just (img "Finished nerikiri" 1.4 220)) (BuyTarget "nerikiri")
          , Product "Strawberry Daifuku" "Pound mochi and wrap sweet beans." (Money.price Money.EUR 65) (Just (img "Strawberry daifuku" 1.2 220)) (BuyTarget "daifuku")
          , Product "Dorayaki & Anko" "Griddle pancakes and simmer anko." (Money.price Money.EUR 70) (Just (img "Dorayaki" 1.5 220)) (BuyTarget "dorayaki")
          , Product "Warabimochi & Kinako" "A cooling summer sweet." (Money.price Money.EUR 60) (Just (img "Warabimochi" 1.3 220)) (BuyTarget "warabimochi")
          ]
      , RichColumns
          (Just "About the studio")
          [ "We are a two-room studio near the river: one room for pounding mochi and simmering bean paste, one quiet room with a long cedar table where the shaping happens. Classes are capped at eight."
          , "Our teacher trained for nine years in a Kyoto wagashi house before opening the school. The recipes are traditional; the pace is gentle; English and Japanese are both spoken."
          ]
      , Contact
          "Visit"
          (Just "Okashi School \xB7 3-12 Kawabata-dori \xB7 open Wednesday to Sunday, 10:00\x2013\&17:00 \xB7 a five-minute walk from the station.")
          (Just "hello@okashi.example")
      ]

main :: IO ()
main = do
  writeFile "sites/nyt/site.json" (encode (siteSpecToJSON nytSite) <> "\n")
  writeFile "sites/okashi/site.json" (encode (siteSpecToJSON okashiSite) <> "\n")
  putStrLn "wrote sites/nyt/site.json and sites/okashi/site.json"
  -- sanity: both must round-trip
  let rt s = siteSpecFromJSON (siteSpecToJSON s) == Right s
  putStrLn ("round-trip nyt=" <> show (rt nytSite) <> " okashi=" <> show (rt okashiSite))
