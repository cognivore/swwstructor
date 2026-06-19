{-# LANGUAGE OverloadedStrings #-}

-- | The constructor's acceptance tests — a hand-rolled harness (no test
-- dependency, so it runs offline against the dev-shell GHC, exactly like the
-- engine's @test\/Spec.hs@). The headline test is the /benchmark/: a New York
-- Times-style front page, built entirely from content data through the template
-- library, that is well-formed and responsive — the same archetype as the
-- engine's @nytFront@ example, but authored, not hand-coded.
module Main (main) where

import Data.List (nub, sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (exitFailure, exitSuccess)

import Swwstructor.Block
  ( Block (..)
  , BuyTarget (BuyTarget)
  , NavLink (NavLink)
  , blockFromJSON
  , blockToJSON
  )
import Swwstructor.Checkout
  ( CheckoutRequest (CheckoutRequest)
  , Url (Url)
  , encodeForm
  , lineItem
  , stripeFormParams
  )
import Swwstructor.Content
import Swwstructor.Edit
  ( addListItem
  , addPage
  , addSection
  , blankSection
  , deletePage
  , deleteSection
  , moveSection
  , pageIndex
  , sectionAt
  , sectionKinds
  , sectionTagOf
  , sectionsOf
  , setSectionAt
  )
import Swwstructor.Money (currencySymbol, formatPrice, price)
import qualified Swwstructor.Money as Money
import Swwstructor.Templates
  ( Section (secBlocks, secLayout)
  , pageSection
  , renderSection
  , resolvePage
  , resolvedSection
  )
import Swwstructor.Theme (nytTheme, themeCss, themeFromJSON, themeToJSON)
import StickyWM
  ( Document
  , Rect (rx)
  , Viewport (Viewport)
  , Win (wSticky, wType)
  , WinType (Figure, Mast, Nav, Strip)
  , build
  , cols
  , ctx0
  , decode
  , encode
  , leafIds
  , leaves
  , solveDoc
  , wId
  , wellFormed
  )

-- ---------------------------------------------------------------------------
-- A tiny test harness
-- ---------------------------------------------------------------------------

check :: String -> Bool -> IO Bool
check name ok = do
  putStrLn ((if ok then "  ok   " else "  FAIL ") <> name)
  pure ok

section :: String -> IO ()
section s = putStrLn ("\n== " <> s <> " ==")

-- ---------------------------------------------------------------------------
-- Fixtures: the NYT-style front (content data only)
-- ---------------------------------------------------------------------------

img :: Text -> Double -> Double -> ImageRef
img cap a c = ImageRef Nothing cap a c

leadStory :: Story
leadStory =
  (story "After a Bitter Split, European Leaders Play Nice With Trump")
    { storyHref = Just "/world/g7"
    , storyDek = Just "A peace framework with Iran, and hope for cooperation with Ukraine, softened the tone at a Group of 7 gathering in France."
    , storyTimestamp = Just "5 MIN READ"
    , storyImage = Just (img "Leaders at the G7 round table" 1.3 430)
    , storyImageSticky = True
    , storyBody = Just "The shift in mood was striking after months of acrimony, with leaders publicly praising one another's resolve while privately conceding how far apart they remain on trade and security."
    }

mainStories :: [Story]
mainStories =
  [ leadStory
  , (story "How Ukraine Uses A.I. to Knock Deadly Russian Drones Out of the Skies")
      { storyHref = Just "/world/ukraine-ai"
      , storyDek = Just "Interceptors show Ukraine's embrace of autonomous technologies trained on immense troves of wartime data."
      , storyTimestamp = Just "5 MIN READ"
      }
  , (story "The War in Iran Has Permanently Altered the Global Economy")
      { storyKicker = Just "Analysis"
      , storyHref = Just "/business/iran-economy"
      , storyDek = Just "The war has set in motion changes that will be hard to reverse."
      , storyImage = Just (img "Crowds with flags" 0.74 540)
      , storyImageSticky = True
      , storyBody = Just "Energy markets, shipping lanes and fertiliser prices have all moved, and economists say the new normal will outlast any ceasefire."
      }
  ]

railStories :: [Story]
railStories =
  [ (story "Here Are the 2026 James Beard Restaurant Award Winners")
      { storyHref = Just "/food/james-beard"
      , storyDek = Just "Kalaya took the outstanding restaurant award and Michael Tusk took home the outstanding chef honor."
      , storyTimestamp = Just "4 MIN READ"
      , storyImage = Just (img "An awarded chef" 1.4 260)
      }
  , (story "Dakota Johnson Finds a Buyer for Her Los Angeles House")
      {storyHref = Just "/realestate/dakota", storyTimestamp = Just "4 MIN READ"}
  , (story "Iran Found Trump's Bone Spur")
      {storyKicker = Just "Opinion", storyHref = Just "/opinion/bone-spur", storyByline = Just "Bret Stephens"}
  ]

nytSite :: SiteSpec
nytSite =
  SiteSpec
    { siteTitle = "The New York Times"
    , siteDescription = "Live news, investigations, opinion, photos and video."
    , siteBaseUrl = Just "https://example-times.test"
    , siteTheme = nytTheme
    , siteNav = nav
    , siteFooter = Just "\xA9 2026 swwstructor demo \xB7 not affiliated with The New York Times"
    , sitePages =
        [ PageSpec "/" "The New York Times — Breaking News" frontSections
        ]
    , sitePartials = []
    }
  where
    nav =
      [ NavLink "U.S." "/us"
      , NavLink "World" "/world"
      , NavLink "Business" "/business"
      , NavLink "Arts" "/arts"
      , NavLink "Opinion" "/opinion"
      ]
    frontSections =
      [ Masthead "The New York Times" (Just "Tuesday, June 16, 2026")
      , NavStrip nav True
      , Ribbon [NavLink "LIVE War in the Middle East" "/live/war", NavLink "World Cup: France vs. Senegal" "/live/worldcup"]
      , FeatureSplit 0.62 28 mainStories railStories
      ]

firstPageOf :: SiteSpec -> PageSpec
firstPageOf s = case sitePages s of
  (p : _) -> p
  [] -> PageSpec "/" "empty" []

frontDoc :: Document
frontDoc = secLayout (pageSection (firstPageOf nytSite))

-- A kitchen-sink site exercising every section kind, for the codec round-trip.
sinkSite :: SiteSpec
sinkSite =
  nytSite
    { sitePages =
        [ PageSpec
            "/everything"
            "Everything"
            [ Masthead "Title" Nothing
            , NavStrip [NavLink "A" "/a"] False
            , Ribbon [NavLink "R" "/r"]
            , Hero (HeroContent (Just "KICK") "Hero headline" (Just "dek") (Just "body") (Just (img "hero" 1.5 360)) (Just (Cta "Go" "/go")))
            , FeatureSplit 0.6 24 [leadStory] [story "rail"]
            , StoryRow [story "one", story "two", story "three"]
            , ProductGrid (Just "Shop") (Just "intro") [demoProduct]
            , RichColumns (Just "About") ["left column text", "right column text"]
            , Gallery (Just "Gallery") [img "g1" 1.5 240, img "g2" 1.2 240]
            , CtaBand "Subscribe" (Just "for 50p a week") (Just (Cta "Subscribe" "/sub"))
            , Contact "Visit" (Just "come by") (Just "hello@example.test")
            , ProseSection (Just "Note") (Just "Heading") "Some prose."
            , FooterBand "the end"
            ]
        ]
    }

demoProduct :: Product
demoProduct = Product "Demo Widget" "A demo product." (price Money.EUR 85) (Just (img "a product photo" 1.4 220)) (BuyTarget "demo-widget")

-- ---------------------------------------------------------------------------
-- Predicates over the layout
-- ---------------------------------------------------------------------------

wf :: Document -> Viewport -> Double -> Bool
wf doc vp w = wellFormed (build doc) vp w

leafTypes :: Document -> [WinType]
leafTypes = map wType . leaves

hasStickyFigure :: Document -> Bool
hasStickyFigure d = any (\w -> wType w == Figure && wSticky w /= Nothing) (leaves d)

-- The "looks like a newspaper front" archetype: masthead + nav + strip chrome,
-- a 2-column split, and a figure that pins within its story.
looksLikeFront :: Document -> Bool
looksLikeFront d =
  let ts = leafTypes d
   in Mast `elem` ts
        && Nav `elem` ts
        && Strip `elem` ts
        && hasSplit2Top d
        && hasStickyFigure d

-- Whether the top-level document contains a split with at least two columns.
-- We test this robustly by solving: at a wide viewport a multi-column front has
-- placements at x > 0; a single column has them all at x == 0.
hasSplit2Top :: Document -> Bool
hasSplit2Top d =
  let ps = solveDoc (ctx0 wide) d 1116
   in any ((> 0) . rx . snd) ps

wide :: Viewport
wide = Viewport 1180 900

phone :: Viewport
phone = Viewport 390 844

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

nytBenchmark :: IO [Bool]
nytBenchmark = do
  section "NYT-style front benchmark (built from content data)"
  let wWide = 1116
      wPhone = 326
      psWide = solveDoc (ctx0 wide) frontDoc wWide
      psPhone = solveDoc (ctx0 phone) frontDoc wPhone
      idsWide = sort (map (wId . fst) psWide)
      idsPhone = sort (map (wId . fst) psPhone)
      docIds = sort (leafIds frontDoc)
      blockKeys = sort (M.keys (secBlocks (pageSection (firstPageOf nytSite))))
  sequence
    [ check "front is well-formed at 1180px" (wf frontDoc wide wWide)
    , check "front is well-formed at 390px" (wf frontDoc phone wPhone)
    , check "desktop is two columns (some boxes at x>0)" (any ((> 0) . rx . snd) psWide)
    , check "phone re-homes to one column (all boxes at x==0)" (all ((== 0) . rx . snd) psPhone)
    , check "cols == 2 at desktop, 1 at phone" (cols wide == 2 && cols phone == 1)
    , check "conservation: same window set at both widths" (idsWide == idsPhone && idsWide == docIds)
    , check "every leaf id has a content block" (blockKeys == docIds)
    , check "ids are unique" (length docIds == length (nub docIds))
    , check "has a masthead, nav and strip" (all (`elem` leafTypes frontDoc) [Mast, Nav, Strip])
    , check "lead figure pins within its story (sticky figure present)" (hasStickyFigure frontDoc)
    , check "same archetype as the engine's nytFront example" (looksLikeFront frontDoc)
    ]

-- ---------------------------------------------------------------------------

templateWf :: IO [Bool]
templateWf = do
  section "every section kind is well-formed and uniquely-ided"
  let specs = pageSections (firstPageOf sinkSite)
      one i spec =
        let s = renderSection i spec
            d = secLayout s
            ids = leafIds d
            okWf = wf d wide 1116 && wf d phone 326
            okIds = length ids == length (nub ids)
            okBlocks = sort ids == sort (M.keys (secBlocks s))
         in check ("section " <> show i <> " (" <> take 16 (show spec) <> "\x2026): wf + unique ids + blocks") (okWf && okIds && okBlocks)
  sequence (zipWith one [0 ..] specs)

codecRoundTrip :: IO [Bool]
codecRoundTrip = do
  section "content codec round-trips (decode . encode == id)"
  let viaValue s = siteSpecFromJSON (siteSpecToJSON s) == Right s
      viaText s = case decode (encode (siteSpecToJSON s)) of
        Right j -> siteSpecFromJSON j == Right s
        Left _ -> False
  sequence
    [ check "nyt site round-trips (value)" (viaValue nytSite)
    , check "nyt site round-trips (json text)" (viaText nytSite)
    , check "kitchen-sink site round-trips (value)" (viaValue sinkSite)
    , check "kitchen-sink site round-trips (json text)" (viaText sinkSite)
    ]

blockRoundTrip :: IO [Bool]
blockRoundTrip = do
  section "every block kind round-trips"
  let blocks =
        [ BHeading "h"
        , BHeadingLink "h" "/x"
        , BSubhead "s"
        , BKicker "k"
        , BParagraph "p"
        , BByline "b"
        , BTimestamp "t"
        , BNavBar [NavLink "a" "/a"]
        , BBrandMark "m"
        , BFigureBox "cap" (Just "/i.jpg")
        , BFigureBox "cap" Nothing
        , BPriceTag (price Money.EUR 85)
        , BBuyButton (BuyTarget "x") "Buy"
        , BLinkText "l" "/l"
        , BRuleLine
        ]
      ok b = blockFromJSON (blockToJSON b) == Right b
  sequence [check ("block " <> take 18 (show b)) (ok b) | b <- blocks]

themeTests :: IO [Bool]
themeTests = do
  section "theme interpreter"
  let css = themeCss nytTheme
  sequence
    [ check "theme round-trips" (themeFromJSON (themeToJSON nytTheme) == Right nytTheme)
    , check "css has a :root block" (":root" `T.isInfixOf` css)
    , check "css carries the foreground colour" ("#121212" `T.isInfixOf` css)
    , check "css uses sww- (not ok-) class names" ("sww-heading" `T.isInfixOf` css && not ("ok-heading" `T.isInfixOf` css))
    ]

checkoutTests :: IO [Bool]
checkoutTests = do
  section "stripe form encoding (pure)"
  let req =
        CheckoutRequest
          (lineItem "Demo Widget" (price Money.EUR 85) :| [])
          (Url "https://x.test/success")
          (Url "https://x.test/cancel")
      params = stripeFormParams req
      look k = lookup k params
      body = encodeForm [("a b", "c&d")]
  sequence
    [ check "mode is payment" (look "mode" == Just "payment")
    , check "unit_amount is minor units (8500)" (look "line_items[0][price_data][unit_amount]" == Just "8500")
    , check "currency is eur" (look "line_items[0][price_data][currency]" == Just "eur")
    , check "quantity is 1" (look "line_items[0][quantity]" == Just "1")
    , check "form encoding escapes & and space" (body == "a+b=c%26d")
    ]

moneyTests :: IO [Bool]
moneyTests = do
  section "money rendering"
  sequence
    [ check "EUR 85 renders €85" (formatPrice (price Money.EUR 85) == currencySymbol Money.EUR <> "85")
    , check "GBP 12.50 renders £12.50" (formatPrice (Money.cents Money.GBP 1250) == "\xA3" <> "12.50")
    , check "JPY 600 has no decimals" (formatPrice (Money.cents Money.JPY 600) == "\xA5" <> "600")
    ]

editTests :: IO [Bool]
editTests = do
  section "content editing (the constructor admin's model)"
  let home = "/"
      uniqueIds d = let i = leafIds d in length i == length (nub i)
      okWf s = let d = secLayout (pageSection (firstPageOf s)) in wf d wide 1116 && wf d phone 326 && uniqueIds d
      addedHero = addSection home "hero" nytSite
      moved = moveSection home 3 (-1) nytSite
      del0 = deleteSection home 0 nytSite
      withPage = addPage "/about" "About" nytSite
      fs = maybe (StoryRow []) id (sectionAt home 3 nytSite)
      moreMain = setSectionAt home 3 (addListItem "main" fs) nytSite
      blankOk tag = case blankSection tag of
        Just sec -> let d = secLayout (renderSection 0 sec) in wf d wide 1116 && wf d phone 326 && uniqueIds d
        Nothing -> False
  sequence $
    [ check "addSection hero: +1 section, still wf + unique" (length (sectionsOf home addedHero) == length (sectionsOf home nytSite) + 1 && okWf addedHero)
    , check "moveSection up: count conserved, still wf" (length (sectionsOf home moved) == length (sectionsOf home nytSite) && okWf moved)
    , check "deleteSection: one fewer, still wf" (length (sectionsOf home del0) == length (sectionsOf home nytSite) - 1 && okWf del0)
    , check "addPage then deletePage" (pageIndex "/about" withPage == Just 1 && pageIndex "/about" (deletePage "/about" withPage) == Nothing)
    , check "addListItem 'main': featureSplit gains a story, still wf" (okWf moreMain)
    , check "edited site round-trips through the codec" (siteSpecFromJSON (siteSpecToJSON addedHero) == Right addedHero)
    ]
      <> [ check ("blank section '" <> T.unpack tag <> "' is valid (wf @1180/390 + unique ids)") (blankOk tag)
         | (tag, _) <- sectionKinds
         ]

-- A site exercising the template engine: header/footer partials auto-applied, a
-- reusable "shop" partial included on a page, and a deliberate partial cycle.
tplSite :: SiteSpec
tplSite =
  nytSite
    { sitePartials =
        [ Partial "header" [Masthead "The Times" Nothing, NavStrip [NavLink "U.S." "/us"] True]
        , Partial "footer" [FooterBand "\xA9 2026"]
        , Partial "shop" [ProductGrid (Just "Shop") Nothing [Product "Widget" "A widget." (price Money.EUR 9) Nothing (BuyTarget "widget")]]
        , Partial "loopA" [IncludePartial "loopB"]
        , Partial "loopB" [IncludePartial "loopA"]
        ]
    , sitePages =
        [ PageSpec "/" "Home" [IncludePartial "shop", ProseSection Nothing (Just "Body") "hello"]
        , PageSpec "/cyc" "Cycle" [IncludePartial "loopA"]
        ]
    }

templateEngineTests :: IO [Bool]
templateEngineTests = do
  section "template engine (partials: header/footer + include + cycles)"
  let home = PageSpec "/" "Home" [IncludePartial "shop", ProseSection Nothing (Just "Body") "hello"]
      cyc = PageSpec "/cyc" "Cycle" [IncludePartial "loopA"]
      resolved = resolvePage tplSite home
      resolvedCyc = resolvePage tplSite cyc
      tagFirst xs = case xs of (s : _) -> sectionTagOf s; _ -> ""
      tagLast xs = tagFirst (reverse xs)
      docOf pg = secLayout (resolvedSection tplSite pg)
      uniqueIds d = let i = leafIds d in length i == length (nub i)
      okWf d = wf d wide 1116 && wf d phone 326 && uniqueIds d
      buys = [t | (BuyTarget t, _, _) <- pricedItems tplSite]
  sequence
    [ check "header partial is auto-prepended (first section is the masthead)" (tagFirst resolved == "masthead")
    , check "footer partial is auto-appended (last section is the footer)" (tagLast resolved == "footer")
    , check "IncludePartial 'shop' expands to its productGrid" (any ((== "productGrid") . sectionTagOf) resolved)
    , check "resolved page = header(2) + body(2) + footer(1) = 5 sections" (length resolved == 5)
    , check "no IncludePartial survives resolution" (not (any ((== "include") . sectionTagOf) resolved))
    , check "resolved page is well-formed + unique ids @1180/390" (okWf (docOf home))
    , check "a partial cycle terminates (loopA<->loopB expands to nothing)" (length resolvedCyc == 3 && okWf (docOf cyc))
    , check "products defined in a partial reach the buy registry" ("widget" `elem` buys)
    ]

main :: IO ()
main = do
  results <-
    concat
      <$> sequence
        [ nytBenchmark
        , templateWf
        , editTests
        , templateEngineTests
        , codecRoundTrip
        , blockRoundTrip
        , themeTests
        , checkoutTests
        , moneyTests
        ]
  let passed = length (filter id results)
      total = length results
  putStrLn ("\n" <> show passed <> "/" <> show total <> " PASS")
  if passed == total then exitSuccess else exitFailure
