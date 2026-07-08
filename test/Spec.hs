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
  , PlainBlock (runPlainBlock)
  , blockFromJSON
  , blockToJSON
  , buildBlock
  , fmtSlotDay
  )
import Swwstructor.Html (renderHtml)
import Swwstructor.Site (renderStage)
import Swwstructor.Checkout
  ( CheckoutItem (ItemAdHoc, ItemPrice)
  , CheckoutRequest (CheckoutRequest)
  , Url (Url)
  , encodeForm
  , lineItem
  , stripeFormParams
  )
import Swwstructor.Plugins.StripeCatalog
  ( CatalogItem (..)
  , catalogSection
  , isPriceId
  , parseCatalogJSON
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
import Swwstructor.Media
  ( MediaKind (Avif, Gif, Jpeg, Png, Svg, Webp)
  , contentTypeFor
  , isHeicBytes
  , isSafeMediaName
  , mediaFileName
  , sanitizeBaseName
  , sniffKind
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
import Swwstructor.Theme (nytTheme, sanitizeFontUrl, themeCss, themeFromJSON, themeToJSON)

import qualified Data.ByteString as BS
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
            , ScheduleClass (Just "Schedule your class") (Just "Pick a date.") demoSlots
            , FooterBand "the end"
            ]
        ]
    }

-- Week 1 / week 2 areas, one slot already full. 'slotTaken' MUST be 0 here:
-- the content codec never persists it, so a non-zero value would (rightly)
-- break the round-trip test.
demoSlots :: [SlotSpec]
demoSlots =
  [ SlotSpec "2026-09-01" "11:00" "13:00" "Islington / Finsbury Park" 6 0
  , SlotSpec "2026-09-01" "18:00" "20:00" "Islington / Finsbury Park" 6 0
  , SlotSpec "2026-09-08" "11:00" "13:00" "Marylebone / Clerkenwell" 6 0
  ]

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
        , BEmailReveal
        , BScheduleForm [SlotSpec "2026-09-01" "11:00" "13:00" "Islington" 6 3]
        , BRuleLine
        ]
      ok b = blockFromJSON (blockToJSON b) == Right b
  sequence [check ("block " <> take 18 (show b)) (ok b) | b <- blocks]

-- The email-protection property: an address configured on a contact section
-- must never reach the rendered page — only the reveal widget may.
emailProtectionTests :: IO [Bool]
emailProtectionTests = do
  section "email protection (address never rendered)"
  let addr = "secret-address@example.test"
      sec = renderSection 0 (Contact "Get in touch" (Just "Write to me.") (Just addr))
      html = renderHtml (renderStage nytTheme sec (Viewport 1180 900))
  sequence
    [ check "address absent from the rendered page" (not (addr `T.isInfixOf` html))
    , check "no mailto: carries an address (only the script constant)" (T.count "mailto:" html == T.count "mailto:'" html)
    , check "the reveal widget is present" ("data-sww-reveal" `T.isInfixOf` html)
    , check "the reveal client is present" ("/email-challenge" `T.isInfixOf` html)
    , check "plain interpreter never leaks either" (not (addr `T.isInfixOf` runPlainBlock (buildBlock BEmailReveal)))
    ]

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
          (ItemAdHoc (lineItem "Demo Widget" (price Money.EUR 85)) :| [])
          (Url "https://x.test/success")
          (Url "https://x.test/cancel")
          [("sww_product", "Demo Widget")]
      params = stripeFormParams req
      look k = lookup k params
      reqP =
        CheckoutRequest
          (ItemPrice "price_1ABCdef" (Money.Quantity 1) :| [])
          (Url "https://x.test/success")
          (Url "https://x.test/cancel")
          []
      paramsP = stripeFormParams reqP
      lookP k = lookup k paramsP
      body = encodeForm [("a b", "c&d")]
  sequence
    [ check "mode is payment" (look "mode" == Just "payment")
    , check "unit_amount is minor units (8500)" (look "line_items[0][price_data][unit_amount]" == Just "8500")
    , check "currency is eur" (look "line_items[0][price_data][currency]" == Just "eur")
    , check "quantity is 1" (look "line_items[0][quantity]" == Just "1")
    , check "session metadata is encoded" (look "metadata[sww_product]" == Just "Demo Widget")
    , check "a price-id item encodes line_items[0][price]" (lookP "line_items[0][price]" == Just "price_1ABCdef")
    , check "a price-id item sends no price_data" (lookP "line_items[0][price_data][unit_amount]" == Nothing)
    , check "no metadata params when none given" (all (\(k, _) -> not ("metadata[" `T.isPrefixOf` k)) paramsP)
    , check "form encoding escapes & and space" (body == "a+b=c%26d")
    ]

catalogTests :: IO [Bool]
catalogTests = do
  section "stripe catalogue (single source of truth: parse + section)"
  let body =
        "{\"object\":\"list\",\"data\":[\
        \{\"id\":\"prod_A\",\"name\":\"Individual Lesson\",\"description\":\"One-to-one class.\",\
        \\"images\":[\"https://files.stripe.com/links/abc\"],\
        \\"default_price\":{\"id\":\"price_indiv\",\"type\":\"one_time\",\"unit_amount\":24000,\"currency\":\"gbp\"}},\
        \{\"id\":\"prod_B\",\"name\":\"Group Class: Single Ticket\",\"images\":[],\
        \\"default_price\":{\"id\":\"price_single\",\"type\":\"one_time\",\"unit_amount\":8000,\"currency\":\"gbp\"}},\
        \{\"id\":\"prod_S\",\"name\":\"Subscription Thing\",\"images\":[],\
        \\"default_price\":{\"id\":\"price_sub\",\"type\":\"recurring\",\"unit_amount\":1000,\"currency\":\"gbp\"}},\
        \{\"id\":\"prod_C\",\"name\":\"No Price\",\"images\":[]}\
        \]}"
      parsed = parseCatalogJSON body
  case parsed of
    Left e -> do
      _ <- check ("catalog parses: " <> e) False
      pure [False]
    Right items -> do
      let sec = catalogSection (Just "Book a class") Nothing items
          d = secLayout (renderSection 0 sec)
          uniqueIds doc = let i = leafIds doc in length i == length (nub i)
      sequence
        [ check "unpriced + recurring products are skipped (2 of 4)" (length items == 2)
        , check "sorted cheapest first" (map ciPriceId items == ["price_single", "price_indiv"])
        , check "amount + currency parsed" (fmap (\(CatalogItem _ _ _ _ p) -> p) (listToMaybe' items) == Just (Money.cents Money.GBP 8000))
        , check "image url survives" (ciImage (last items) == Just "https://files.stripe.com/links/abc")
        , check "buy targets are price ids" (all (isPriceId . ciPriceId) items)
        , check "catalogue section is a wf grid @1180/390 + unique ids" (wf d wide 1116 && wf d phone 326 && uniqueIds d)
        , check "stripeCatalog codec round-trips" (siteSpecFromJSON (siteSpecToJSON withCat) == Right withCat)
        ]
  where
    listToMaybe' (x : _) = Just x
    listToMaybe' [] = Nothing
    withCat =
      nytSite
        { sitePages = [PageSpec "/" "Home" [StripeCatalog (Just "Book") (Just "Pick a format.")]]
        }

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

fontUrlTests :: IO [Bool]
fontUrlTests = do
  section "webfont url sanitisation (a URL is not a CSS value)"
  let google = "https://fonts.googleapis.com/css2?family=Cormorant+Garamond:wght@500;600;700&family=Inter:wght@400;500&display=swap"
  sequence
    [ check "a Google Fonts href survives untouched (:?=&+;@ intact)" (sanitizeFontUrl google == Just google)
    , check "http is allowed" (sanitizeFontUrl "http://x.test/a?b=c" == Just "http://x.test/a?b=c")
    , check "javascript: is refused" (sanitizeFontUrl "javascript:alert(1)" == Nothing)
    , check "protocol-relative is refused" (sanitizeFontUrl "//evil.test/x.css" == Nothing)
    , check "quotes and angle brackets are dropped" (sanitizeFontUrl "https://x.test/\"><script>" == Just "https://x.test/script")
    , check "surrounding whitespace is tolerated" (sanitizeFontUrl "  https://x.test/f.css  " == Just "https://x.test/f.css")
    ]

mediaTests :: IO [Bool]
mediaTests = do
  section "media library (pure core: sniffing + names)"
  let jpeg = BS.pack [0xFF, 0xD8, 0xFF, 0xE0] <> "....JFIF"
      png = BS.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A] <> "IHDR"
      gif = "GIF89a......"
      webp = "RIFF" <> BS.pack [0, 0, 0, 0] <> "WEBPVP8 "
      svg = "<?xml version=\"1.0\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\"></svg>"
      exe = "MZ\x90\x00stub"
      avif = BS.pack [0, 0, 0, 32] <> "ftypavif" <> BS.pack [0, 0, 0, 0] <> "avifmif1" <> BS.replicate 8 0
      avifMajorMif1 = BS.pack [0, 0, 0, 32] <> "ftypmif1" <> BS.pack [0, 0, 0, 0] <> "miafavif" <> BS.replicate 8 0
      heic = BS.pack [0, 0, 0, 24] <> "ftypheic" <> BS.pack [0, 0, 0, 0] <> "mif1heic"
  sequence
    [ check "sniffs jpeg" (sniffKind jpeg == Just Jpeg)
    , check "sniffs png" (sniffKind png == Just Png)
    , check "sniffs gif" (sniffKind gif == Just Gif)
    , check "sniffs webp" (sniffKind webp == Just Webp)
    , check "sniffs svg" (sniffKind svg == Just Svg)
    , check "sniffs avif (major brand)" (sniffKind avif == Just Avif)
    , check "sniffs avif (compat brand)" (sniffKind avifMajorMif1 == Just Avif)
    , check "heic is not storable but is transcodable" (sniffKind heic == Nothing && isHeicBytes heic)
    , check "avif stored name gets .avif" (mediaFileName "Tofu Bou.avif" "a1b2c3d4e5f6" Avif == "tofu-bou-a1b2c3d4e5f6.avif")
    , check "refuses an executable" (sniffKind exe == Nothing && not (isHeicBytes exe))
    , check "base name is slugged" (sanitizeBaseName "Emilie's Photo (1).JPG" == "emilie-s-photo-1")
    , check "empty base name falls back" (sanitizeBaseName "....jpg" == "img")
    , check "stored name is slug-hash.ext" (mediaFileName "My Pic.PNG" "a1b2c3d4e5f6" Png == "my-pic-a1b2c3d4e5f6.png")
    , check "safe names accept generated ones" (isSafeMediaName "my-pic-a1b2c3d4e5f6.png")
    , check "safe names refuse traversal" (not (isSafeMediaName "../secrets.enc"))
    , check "safe names refuse separators" (not (isSafeMediaName "a/b.png"))
    , check "safe names refuse dotfiles" (not (isSafeMediaName ".hidden.png"))
    , check "content type follows the extension" (contentTypeFor "x.webp" == Just "image/webp" && contentTypeFor "x.bin" == Nothing)
    ]

-- The schedule-a-class section: grouping, availability, and the no-JS form.
scheduleTests :: IO [Bool]
scheduleTests = do
  section "schedule a class (ticket redemption form)"
  let slots =
        [ SlotSpec "2026-09-01" "11:00" "13:00" "Islington / Finsbury Park" 6 0
        , SlotSpec "2026-09-02" "18:00" "20:00" "Islington / Finsbury Park" 6 6 -- full
        , SlotSpec "2026-09-08" "11:00" "13:00" "Marylebone / Clerkenwell" 6 5
        ]
      sec = renderSection 0 (ScheduleClass (Just "Schedule") Nothing slots)
      html = renderHtml (renderStage nytTheme sec (Viewport 1180 900))
      persisted = sectionSpecToJSON (ScheduleClass Nothing Nothing [SlotSpec "2026-09-01" "11:00" "13:00" "A" 6 3])
  sequence
    [ check "form posts to /schedule" ("action=\"/schedule\"" `T.isInfixOf` html)
    , check "slots become radios with stable ids" ("value=\"2026-09-01-1100\"" `T.isInfixOf` html)
    , check "one fieldset per area" (T.count "sww-schedweek" html >= 2)
    , check "a full slot is disabled and marked" ("disabled" `T.isInfixOf` html && "fully booked" `T.isInfixOf` html && "sww-slot-full" `T.isInfixOf` html)
    , check "scarcity shows (1 place left)" ("1 place left" `T.isInfixOf` html)
    , check "the venue-announcement note is present" ("about a week before" `T.isInfixOf` html)
    , check "ticket code + email + name fields present" (all (`T.isInfixOf` html) ["name=\"code\"", "name=\"email\"", "name=\"name\""])
    , check "no client JS in the widget (form-post-and-navigate)" (not ("<script" `T.isInfixOf` html))
    , check "day formatting (Sakamoto)" (fmtSlotDay "2026-09-01" == "Tue 1 Sep" && fmtSlotDay "2026-09-08" == "Tue 8 Sep" && fmtSlotDay "2026-09-13" == "Sun 13 Sep")
    , check "slot ids strip the colon" (slotIdOf (SlotSpec "2026-09-01" "11:00" "13:00" "A" 6 0) == "2026-09-01-1100")
    , check "grouping preserves first-appearance order" (map fst (groupSlotsByArea slots) == ["Islington / Finsbury Park", "Marylebone / Clerkenwell"])
    , check "live counts are NOT persisted by the content codec" (sectionSpecFromJSON persisted == Right (ScheduleClass Nothing Nothing [SlotSpec "2026-09-01" "11:00" "13:00" "A" 6 0]))
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
        , emailProtectionTests
        , themeTests
        , fontUrlTests
        , mediaTests
        , checkoutTests
        , catalogTests
        , scheduleTests
        , moneyTests
        ]
  let passed = length (filter id results)
      total = length results
  putStrLn ("\n" <> show passed <> "/" <> show total <> " PASS")
  if passed == total then exitSuccess else exitFailure
