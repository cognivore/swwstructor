{-# LANGUAGE OverloadedStrings #-}

-- | Content-as-data: the schema an owner authors (in JSON or YAML) and the codec
-- that reads it. A page is literally @[SectionSpec]@; a site bundles a 'Theme',
-- a nav, and a set of 'PageSpec's. Nothing here knows about geometry or HTML —
-- 'Swwstructor.Templates' turns each 'SectionSpec' into a placed, rendered
-- 'Swwstructor.Templates.Section'. Adding a section to a page is a content edit,
-- never a code edit (the constructor's definition of done).
--
-- The codec is built on @StickyWM.Json@ (zero extra dependencies, so this module
-- stays in the pure, offline-testable core). It round-trips:
-- @decode . encode == id@ for any value (optional fields are omitted when
-- 'Nothing', and read back as 'Nothing').
module Swwstructor.Content
  ( -- * Leaf content values
    ImageRef (..)
  , imageRef
  , Cta (..)
  , Product (..)
  , Story (..)
  , story

    -- * Sections, pages, sites
  , HeroContent (..)
  , SectionSpec (..)
  , PageSpec (..)
  , Partial (..)
  , SiteSpec (..)

    -- * Derived
  , pricedItems
  , allSectionsOf

    -- * JSON codec
  , imageRefToJSON
  , imageRefFromJSON
  , sectionSpecToJSON
  , sectionSpecFromJSON
  , pageSpecToJSON
  , pageSpecFromJSON
  , partialToJSON
  , partialFromJSON
  , siteSpecToJSON
  , siteSpecFromJSON
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Swwstructor.Block (BuyTarget (BuyTarget), NavLink (NavLink), navHref, navLabel)
import Swwstructor.Money (Cents (Cents), Currency, Price (Price), centsInt, currencyCode, currencyOfCode)
import Swwstructor.Theme (Theme, defaultTheme, themeFromJSON, themeToJSON)
import StickyWM (JSON (..), jarr, jnum, jstr, (.:), (.:?))

-- ---------------------------------------------------------------------------
-- Leaf values
-- ---------------------------------------------------------------------------

-- | A figure: an optional real image source, a caption, and the engine's two
-- sizing knobs (aspect = w\/h, cap = max rendered height). The cap is
-- load-bearing — an uncapped aspect dominates its column.
data ImageRef = ImageRef
  { imgSrc :: !(Maybe Text)
  , imgCaption :: !Text
  , imgAspect :: !Double
  , imgCap :: !Double
  }
  deriving (Eq, Show)

-- | A caption-only figure with sensible defaults (3:2, capped at 360px).
imageRef :: Text -> ImageRef
imageRef cap = ImageRef Nothing cap 1.5 360

-- | A call-to-action link.
data Cta = Cta
  { ctaLabel :: !Text
  , ctaHref :: !Text
  }
  deriving (Eq, Show)

-- | A sellable item. 'prodBuy' is the stable id a buy button posts to; the
-- server resolves it to this priced line item (the amount is never client-set).
data Product = Product
  { prodName :: !Text
  , prodBlurb :: !Text
  , prodPrice :: !Price
  , prodImage :: !(Maybe ImageRef)
  , prodBuy :: !BuyTarget
  }
  deriving (Eq, Show)

-- | An article atom: the editorial unit of an NYT-style front. Everything but
-- the headline is optional, so a one-line link and a full lead story are the
-- same shape at different fill levels.
data Story = Story
  { storyKicker :: !(Maybe Text)
  , storyHeadline :: !Text
  , storyHref :: !(Maybe Text)
  , storyDek :: !(Maybe Text)
  , storyByline :: !(Maybe Text)
  , storyTimestamp :: !(Maybe Text)
  , storyImage :: !(Maybe ImageRef)
  , storyImageSticky :: !Bool
  -- ^ pin the image while its story's text is read (the paper's figure-pinning
  -- case, bounded by the story = its container)
  , storyBody :: !(Maybe Text)
  }
  deriving (Eq, Show)

-- | A headline-only story.
story :: Text -> Story
story h = Story Nothing h Nothing Nothing Nothing Nothing Nothing False Nothing

-- ---------------------------------------------------------------------------
-- Sections
-- ---------------------------------------------------------------------------

-- | The hero band's content (a record, since it has six optional-ish fields).
data HeroContent = HeroContent
  { heroKicker :: !(Maybe Text)
  , heroHeadline :: !Text
  , heroDek :: !(Maybe Text)
  , heroBody :: !(Maybe Text)
  , heroImage :: !(Maybe ImageRef)
  , heroCta :: !(Maybe Cta)
  }
  deriving (Eq, Show)

-- | The closed set of section kinds. Each maps to a template in
-- 'Swwstructor.Templates'. This is the vocabulary an owner writes.
data SectionSpec
  = -- | centered wordmark + optional tagline (the masthead)
    Masthead !Text !(Maybe Text)
  | -- | a nav bar; 'Bool' = pin it to the top for the whole page
    NavStrip ![NavLink] !Bool
  | -- | a thin link ribbon (the breaking-news strip)
    Ribbon ![NavLink]
  | -- | a hero: copy column + figure rail
    Hero !HeroContent
  | -- | the two-column front: rho, gutter, main stories, rail stories
    FeatureSplit !Double !Double ![Story] ![Story]
  | -- | an equal-weight row of stories (re-homes to one column on phone)
    StoryRow ![Story]
  | -- | a product grid: optional title, optional intro, the catalogue
    ProductGrid !(Maybe Text) !(Maybe Text) ![Product]
  | -- | N prose columns under an optional title
    RichColumns !(Maybe Text) ![Text]
  | -- | an image gallery under an optional title
    Gallery !(Maybe Text) ![ImageRef]
  | -- | a call-to-action band: headline, optional text, optional button
    CtaBand !Text !(Maybe Text) !(Maybe Cta)
  | -- | contact: title, optional body, optional email
    Contact !Text !(Maybe Text) !(Maybe Text)
  | -- | a generic prose block: optional kicker, optional headline, body
    ProseSection !(Maybe Text) !(Maybe Text) !Text
  | -- | a footer band
    FooterBand !Text
  | -- | include a named partial here — the template engine's reuse primitive
    IncludePartial !Text
  deriving (Eq, Show)

-- | A page: its path (@\"/\"@, @\"/classes\"@…), a title, and its sections.
data PageSpec = PageSpec
  { pagePath :: !Text
  , pageTitle :: !Text
  , pageSections :: ![SectionSpec]
  }
  deriving (Eq, Show)

-- | A named, reusable group of sections — the template engine's /partial/.
-- A page pulls one in with 'IncludePartial'; partials named 'headerPartial' /
-- 'footerPartial' are auto-applied to every page (see 'Swwstructor.Templates').
data Partial = Partial
  { partialName :: !Text
  , partialSections :: ![SectionSpec]
  }
  deriving (Eq, Show)

-- | A whole site: metadata, theme, shared nav, footer, pages, and the reusable
-- partials of the template engine.
data SiteSpec = SiteSpec
  { siteTitle :: !Text
  , siteDescription :: !Text
  , siteBaseUrl :: !(Maybe Text)
  , siteTheme :: !Theme
  , siteNav :: ![NavLink]
  , siteFooter :: !(Maybe Text)
  , sitePages :: ![PageSpec]
  , sitePartials :: ![Partial]
  }
  deriving (Eq, Show)

-- | Every priced, buyable item across the whole site, as
-- @(buy-target, display-name, price)@. The server folds this into the buy
-- registry so a checkout amount is always server-resolved.
pricedItems :: SiteSpec -> [(BuyTarget, Text, Price)]
pricedItems site =
  [ (prodBuy p, prodName p, prodPrice p)
  | sec <- allSectionsOf site
  , p <- productsOf sec
  ]
  where
    productsOf (ProductGrid _ _ ps) = ps
    productsOf _ = []

-- | Every section across the site — pages AND partials — so products defined in
-- a partial (then included on pages) still resolve in the buy registry.
allSectionsOf :: SiteSpec -> [SectionSpec]
allSectionsOf site =
  concatMap pageSections (sitePages site) <> concatMap partialSections (sitePartials site)

-- ---------------------------------------------------------------------------
-- JSON helpers
-- ---------------------------------------------------------------------------

jS :: Text -> JSON
jS = JStr . T.unpack

jText :: JSON -> Either String Text
jText j = T.pack <$> jstr j

reqText :: JSON -> String -> Either String Text
reqText v k = jText =<< v .: k

optText :: JSON -> String -> Maybe Text
optText v k = case v .:? k of
  Just (JStr s) -> Just (T.pack s)
  _ -> Nothing

optDouble :: JSON -> String -> Maybe Double
optDouble v k = case v .:? k of
  Just (JNum n) -> Just n
  _ -> Nothing

optBool :: JSON -> String -> Bool
optBool v k = case v .:? k of
  Just (JBool b) -> b
  _ -> False

-- | Build an object, dropping fields whose value is omitted.
obj :: [[(String, JSON)]] -> JSON
obj = JObj . concat

req :: String -> JSON -> [(String, JSON)]
req k j = [(k, j)]

optF :: String -> Maybe a -> (a -> JSON) -> [(String, JSON)]
optF _ Nothing _ = []
optF k (Just x) enc = [(k, enc x)]

arrOf :: (a -> JSON) -> [a] -> JSON
arrOf enc = JArr . map enc

decArr :: (JSON -> Either String a) -> JSON -> Either String [a]
decArr dec j = mapM dec =<< jarr j

decArrAt :: (JSON -> Either String a) -> JSON -> String -> Either String [a]
decArrAt dec v k = case v .:? k of
  Just j -> decArr dec j
  Nothing -> Right []

-- ---------------------------------------------------------------------------
-- Leaf codecs
-- ---------------------------------------------------------------------------

navLinkJSON :: NavLink -> JSON
navLinkJSON l = JObj [("label", jS (navLabel l)), ("href", jS (navHref l))]

navLinkOf :: JSON -> Either String NavLink
navLinkOf v = NavLink <$> reqText v "label" <*> reqText v "href"

priceJSON :: Price -> JSON
priceJSON (Price c cur) =
  JObj [("amount", JNum (fromIntegral (centsInt c))), ("currency", jS (currencyCode cur))]

priceOf :: JSON -> Either String Price
priceOf v = do
  amount <- jnum =<< v .: "amount"
  code <- reqText v "currency"
  cur <- maybe (Left ("unknown currency: " ++ T.unpack code)) Right (currencyOfCode code) :: Either String Currency
  pure (Price (Cents (clampAmount amount)) cur)

-- | A checkout amount must be a sane, non-negative integer of minor units. Clamp
-- defensively (the registry feeds this straight to Stripe).
clampAmount :: Double -> Int
clampAmount d
  | isNaN d || d < 0 = 0
  | d > 1.0e9 = 1000000000
  | otherwise = round d

ctaJSON :: Cta -> JSON
ctaJSON (Cta l h) = JObj [("label", jS l), ("href", jS h)]

ctaOf :: JSON -> Either String Cta
ctaOf v = Cta <$> reqText v "label" <*> reqText v "href"

imageRefToJSON :: ImageRef -> JSON
imageRefToJSON (ImageRef src cap a c) =
  obj
    [ optF "src" src jS
    , req "caption" (jS cap)
    , req "aspect" (JNum a)
    , req "cap" (JNum c)
    ]

imageRefFromJSON :: JSON -> Either String ImageRef
imageRefFromJSON v = do
  cap <- reqText v "caption"
  let src = optText v "src"
      a = maybe 1.5 id (optDouble v "aspect")
      c = maybe 360 id (optDouble v "cap")
  pure (ImageRef src cap a c)

productToJSON :: Product -> JSON
productToJSON (Product name blurb pr img (BuyTarget tgt)) =
  obj
    [ req "name" (jS name)
    , req "blurb" (jS blurb)
    , req "price" (priceJSON pr)
    , optF "image" img imageRefToJSON
    , req "buy" (jS tgt)
    ]

productFromJSON :: JSON -> Either String Product
productFromJSON v = do
  name <- reqText v "name"
  blurb <- reqText v "blurb"
  pr <- priceOf =<< v .: "price"
  img <- optImage v "image"
  tgt <- reqText v "buy"
  pure (Product name blurb pr img (BuyTarget tgt))

optImage :: JSON -> String -> Either String (Maybe ImageRef)
optImage v k = case v .:? k of
  Just j -> Just <$> imageRefFromJSON j
  Nothing -> Right Nothing

storyToJSON :: Story -> JSON
storyToJSON s =
  obj
    [ optF "kicker" (storyKicker s) jS
    , req "headline" (jS (storyHeadline s))
    , optF "href" (storyHref s) jS
    , optF "dek" (storyDek s) jS
    , optF "byline" (storyByline s) jS
    , optF "timestamp" (storyTimestamp s) jS
    , optF "image" (storyImage s) imageRefToJSON
    , if storyImageSticky s then req "imageSticky" (JBool True) else []
    , optF "body" (storyBody s) jS
    ]

storyFromJSON :: JSON -> Either String Story
storyFromJSON v = do
  h <- reqText v "headline"
  img <- optImage v "image"
  pure
    Story
      { storyKicker = optText v "kicker"
      , storyHeadline = h
      , storyHref = optText v "href"
      , storyDek = optText v "dek"
      , storyByline = optText v "byline"
      , storyTimestamp = optText v "timestamp"
      , storyImage = img
      , storyImageSticky = optBool v "imageSticky"
      , storyBody = optText v "body"
      }

-- ---------------------------------------------------------------------------
-- Section codec
-- ---------------------------------------------------------------------------

sectionSpecToJSON :: SectionSpec -> JSON
sectionSpecToJSON spec = case spec of
  Masthead title tag ->
    obj [tag' "masthead", req "title" (jS title), optF "tagline" tag jS]
  NavStrip links sticky ->
    obj [tag' "navStrip", req "links" (arrOf navLinkJSON links), req "sticky" (JBool sticky)]
  Ribbon links ->
    obj [tag' "ribbon", req "links" (arrOf navLinkJSON links)]
  Hero h ->
    obj
      [ tag' "hero"
      , optF "kicker" (heroKicker h) jS
      , req "headline" (jS (heroHeadline h))
      , optF "dek" (heroDek h) jS
      , optF "body" (heroBody h) jS
      , optF "image" (heroImage h) imageRefToJSON
      , optF "cta" (heroCta h) ctaJSON
      ]
  FeatureSplit rho gut mainCol rail ->
    obj
      [ tag' "featureSplit"
      , req "rho" (JNum rho)
      , req "gutter" (JNum gut)
      , req "main" (arrOf storyToJSON mainCol)
      , req "rail" (arrOf storyToJSON rail)
      ]
  StoryRow stories ->
    obj [tag' "storyRow", req "stories" (arrOf storyToJSON stories)]
  ProductGrid title intro products ->
    obj
      [ tag' "productGrid"
      , optF "title" title jS
      , optF "intro" intro jS
      , req "products" (arrOf productToJSON products)
      ]
  RichColumns title cols ->
    obj [tag' "richColumns", optF "title" title jS, req "columns" (arrOf jS cols)]
  Gallery title imgs ->
    obj [tag' "gallery", optF "title" title jS, req "images" (arrOf imageRefToJSON imgs)]
  CtaBand headline body cta ->
    obj [tag' "ctaBand", req "headline" (jS headline), optF "body" body jS, optF "cta" cta ctaJSON]
  Contact title body email ->
    obj [tag' "contact", req "title" (jS title), optF "body" body jS, optF "email" email jS]
  ProseSection kick headline body ->
    obj [tag' "prose", optF "kicker" kick jS, optF "headline" headline jS, req "body" (jS body)]
  FooterBand t ->
    obj [tag' "footer", req "text" (jS t)]
  IncludePartial name ->
    obj [tag' "include", req "partial" (jS name)]
  where
    tag' k = req "section" (jS (T.pack k))

sectionSpecFromJSON :: JSON -> Either String SectionSpec
sectionSpecFromJSON v = do
  k <- jstr =<< v .: "section"
  case k of
    "masthead" -> Masthead <$> reqText v "title" <*> pure (optText v "tagline")
    "navStrip" -> do
      links <- decArrAt navLinkOf v "links"
      pure (NavStrip links (optBool v "sticky"))
    "ribbon" -> Ribbon <$> decArrAt navLinkOf v "links"
    "hero" -> do
      headline <- reqText v "headline"
      img <- optImage v "image"
      cta <- optCta v "cta"
      pure
        ( Hero
            HeroContent
              { heroKicker = optText v "kicker"
              , heroHeadline = headline
              , heroDek = optText v "dek"
              , heroBody = optText v "body"
              , heroImage = img
              , heroCta = cta
              }
        )
    "featureSplit" -> do
      let rho = maybe 0.62 id (optDouble v "rho")
          gut = maybe 28 id (optDouble v "gutter")
      mainCol <- decArrAt storyFromJSON v "main"
      rail <- decArrAt storyFromJSON v "rail"
      pure (FeatureSplit rho gut mainCol rail)
    "storyRow" -> StoryRow <$> decArrAt storyFromJSON v "stories"
    "productGrid" -> do
      products <- decArrAt productFromJSON v "products"
      pure (ProductGrid (optText v "title") (optText v "intro") products)
    "richColumns" -> do
      cols <- decArrAt jText v "columns"
      pure (RichColumns (optText v "title") cols)
    "gallery" -> do
      imgs <- decArrAt imageRefFromJSON v "images"
      pure (Gallery (optText v "title") imgs)
    "ctaBand" -> do
      headline <- reqText v "headline"
      cta <- optCta v "cta"
      pure (CtaBand headline (optText v "body") cta)
    "contact" -> do
      title <- reqText v "title"
      pure (Contact title (optText v "body") (optText v "email"))
    "prose" -> do
      body <- reqText v "body"
      pure (ProseSection (optText v "kicker") (optText v "headline") body)
    "footer" -> FooterBand <$> reqText v "text"
    "include" -> IncludePartial <$> reqText v "partial"
    other -> Left ("unknown section kind: " ++ other)

partialToJSON :: Partial -> JSON
partialToJSON (Partial name secs) =
  JObj [("name", jS name), ("sections", arrOf sectionSpecToJSON secs)]

partialFromJSON :: JSON -> Either String Partial
partialFromJSON v = do
  name <- reqText v "name"
  secs <- decArrAt sectionSpecFromJSON v "sections"
  pure (Partial name secs)

optCta :: JSON -> String -> Either String (Maybe Cta)
optCta v k = case v .:? k of
  Just j -> Just <$> ctaOf j
  Nothing -> Right Nothing

-- ---------------------------------------------------------------------------
-- Page + site codec
-- ---------------------------------------------------------------------------

pageSpecToJSON :: PageSpec -> JSON
pageSpecToJSON (PageSpec path title secs) =
  JObj
    [ ("path", jS path)
    , ("title", jS title)
    , ("sections", arrOf sectionSpecToJSON secs)
    ]

pageSpecFromJSON :: JSON -> Either String PageSpec
pageSpecFromJSON v = do
  path <- reqText v "path"
  title <- reqText v "title"
  secs <- decArrAt sectionSpecFromJSON v "sections"
  pure (PageSpec path title secs)

siteSpecToJSON :: SiteSpec -> JSON
siteSpecToJSON s =
  obj
    [ req "title" (jS (siteTitle s))
    , req "description" (jS (siteDescription s))
    , optF "baseUrl" (siteBaseUrl s) jS
    , req "theme" (themeToJSON (siteTheme s))
    , req "nav" (arrOf navLinkJSON (siteNav s))
    , optF "footer" (siteFooter s) jS
    , req "pages" (arrOf pageSpecToJSON (sitePages s))
    , req "partials" (arrOf partialToJSON (sitePartials s))
    ]

siteSpecFromJSON :: JSON -> Either String SiteSpec
siteSpecFromJSON v = do
  title <- reqText v "title"
  th <- case v .:? "theme" of
    Just j -> themeFromJSON j
    Nothing -> Right defaultTheme
  nav <- decArrAt navLinkOf v "nav"
  pages <- decArrAt pageSpecFromJSON v "pages"
  partials <- decArrAt partialFromJSON v "partials"
  pure
    SiteSpec
      { siteTitle = title
      , siteDescription = maybe "" id (optText v "description")
      , siteBaseUrl = optText v "baseUrl"
      , siteTheme = th
      , siteNav = nav
      , siteFooter = optText v "footer"
      , sitePages = pages
      , sitePartials = partials
      }
