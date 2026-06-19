{-# LANGUAGE OverloadedStrings #-}

-- | The content DSL — a final-tagless algebra of the /things that render inside
-- a window/, orthogonal to the layout algebra (which only places and sizes).
-- Generalised from a reference content DSL: in addition to the storefront blocks
-- (@priceTag@, @buyButton@) it carries the editorial blocks an NYT-style front
-- needs (@kicker@, @byline@, @timestamp@, @headingLink@, @ruleLine@).
--
-- As in the reference app there are three coordinated pieces:
--
--   * 'BlockSym' — the surface syntax (the typeclass),
--   * 'Block' — its /initial encoding/ (the free term, what content-as-data
--     decodes to), with @instance BlockSym Block@ and 'buildBlock' the
--     initial→final transform (mirroring @StickyWM.build@), and
--   * interpreters 'HtmlBlock' (SSR) and 'PlainBlock' (text / accessibility /
--     tests).
--
-- A 'Block' also has a JSON codec ('blockToJSON' / 'blockFromJSON') so a page is
-- literally data: @[{ template, content }]@.
module Swwstructor.Block
  ( -- * Auxiliary content values
    NavLink (..)
  , navLink
  , BuyTarget (..)

    -- * The algebra
  , BlockSym (..)

    -- * Initial encoding
  , Block (..)
  , buildBlock

    -- * Interpreters
  , HtmlBlock (..)
  , PlainBlock (..)

    -- * JSON codec
  , blockToJSON
  , blockFromJSON
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Swwstructor.Html
  ( Html
  , elAttr
  , esc
  , htmlConcat
  )
import Swwstructor.Money
  ( Cents (Cents)
  , Currency
  , Price (Price)
  , centsInt
  , currencyCode
  , currencyOfCode
  , formatPrice
  )
import StickyWM
  ( JSON (..)
  , jnum
  , jstr
  , (.:)
  , (.:?)
  )

-- ---------------------------------------------------------------------------
-- Auxiliary values
-- ---------------------------------------------------------------------------

-- | A navigation entry: visible label and target href.
data NavLink = NavLink
  { navLabel :: !Text
  , navHref :: !Text
  }
  deriving (Eq, Show)

navLink :: Text -> Text -> NavLink
navLink = NavLink

-- | The id a buy button posts to. The server resolves it to a priced line item
-- (so the amount is never client-supplied). A strong newtype, never a bare
-- 'Text'.
newtype BuyTarget = BuyTarget Text
  deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- The algebra
-- ---------------------------------------------------------------------------

-- | Content surface syntax. Every constructor is a /kind of thing that can sit
-- inside a placed window/; none of them know anything about geometry.
class BlockSym repr where
  -- | A section/article headline (rendered as a heading).
  heading :: Text -> repr

  -- | A headline that is also a link (headline text, href). NYT headlines link.
  headingLink :: Text -> Text -> repr

  -- | A standfirst / dek under a headline.
  subhead :: Text -> repr

  -- | A small label above a headline — @ANALYSIS@, @LIVE@, a section kicker.
  kicker :: Text -> repr

  -- | Body prose.
  paragraph :: Text -> repr

  -- | A byline — @"By Jane Doe"@.
  byline :: Text -> repr

  -- | A timestamp / read-length chip — @"5 MIN READ"@, @"8m ago"@.
  timestamp :: Text -> repr

  -- | A horizontal navigation bar.
  navBar :: [NavLink] -> repr

  -- | The site wordmark / masthead title.
  brandMark :: Text -> repr

  -- | An image placeholder carrying its caption (a real @\<img>@ when a source
  -- url is supplied, else a captioned gradient box).
  figureBox :: Text -> Maybe Text -> repr

  -- | A commerce price.
  priceTag :: Price -> repr

  -- | A commerce call to action: a POST form to @\/buy\/<target>@.
  buyButton :: BuyTarget -> Text -> repr

  -- | A standalone inline link (label, href).
  linkText :: Text -> Text -> repr

  -- | A hairline rule — a section divider.
  ruleLine :: repr

-- ---------------------------------------------------------------------------
-- Initial encoding
-- ---------------------------------------------------------------------------

-- | The free 'BlockSym' term: what a content file decodes into, and what the
-- renderer stores per window id.
data Block
  = BHeading !Text
  | BHeadingLink !Text !Text
  | BSubhead !Text
  | BKicker !Text
  | BParagraph !Text
  | BByline !Text
  | BTimestamp !Text
  | BNavBar ![NavLink]
  | BBrandMark !Text
  | BFigureBox !Text !(Maybe Text)
  | BPriceTag !Price
  | BBuyButton !BuyTarget !Text
  | BLinkText !Text !Text
  | BRuleLine
  deriving (Eq, Show)

instance BlockSym Block where
  heading = BHeading
  headingLink = BHeadingLink
  subhead = BSubhead
  kicker = BKicker
  paragraph = BParagraph
  byline = BByline
  timestamp = BTimestamp
  navBar = BNavBar
  brandMark = BBrandMark
  figureBox = BFigureBox
  priceTag = BPriceTag
  buyButton = BBuyButton
  linkText = BLinkText
  ruleLine = BRuleLine

-- | The initial→final transform: interpret a stored 'Block' into any
-- 'BlockSym'. Mirrors @StickyWM.build@ for the layout algebra.
buildBlock :: (BlockSym repr) => Block -> repr
buildBlock (BHeading t) = heading t
buildBlock (BHeadingLink t h) = headingLink t h
buildBlock (BSubhead t) = subhead t
buildBlock (BKicker t) = kicker t
buildBlock (BParagraph t) = paragraph t
buildBlock (BByline t) = byline t
buildBlock (BTimestamp t) = timestamp t
buildBlock (BNavBar ls) = navBar ls
buildBlock (BBrandMark t) = brandMark t
buildBlock (BFigureBox c s) = figureBox c s
buildBlock (BPriceTag p) = priceTag p
buildBlock (BBuyButton tgt l) = buyButton tgt l
buildBlock (BLinkText l h) = linkText l h
buildBlock BRuleLine = ruleLine

-- ---------------------------------------------------------------------------
-- HTML interpreter (SSR)
-- ---------------------------------------------------------------------------

-- | The server-side rendering interpreter. CSS classes are stable (@sww-*@) so
-- the 'Swwstructor.Theme' stylesheet — and only it — controls appearance.
newtype HtmlBlock = HtmlBlock {runHtmlBlock :: Html}

instance BlockSym HtmlBlock where
  heading t = HtmlBlock $ elAttr "h2" [("class", "sww-heading")] (esc t)
  headingLink t h =
    HtmlBlock $
      elAttr "h2" [("class", "sww-heading")] $
        elAttr "a" [("class", "sww-headlink"), ("href", safeHref h)] (esc t)
  subhead t = HtmlBlock $ elAttr "p" [("class", "sww-subhead")] (esc t)
  kicker t = HtmlBlock $ elAttr "span" [("class", "sww-kicker")] (esc t)
  paragraph t = HtmlBlock $ elAttr "p" [("class", "sww-paragraph")] (esc t)
  byline t = HtmlBlock $ elAttr "p" [("class", "sww-byline")] (esc t)
  timestamp t = HtmlBlock $ elAttr "span" [("class", "sww-timestamp")] (esc t)
  brandMark t =
    HtmlBlock $
      elAttr "div" [("class", "sww-brand")] $
        elAttr "a" [("href", "/"), ("class", "sww-brandlink")] (esc t)
  navBar links =
    HtmlBlock $
      elAttr "nav" [("class", "sww-nav")] $
        htmlConcat
          [ elAttr "a" [("class", "sww-navlink"), ("href", safeHref (navHref l))] (esc (navLabel l))
          | l <- links
          ]
  figureBox cap src =
    HtmlBlock $ case src of
      Just url ->
        elAttr "div" [("class", "sww-figure sww-figure-img")] $
          elAttr "img" [("class", "sww-img"), ("src", safeHref url), ("alt", cap), ("loading", "lazy")] mempty
            <> figcap cap
      Nothing ->
        elAttr "div" [("class", "sww-figure"), ("role", "img"), ("aria-label", cap)] (figcap cap)
    where
      figcap c
        | T.null c = mempty
        | otherwise = elAttr "span" [("class", "sww-figcap")] (esc c)
  priceTag p = HtmlBlock $ elAttr "span" [("class", "sww-price")] (esc (formatPrice p))
  buyButton (BuyTarget tgt) lbl =
    HtmlBlock $
      elAttr
        "form"
        [ ("class", "sww-buyform")
        , ("method", "post")
        , ("action", "/buy/" <> tgt)
        ]
        (elAttr "button" [("class", "sww-buy"), ("type", "submit")] (esc lbl))
  linkText lbl h =
    HtmlBlock $ elAttr "a" [("class", "sww-link"), ("href", safeHref h)] (esc lbl)
  ruleLine = HtmlBlock $ elAttr "hr" [("class", "sww-rule")] mempty

-- | Neutralise dangerous URL schemes in owner/adapter-supplied hrefs and image
-- sources. Whitespace/control characters are stripped for the check (browsers
-- ignore them in schemes), so @"java\\tscript:"@ and case tricks are caught.
-- Defence in depth — the 'Adapter.UAL' path can ingest external content.
safeHref :: Text -> Text
safeHref h =
  let probe = T.toLower (T.filter (> ' ') h)
   in if any (`T.isPrefixOf` probe) ["javascript:", "vbscript:", "data:"]
        then "#"
        else h

-- ---------------------------------------------------------------------------
-- Plain interpreter (text / accessibility / tests)
-- ---------------------------------------------------------------------------

-- | A plain-text interpreter — useful in tests (no HTML to parse) and as the
-- basis of an accessible text dump.
newtype PlainBlock = PlainBlock {runPlainBlock :: Text}

instance BlockSym PlainBlock where
  heading = PlainBlock
  headingLink t _ = PlainBlock t
  subhead = PlainBlock
  kicker t = PlainBlock (T.toUpper t)
  paragraph = PlainBlock
  byline = PlainBlock
  timestamp = PlainBlock
  navBar links = PlainBlock (T.intercalate " \xB7 " (map navLabel links))
  brandMark = PlainBlock
  figureBox cap _ = PlainBlock ("(image: " <> cap <> ")")
  priceTag p = PlainBlock (formatPrice p)
  buyButton _ lbl = PlainBlock ("[" <> lbl <> "]")
  linkText lbl _ = PlainBlock lbl
  ruleLine = PlainBlock "\x2014"

-- ---------------------------------------------------------------------------
-- JSON codec (content is data)
-- ---------------------------------------------------------------------------

-- | Encode a 'Block' to the wire JSON (tagged by @"b"@).
blockToJSON :: Block -> JSON
blockToJSON b = case b of
  BHeading t -> tagged "heading" [("text", JStr (T.unpack t))]
  BHeadingLink t h -> tagged "headingLink" [("text", JStr (T.unpack t)), ("href", JStr (T.unpack h))]
  BSubhead t -> tagged "subhead" [("text", JStr (T.unpack t))]
  BKicker t -> tagged "kicker" [("text", JStr (T.unpack t))]
  BParagraph t -> tagged "paragraph" [("text", JStr (T.unpack t))]
  BByline t -> tagged "byline" [("text", JStr (T.unpack t))]
  BTimestamp t -> tagged "timestamp" [("text", JStr (T.unpack t))]
  BNavBar ls -> tagged "navBar" [("links", JArr (map navLinkToJSON ls))]
  BBrandMark t -> tagged "brandMark" [("text", JStr (T.unpack t))]
  BFigureBox c s -> tagged "figureBox" (("caption", JStr (T.unpack c)) : maybe [] (\u -> [("src", JStr (T.unpack u))]) s)
  BPriceTag p -> tagged "priceTag" [("price", priceToJSON p)]
  BBuyButton (BuyTarget tgt) l -> tagged "buyButton" [("target", JStr (T.unpack tgt)), ("label", JStr (T.unpack l))]
  BLinkText l h -> tagged "linkText" [("label", JStr (T.unpack l)), ("href", JStr (T.unpack h))]
  BRuleLine -> tagged "ruleLine" []
  where
    tagged k extra = JObj (("b", JStr k) : extra)

-- | Decode a 'Block' from wire JSON.
blockFromJSON :: JSON -> Either String Block
blockFromJSON v = do
  k <- jstr =<< v .: "b"
  case k of
    "heading" -> BHeading <$> txt "text"
    "headingLink" -> BHeadingLink <$> txt "text" <*> txt "href"
    "subhead" -> BSubhead <$> txt "text"
    "kicker" -> BKicker <$> txt "text"
    "paragraph" -> BParagraph <$> txt "text"
    "byline" -> BByline <$> txt "text"
    "timestamp" -> BTimestamp <$> txt "text"
    "navBar" -> do
      arr <- jarr' =<< v .: "links"
      BNavBar <$> mapM navLinkFromJSON arr
    "brandMark" -> BBrandMark <$> txt "text"
    "figureBox" -> do
      c <- txt "caption"
      let s = case v .:? "src" of
            Just (JStr u) -> Just (T.pack u)
            _ -> Nothing
      pure (BFigureBox c s)
    "priceTag" -> BPriceTag <$> (priceFromJSON =<< v .: "price")
    "buyButton" -> BBuyButton . BuyTarget <$> txt "target" <*> txt "label"
    "linkText" -> BLinkText <$> txt "label" <*> txt "href"
    "ruleLine" -> Right BRuleLine
    other -> Left ("unknown block kind: " ++ other)
  where
    txt key = T.pack <$> (jstr =<< v .: key)
    jarr' (JArr xs) = Right xs
    jarr' _ = Left "expected array"

navLinkToJSON :: NavLink -> JSON
navLinkToJSON (NavLink l h) = JObj [("label", JStr (T.unpack l)), ("href", JStr (T.unpack h))]

navLinkFromJSON :: JSON -> Either String NavLink
navLinkFromJSON v = do
  l <- jstr =<< v .: "label"
  h <- jstr =<< v .: "href"
  pure (NavLink (T.pack l) (T.pack h))

priceToJSON :: Price -> JSON
priceToJSON (Price c cur) =
  JObj [("amount", JNum (fromIntegral (centsInt c))), ("currency", JStr (T.unpack (currencyCode cur)))]

priceFromJSON :: JSON -> Either String Price
priceFromJSON v = do
  amount <- jnum =<< v .: "amount"
  code <- jstr =<< v .: "currency"
  cur <- maybe (Left ("unknown currency: " ++ code)) Right (currencyOfCode (T.pack code)) :: Either String Currency
  let n
        | isNaN amount || amount < 0 = 0
        | amount > 1.0e9 = 1000000000
        | otherwise = round amount
  pure (Price (Cents n) cur)
