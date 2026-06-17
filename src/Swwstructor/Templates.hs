{-# LANGUAGE OverloadedStrings #-}

-- | The template library: pure functions @SectionSpec -> Section@ that turn
-- owner content into a /placed, rendered/ unit. A 'Section' is exactly the pair
-- the reference app uses — a layout 'Document' (sizing\/placement) and a content
-- map keyed by window id (what renders inside each placed window). The renderer
-- ('Swwstructor.Site') later solves the layout and, per placed window, looks up
-- and runs its 'Block'.
--
-- This is the generalisation of @Okashi.Page@: the same @classColumn@ /
-- @splitN@ patterns, but driven by data. Two invariants every template upholds,
-- because the engine's well-formedness and conservation theorems depend on them:
--
--   1. every leaf id is unique (window ids are namespaced by section index and,
--      within a section, by a stable suffix), and
--   2. every leaf has a content block (ids and content cannot drift — both come
--      from the same 'Win' via 'leafWith').
module Swwstructor.Templates
  ( Section (..)
  , renderSection
  , renderSections
  , pageSection
  , emptySection
    -- * The template engine: resolve partials/header/footer into a flat page
  , resolvePage
  , resolvedSection
  , expandSection
  , headerPartial
  , footerPartial
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Swwstructor.Block
  ( Block
  , brandMark
  , buyButton
  , byline
  , figureBox
  , heading
  , headingLink
  , kicker
  , linkText
  , navBar
  , paragraph
  , priceTag
  , subhead
  , timestamp
  )
import Swwstructor.Content
  ( Cta (Cta)
  , HeroContent (..)
  , ImageRef (..)
  , PageSpec (pageSections)
  , Partial (partialName, partialSections)
  , Product (..)
  , SectionSpec (..)
  , SiteSpec (sitePartials)
  , Story (..)
  )
import StickyWM
  ( Document (..)
  , LayoutSym (..)
  , Win (..)
  , WinType (Custom, Mast, Nav, Strip)
  , chrome
  , figure
  , headline
  , leafIds
  , prose
  , split2
  , stickyTo
  )

-- | A page section: its layout fragment and the content for the windows it
-- names. Mirrors @Okashi.Page.Section@.
data Section = Section
  { secLayout :: !Document
  , secBlocks :: !(Map String Block)
  }

emptySection :: Section
emptySection = Section (stack []) M.empty

-- A leaf paired with its content block, so layout id and content key cannot
-- drift apart: both come from the same 'Win'.
type Part = (Document, (String, Block))

leafWith :: Win -> Block -> Part
leafWith w b = (leaf w, (wId w, b))

sectionOf :: [Part] -> Section
sectionOf parts =
  Section
    { secLayout = stack (map fst parts)
    , secBlocks = M.fromList (map snd parts)
    }

mconcatSections :: [Section] -> Section
mconcatSections secs =
  Section
    { secLayout = stack (map secLayout secs)
    , secBlocks = M.unions (map secBlocks secs)
    }

-- Approximate a character count for an analytic measure (real heights are
-- measured in the browser on pass 2). Mirrors @T.length@ usage in okashi.
chars :: Text -> Int
chars = max 1 . T.length

-- ---------------------------------------------------------------------------
-- Page assembly
-- ---------------------------------------------------------------------------

-- | Render a flat list of sections into one 'Section'. Each section is
-- namespaced by its index, then all are stacked. Finally, any "always sticky"
-- nav (a sticky 'Nav' window left with empty refs by 'renderSection') has its
-- container set to the last window, so it pins for the whole page — the @okashi@
-- nav pattern, made automatic. Because ids are keyed by the FLAT index, the same
-- partial included twice yields distinct ids (conservation holds).
renderSections :: [SectionSpec] -> Section
renderSections specs =
  let secs = zipWith renderSection [0 ..] specs
      combined = mconcatSections secs
      ids = leafIds (secLayout combined)
      lastId = if null ids then "" else last ids
   in combined {secLayout = mapLeaves (fixupNav lastId) (secLayout combined)}

-- | Render a page's own sections (no template resolution).
pageSection :: PageSpec -> Section
pageSection pg = renderSections (pageSections pg)

-- ---------------------------------------------------------------------------
-- The template engine: header/footer + partial includes
-- ---------------------------------------------------------------------------

-- | Partials with these names are auto-applied to every page (Zola's base
-- template, visually): @"header"@ is prepended, @"footer"@ appended.
headerPartial, footerPartial :: Text
headerPartial = "header"
footerPartial = "footer"

-- | Resolve a page into the flat section list the engine actually lays out:
-- the @header@ partial, then the page body (with every 'IncludePartial'
-- expanded), then the @footer@ partial. This is the whole "template engine" —
-- a pure expansion of shared definitions, exactly like a template's includes.
resolvePage :: SiteSpec -> PageSpec -> [SectionSpec]
resolvePage site pg =
  expandList (named headerPartial)
    <> expandList (pageSections pg)
    <> expandList (named footerPartial)
  where
    named nm = maybe [] partialSections (findPartial site nm)
    expandList = concatMap (expandSection site [])

-- | A fully-resolved page, ready to render.
resolvedSection :: SiteSpec -> PageSpec -> Section
resolvedSection site pg = renderSections (resolvePage site pg)

findPartial :: SiteSpec -> Text -> Maybe Partial
findPartial site nm = listToMaybe [p | p <- sitePartials site, partialName p == nm]

-- | Expand one section. 'IncludePartial' becomes the partial's sections,
-- recursively — cycle-guarded by the chain of names currently being expanded
-- (so a partial that includes itself, directly or via others, just stops).
expandSection :: SiteSpec -> [Text] -> SectionSpec -> [SectionSpec]
expandSection site seen sec = case sec of
  IncludePartial nm
    | nm `elem` seen -> []
    | otherwise -> case findPartial site nm of
        Just p -> concatMap (expandSection site (nm : seen)) (partialSections p)
        Nothing -> []
  _ -> [sec]

mapLeaves :: (Win -> Win) -> Document -> Document
mapLeaves f (DLeaf w) = DLeaf (f w)
mapLeaves f (DStack ds) = DStack (map (mapLeaves f) ds)
mapLeaves f (DSplitN g cs) = DSplitN g [(r, mapLeaves f d) | (r, d) <- cs]

-- A sticky nav with empty refs is the "container is the whole page" marker; bind
-- it to the page bottom so its release point is the end of the page.
fixupNav :: String -> Win -> Win
fixupNav lastId w
  | wType w == Nav && wSticky w /= Nothing && null (wRefs w) && not (null lastId) =
      w {wRefs = [lastId]}
  | otherwise = w

-- ---------------------------------------------------------------------------
-- The dispatcher
-- ---------------------------------------------------------------------------

-- | Render one section, namespacing its window ids with the section index.
renderSection :: Int -> SectionSpec -> Section
renderSection i spec = case spec of
  Masthead title tag ->
    sectionOf $
      leafWith (chrome Mast (pfx <> "-mast") 90) (brandMark title)
        : maybe [] (\t -> [leafWith (chrome Strip (pfx <> "-tag") 22) (kicker t)]) tag
  NavStrip links sticky ->
    let navWin0 = chrome Nav (pfx <> "-nav") 32
        navWin = if sticky then stickyTo 0 [] navWin0 else navWin0
     in sectionOf [leafWith navWin (navBar links)]
  Ribbon links ->
    sectionOf [leafWith (chrome Strip (pfx <> "-ribbon") 28) (navBar links)]
  Hero h -> heroSection pfx h
  FeatureSplit rho gut mainCol rail ->
    let mainStack = stack (concatMap (\(j, s) -> map fst (storyParts (pfx <> "-m" <> show j) s)) (idx mainCol))
        railStack = stack (concatMap (\(j, s) -> map fst (storyParts (pfx <> "-r" <> show j) s)) (idx rail))
        blocks =
          M.fromList $
            concatMap (\(j, s) -> map snd (storyParts (pfx <> "-m" <> show j) s)) (idx mainCol)
              <> concatMap (\(j, s) -> map snd (storyParts (pfx <> "-r" <> show j) s)) (idx rail)
     in Section {secLayout = split2 rho gut mainStack railStack, secBlocks = blocks}
  StoryRow stories ->
    let cols =
          [ (1, stack (map fst (storyParts (pfx <> "-c" <> show j) s)))
          | (j, s) <- idx stories
          ]
        blocks = M.fromList (concatMap (\(j, s) -> map snd (storyParts (pfx <> "-c" <> show j) s)) (idx stories))
     in Section {secLayout = splitN 24 cols, secBlocks = blocks}
  ProductGrid title intro products -> productGridSection pfx title intro products
  RichColumns title cols -> richColumnsSection pfx title cols
  Gallery title imgs -> gallerySection pfx title imgs
  CtaBand h body cta -> ctaSection pfx h body cta
  Contact title body email -> contactSection pfx title body email
  ProseSection kick h body -> proseSectionT pfx kick h body
  FooterBand t ->
    sectionOf [leafWith (chrome (Custom "footer") (pfx <> "-footer") 60) (paragraph t)]
  IncludePartial _ ->
    -- Resolved away by 'resolvePage' before rendering; nothing to place here.
    emptySection
  where
    pfx = "s" <> show i
    idx = zip [(0 :: Int) ..]

-- ---------------------------------------------------------------------------
-- Story atom
-- ---------------------------------------------------------------------------

-- | The editorial atom: an optional figure (sticky within the story if asked),
-- a kicker, a (possibly linked) headline, a dek, a byline, a timestamp, and a
-- body — in reading order, every present part a leaf with its block.
storyParts :: String -> Story -> [Part]
storyParts pfx s =
  let kickP = (\t -> leafWith (chrome (Custom "kicker") (pfx <> "-kick") 18) (kicker t)) <$> storyKicker s
      headBlock = case storyHref s of
        Just h -> headingLink (storyHeadline s) h
        Nothing -> heading (storyHeadline s)
      headP = leafWith (headline (pfx <> "-head") (chars (storyHeadline s))) headBlock
      dekP = (\t -> leafWith (prose (pfx <> "-dek") (chars t)) (subhead t)) <$> storyDek s
      byP = (\t -> leafWith (chrome (Custom "byline") (pfx <> "-by") 18) (byline t)) <$> storyByline s
      tsP = (\t -> leafWith (chrome (Custom "timestamp") (pfx <> "-ts") 18) (timestamp t)) <$> storyTimestamp s
      bodyP = (\t -> leafWith (prose (pfx <> "-body") (chars t)) (paragraph t)) <$> storyBody s
      textParts = catMaybes [kickP, Just headP, dekP, byP, tsP, bodyP]
      textIds = [i | (_, (i, _)) <- textParts]
      imgP =
        ( \ir ->
            let base = figure (pfx <> "-img") (imgAspect ir) (imgCap ir)
                win =
                  if storyImageSticky s && not (null textIds)
                    then stickyTo 12 textIds base
                    else base
             in leafWith win (figureBox (imgCaption ir) (imgSrc ir))
        )
          <$> storyImage s
   in catMaybes [kickP, Just headP, dekP, imgP, byP, tsP, bodyP]

-- ---------------------------------------------------------------------------
-- The remaining templates
-- ---------------------------------------------------------------------------

heroSection :: String -> HeroContent -> Section
heroSection pfx h =
  let copyParts =
        catMaybes
          [ (\t -> leafWith (chrome (Custom "kicker") (pfx <> "-kick") 18) (kicker t)) <$> heroKicker h
          , Just (leafWith (headline (pfx <> "-h") (chars (heroHeadline h))) (heading (heroHeadline h)))
          , (\t -> leafWith (prose (pfx <> "-sub") (chars t)) (subhead t)) <$> heroDek h
          , (\t -> leafWith (prose (pfx <> "-body") (chars t)) (paragraph t)) <$> heroBody h
          , (\(Cta l u) -> leafWith (chrome (Custom "cta") (pfx <> "-cta") 44) (linkText l u)) <$> heroCta h
          ]
      copyIds = [i | (_, (i, _)) <- copyParts]
      figParts =
        ( \ir ->
            let base = figure (pfx <> "-fig") (imgAspect ir) (max 360 (imgCap ir))
             in [leafWith (stickyTo 48 copyIds base) (figureBox (imgCaption ir) (imgSrc ir))]
        )
          `maybe'` heroImage h
      layout = case figParts of
        [] -> stack (map fst copyParts)
        _ -> split2 0.58 28 (stack (map fst copyParts)) (stack (map fst figParts))
      blocks = M.fromList (map snd copyParts <> map snd figParts)
   in Section {secLayout = layout, secBlocks = blocks}
  where
    maybe' f = maybe [] f

productGridSection :: String -> Maybe Text -> Maybe Text -> [Product] -> Section
productGridSection pfx title intro products =
  let introParts =
        catMaybes
          [ (\t -> leafWith (headline (pfx <> "-h") (chars t)) (heading t)) <$> title
          , (\t -> leafWith (prose (pfx <> "-intro") (chars t)) (subhead t)) <$> intro
          ]
      cards = [productCard (pfx <> "-p" <> show j) p | (j, p) <- zip [(0 :: Int) ..] products]
      gridLayout = splitN 24 [(1, stack (map fst card)) | card <- cards]
      cardBlocks = M.fromList (concatMap (map snd) cards)
      layout = stack (map fst introParts <> [gridLayout])
      blocks = M.fromList (map snd introParts) `M.union` cardBlocks
   in Section {secLayout = layout, secBlocks = blocks}

productCard :: String -> Product -> [Part]
productCard pfx p =
  catMaybes
    [ (\ir -> leafWith (figure (pfx <> "-fig") (imgAspect ir) (imgCap ir)) (figureBox (imgCaption ir) (imgSrc ir)))
        <$> imgOrDefault
    , Just (leafWith (headline (pfx <> "-name") (chars (prodName p))) (heading (prodName p)))
    , Just (leafWith (chrome (Custom "price") (pfx <> "-price") 34) (priceTag (prodPrice p)))
    , Just (leafWith (prose (pfx <> "-blurb") (chars (prodBlurb p))) (paragraph (prodBlurb p)))
    , Just (leafWith (chrome (Custom "cta") (pfx <> "-buy") 56) (buyButton (prodBuy p) ("Buy " <> prodName p)))
    ]
  where
    imgOrDefault = case prodImage p of
      Just ir -> Just ir
      Nothing -> Just (ImageRef Nothing (prodName p) 1.2 220)

richColumnsSection :: String -> Maybe Text -> [Text] -> Section
richColumnsSection pfx title cols =
  let titleParts = maybe [] (\t -> [leafWith (headline (pfx <> "-h") (chars t)) (heading t)]) title
      colParts = [leafWith (prose (pfx <> "-c" <> show j) (chars t)) (paragraph t) | (j, t) <- zip [(0 :: Int) ..] cols]
      grid = case colParts of
        [] -> stack []
        [_] -> stack (map fst colParts)
        _ -> splitN 28 [(1, fst c) | c <- colParts]
      layout = stack (map fst titleParts <> [grid])
      blocks = M.fromList (map snd titleParts <> map snd colParts)
   in Section {secLayout = layout, secBlocks = blocks}

gallerySection :: String -> Maybe Text -> [ImageRef] -> Section
gallerySection pfx title imgs =
  let titleParts = maybe [] (\t -> [leafWith (headline (pfx <> "-h") (chars t)) (heading t)]) title
      figs = [leafWith (figure (pfx <> "-g" <> show j) (imgAspect ir) (imgCap ir)) (figureBox (imgCaption ir) (imgSrc ir)) | (j, ir) <- zip [(0 :: Int) ..] imgs]
      grid = case figs of
        [] -> stack []
        _ -> splitN 20 [(1, fst f) | f <- figs]
      layout = stack (map fst titleParts <> [grid])
      blocks = M.fromList (map snd titleParts <> map snd figs)
   in Section {secLayout = layout, secBlocks = blocks}

ctaSection :: String -> Text -> Maybe Text -> Maybe Cta -> Section
ctaSection pfx h body cta =
  sectionOf $
    leafWith (headline (pfx <> "-h") (chars h)) (heading h)
      : catMaybes
        [ (\t -> leafWith (prose (pfx <> "-body") (chars t)) (paragraph t)) <$> body
        , (\(Cta l u) -> leafWith (chrome (Custom "cta") (pfx <> "-cta") 48) (linkText l u)) <$> cta
        ]

contactSection :: String -> Text -> Maybe Text -> Maybe Text -> Section
contactSection pfx title body email =
  sectionOf $
    leafWith (headline (pfx <> "-h") (chars title)) (heading title)
      : catMaybes
        [ (\t -> leafWith (prose (pfx <> "-body") (chars t)) (paragraph t)) <$> body
        , (\e -> leafWith (chrome (Custom "cta") (pfx <> "-mail") 30) (linkText e ("mailto:" <> e))) <$> email
        ]

proseSectionT :: String -> Maybe Text -> Maybe Text -> Text -> Section
proseSectionT pfx kick h body =
  sectionOf $
    catMaybes
      [ (\t -> leafWith (chrome (Custom "kicker") (pfx <> "-kick") 18) (kicker t)) <$> kick
      , (\t -> leafWith (headline (pfx <> "-h") (chars t)) (heading t)) <$> h
      ]
      <> [leafWith (prose (pfx <> "-body") (chars body)) (paragraph body)]
