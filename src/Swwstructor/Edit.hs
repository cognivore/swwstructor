{-# LANGUAGE OverloadedStrings #-}

-- | The pure editing core of the constructor: structural operations an owner
-- performs through the admin — add/delete/reorder pages and sections, edit a
-- section's content, manage the list items inside a section (stories, products,
-- columns, images, nav links), and set the theme. Everything here is a pure
-- @SiteSpec -> SiteSpec@ (or a projection), so the whole editing model is
-- offline-testable and the server is a thin shell over it.
--
-- The owner never touches layout: they choose a section /kind/ and fill its
-- content; the engine ('Swwstructor.Templates' + the solver) decides placement.
module Swwstructor.Edit
  ( -- * Section kinds (the palette the owner adds from)
    sectionKinds
  , sectionTagOf
  , sectionLabelOf
  , blankSection

    -- * Page operations
  , pageIndex
  , addPage
  , deletePage
  , setPageTitle

    -- * Section operations (within a /container/: a page OR a partial)
  , sectionsOf
  , sectionAt
  , addSection
  , deleteSection
  , moveSection
  , setSectionAt

    -- * Containers & partials (the template engine)
  , partialPath
  , partialPathName
  , isPartialPath
  , containerTitle
  , partialNames
  , addPartial
  , deletePartial

    -- * List items inside a section
  , addListItem
  , deleteListItem

    -- * Site-level
  , setSiteMeta
  , setTheme

    -- * Defaults
  , blankStory
  , blankProduct
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Swwstructor.Block (BuyTarget (BuyTarget), NavLink (NavLink))
import Swwstructor.Content
  ( Cta (Cta)
  , HeroContent (HeroContent)
  , PageSpec (PageSpec, pagePath, pageSections, pageTitle)
  , Partial (Partial, partialName, partialSections)
  , Product (Product)
  , SectionSpec (..)
  , SiteSpec (sitePages, sitePartials, siteTheme, siteDescription, siteTitle)
  , Story
  , imageRef
  , story
  )
import Swwstructor.Money (Currency (EUR), Price (Price), Cents (Cents))
import Swwstructor.Theme (Theme)

-- ---------------------------------------------------------------------------
-- Section kinds
-- ---------------------------------------------------------------------------

-- | The orderable palette of section kinds: @(tag, human label)@.
sectionKinds :: [(Text, Text)]
sectionKinds =
  [ ("masthead", "Masthead (wordmark)")
  , ("navStrip", "Navigation bar")
  , ("ribbon", "Ribbon (link strip)")
  , ("hero", "Hero (headline + figure)")
  , ("featureSplit", "Feature split (main + rail of stories)")
  , ("storyRow", "Story row (equal columns)")
  , ("productGrid", "Product grid (shop)")
  , ("richColumns", "Rich columns (prose)")
  , ("gallery", "Gallery (images)")
  , ("ctaBand", "Call to action")
  , ("contact", "Contact")
  , ("prose", "Prose block")
  , ("footer", "Footer band")
  , ("include", "Include a partial (reuse)")
  ]

-- | The stable tag of a section (matches the JSON codec discriminator).
sectionTagOf :: SectionSpec -> Text
sectionTagOf s = case s of
  Masthead{} -> "masthead"
  NavStrip{} -> "navStrip"
  Ribbon{} -> "ribbon"
  Hero{} -> "hero"
  FeatureSplit{} -> "featureSplit"
  StoryRow{} -> "storyRow"
  ProductGrid{} -> "productGrid"
  RichColumns{} -> "richColumns"
  Gallery{} -> "gallery"
  CtaBand{} -> "ctaBand"
  Contact{} -> "contact"
  ProseSection{} -> "prose"
  FooterBand{} -> "footer"
  IncludePartial{} -> "include"

-- | The human label for a section value.
sectionLabelOf :: SectionSpec -> Text
sectionLabelOf s = maybe (sectionTagOf s) id (lookup (sectionTagOf s) sectionKinds)

-- | A sensible empty section of the given kind (what "add section" inserts).
-- 'Nothing' for an unknown tag.
blankSection :: Text -> Maybe SectionSpec
blankSection tag = case tag of
  "masthead" -> Just (Masthead "New masthead" Nothing)
  "navStrip" -> Just (NavStrip [NavLink "Home" "/"] True)
  "ribbon" -> Just (Ribbon [NavLink "Latest" "/"])
  "hero" -> Just (Hero (HeroContent Nothing "New hero headline" (Just "A short standfirst.") Nothing Nothing Nothing))
  "featureSplit" -> Just (FeatureSplit 0.62 28 [blankStory] [])
  "storyRow" -> Just (StoryRow [blankStory])
  "productGrid" -> Just (ProductGrid (Just "Shop") Nothing [blankProduct 0])
  "richColumns" -> Just (RichColumns (Just "Section title") ["First column.", "Second column."])
  "gallery" -> Just (Gallery (Just "Gallery") [imageRef "A picture"])
  "ctaBand" -> Just (CtaBand "Call to action" (Just "Supporting line.") (Just (Cta "Go" "/")))
  "contact" -> Just (Contact "Contact" (Just "How to reach us.") (Just "hello@example.com"))
  "prose" -> Just (ProseSection Nothing (Just "Heading") "Write your prose here.")
  "footer" -> Just (FooterBand "Footer text")
  "include" -> Just (IncludePartial "")
  _ -> Nothing

blankStory :: Story
blankStory = story "New story headline"

blankProduct :: Int -> Product
blankProduct n =
  Product "New item" "What it is." (Price (Cents 1000) EUR) Nothing (BuyTarget ("item-" <> T.pack (show n)))

-- ---------------------------------------------------------------------------
-- Page operations
-- ---------------------------------------------------------------------------

-- | The index of the page with the given (normalised) path.
pageIndex :: Text -> SiteSpec -> Maybe Int
pageIndex path site = lookup (normPath path) (zip (map (normPath . pagePath) (sitePages site)) [0 ..])

normPath :: Text -> Text
normPath p
  | T.null p = "/"
  | p == "/" = "/"
  | otherwise = "/" <> T.dropWhile (== '/') (T.dropWhileEnd (== '/') p)

-- | Append a page (no-op if the path already exists).
addPage :: Text -> Text -> SiteSpec -> SiteSpec
addPage path title site
  | Just _ <- pageIndex path site = site
  | otherwise =
      site {sitePages = sitePages site <> [PageSpec (normPath path) title []]}

-- | Delete the page at a path (never deletes the last remaining page).
deletePage :: Text -> SiteSpec -> SiteSpec
deletePage path site =
  case pageIndex path site of
    Just i | length (sitePages site) > 1 ->
      site {sitePages = dropIndex i (sitePages site)}
    _ -> site

-- | Set a page's title.
setPageTitle :: Text -> Text -> SiteSpec -> SiteSpec
setPageTitle path title = overPage path (\pg -> pg {pageTitle = title})

-- ---------------------------------------------------------------------------
-- Section operations
-- ---------------------------------------------------------------------------

-- | The sections of a /container/: a real page path, or a synthetic
-- @\@partial:NAME@ path addressing a partial. This is what lets the one section
-- editor edit pages, the header/footer, and any partial.
sectionsOf :: Text -> SiteSpec -> [SectionSpec]
sectionsOf path site = case partialPathName path of
  Just nm -> maybe [] partialSections (findPartial nm site)
  Nothing -> maybe [] pageSections (pageIndex path site >>= \i -> safeIndex i (sitePages site))

sectionAt :: Text -> Int -> SiteSpec -> Maybe SectionSpec
sectionAt path i site = safeIndex i (sectionsOf path site)

addSection :: Text -> Text -> SiteSpec -> SiteSpec
addSection path tag site = case blankSection tag of
  Just sec -> overContainer path (<> [sec]) site
  Nothing -> site

deleteSection :: Text -> Int -> SiteSpec -> SiteSpec
deleteSection path i = overContainer path (dropIndex i)

-- | Move the section at @i@ by @delta@ (clamped); used by ↑ / ↓ buttons.
moveSection :: Text -> Int -> Int -> SiteSpec -> SiteSpec
moveSection path i delta = overContainer path (\secs -> moveAt i (i + delta) secs)

setSectionAt :: Text -> Int -> SectionSpec -> SiteSpec -> SiteSpec
setSectionAt path i sec = overContainer path (updateAt i (const sec))

-- ---------------------------------------------------------------------------
-- Containers & partials (the template engine)
-- ---------------------------------------------------------------------------

-- | The synthetic editor path for a partial.
partialPath :: Text -> Text
partialPath nm = "@partial:" <> nm

-- | The partial name in a synthetic path, if it is one.
partialPathName :: Text -> Maybe Text
partialPathName = T.stripPrefix "@partial:"

isPartialPath :: Text -> Bool
isPartialPath = ("@partial:" `T.isPrefixOf`)

-- | A human title for a container path (for the editor heading).
containerTitle :: Text -> SiteSpec -> Text
containerTitle path site = case partialPathName path of
  Just nm
    | nm == "header" -> "Header (every page)"
    | nm == "footer" -> "Footer (every page)"
    | otherwise -> "Partial: " <> nm
  Nothing -> maybe path pageTitle (pageIndex path site >>= \i -> safeIndex i (sitePages site))

partialNames :: SiteSpec -> [Text]
partialNames = map partialName . sitePartials

findPartial :: Text -> SiteSpec -> Maybe Partial
findPartial nm site = case [p | p <- sitePartials site, partialName p == nm] of
  (p : _) -> Just p
  [] -> Nothing

-- | Create an empty partial (no-op if the name already exists).
addPartial :: Text -> SiteSpec -> SiteSpec
addPartial nm site
  | T.null (T.strip nm) = site
  | Just _ <- findPartial nm site = site
  | otherwise = site {sitePartials = sitePartials site <> [Partial nm []]}

deletePartial :: Text -> SiteSpec -> SiteSpec
deletePartial nm site = site {sitePartials = filter ((/= nm) . partialName) (sitePartials site)}

-- | Transform a container's sections, whether it is a page or a partial.
overContainer :: Text -> ([SectionSpec] -> [SectionSpec]) -> SiteSpec -> SiteSpec
overContainer path f site = case partialPathName path of
  Just nm -> overPartial nm f site
  Nothing -> overSections path f site

overPartial :: Text -> ([SectionSpec] -> [SectionSpec]) -> SiteSpec -> SiteSpec
overPartial nm f site =
  site {sitePartials = [if partialName p == nm then p {partialSections = f (partialSections p)} else p | p <- sitePartials site]}

-- ---------------------------------------------------------------------------
-- List items inside a section
-- ---------------------------------------------------------------------------

-- | Append a blank item to a named list within a section. The list name selects
-- which list (a @featureSplit@ has @main@ and @rail@); unknown names are no-ops.
addListItem :: Text -> SectionSpec -> SectionSpec
addListItem listName sec = case (sec, listName) of
  (FeatureSplit r g m rl, "main") -> FeatureSplit r g (m <> [blankStory]) rl
  (FeatureSplit r g m rl, "rail") -> FeatureSplit r g m (rl <> [blankStory])
  (StoryRow ss, "stories") -> StoryRow (ss <> [blankStory])
  (ProductGrid t i ps, "products") -> ProductGrid t i (ps <> [blankProduct (length ps)])
  (RichColumns t cs, "columns") -> RichColumns t (cs <> ["New column."])
  (Gallery t imgs, "images") -> Gallery t (imgs <> [imageRef "A picture"])
  (NavStrip ls sticky, "links") -> NavStrip (ls <> [NavLink "Link" "/"]) sticky
  (Ribbon ls, "links") -> Ribbon (ls <> [NavLink "Link" "/"])
  _ -> sec

-- | Remove item @i@ from a named list within a section.
deleteListItem :: Text -> Int -> SectionSpec -> SectionSpec
deleteListItem listName i sec = case (sec, listName) of
  (FeatureSplit r g m rl, "main") -> FeatureSplit r g (dropIndex i m) rl
  (FeatureSplit r g m rl, "rail") -> FeatureSplit r g m (dropIndex i rl)
  (StoryRow ss, "stories") -> StoryRow (dropIndex i ss)
  (ProductGrid t ix ps, "products") -> ProductGrid t ix (dropIndex i ps)
  (RichColumns t cs, "columns") -> RichColumns t (dropIndex i cs)
  (Gallery t imgs, "images") -> Gallery t (dropIndex i imgs)
  (NavStrip ls sticky, "links") -> NavStrip (dropIndex i ls) sticky
  (Ribbon ls, "links") -> Ribbon (dropIndex i ls)
  _ -> sec

-- ---------------------------------------------------------------------------
-- Site-level
-- ---------------------------------------------------------------------------

setSiteMeta :: Text -> Text -> SiteSpec -> SiteSpec
setSiteMeta title desc site = site {siteTitle = title, siteDescription = desc}

setTheme :: Theme -> SiteSpec -> SiteSpec
setTheme th site = site {siteTheme = th}

-- ---------------------------------------------------------------------------
-- Plumbing
-- ---------------------------------------------------------------------------

overPage :: Text -> (PageSpec -> PageSpec) -> SiteSpec -> SiteSpec
overPage path f site = case pageIndex path site of
  Just i -> site {sitePages = updateAt i f (sitePages site)}
  Nothing -> site

overSections :: Text -> ([SectionSpec] -> [SectionSpec]) -> SiteSpec -> SiteSpec
overSections path f = overPage path (\pg -> pg {pageSections = f (pageSections pg)})

safeIndex :: Int -> [a] -> Maybe a
safeIndex i xs
  | i >= 0 && i < length xs = Just (xs !! i)
  | otherwise = Nothing

dropIndex :: Int -> [a] -> [a]
dropIndex i xs = [x | (j, x) <- zip [0 ..] xs, j /= i]

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f xs = [if j == i then f x else x | (j, x) <- zip [0 ..] xs]

-- | Move the element at @from@ to position @to@ (both clamped into range).
moveAt :: Int -> Int -> [a] -> [a]
moveAt from to xs
  | from < 0 || from >= n = xs
  | otherwise = insertAt to' x (dropIndex from xs)
  where
    n = length xs
    x = xs !! from
    to' = max 0 (min (n - 1) to)
    insertAt k y ys = let (a, b) = splitAt k ys in a <> [y] <> b
