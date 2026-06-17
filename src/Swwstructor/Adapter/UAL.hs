{-# LANGUAGE OverloadedStrings #-}

-- | A content-source adapter (CONSTRUCTOR §3.5): read a @universal-art-link@
-- style content file and map it onto the constructor's 'SiteSpec'. UAL's section
-- vocabulary (@hero@, @projects-grid@, @text-columns@, @list-section@,
-- @contact@) and its @site.config@ palette become our sections + 'Theme'. This
-- proves the constructor can ingest a foreign schema without per-page code — the
-- owner edits UAL content, the engine lays it out.
--
-- The input shape (JSON, the same one a UAL YAML file decodes to):
--
-- @
-- { "title": "...", "description": "...",
--   "theme": { "background":"#..","foreground":"#..","accent":"#.." },
--   "navigation": [ { "label":"..", "href":".." } ],
--   "pages": [ { "slug":"/", "title":"..",
--     "sections": [ { "type":"hero", "heading":"..", "subheading":"..",
--                     "image":"..", "ctaLabel":"..", "ctaUrl":".." },
--                   { "type":"projects-grid", "projects":[ {"title","blurb","image","href"} ] },
--                   { "type":"text-columns", "title":"..", "columns":["..",".."] },
--                   { "type":"list-section", "title":"..", "items":[ {"title","note","href"} ] },
--                   { "type":"contact", "heading":"..", "description":"..", "email":".." } ] } ] }
-- @
module Swwstructor.Adapter.UAL
  ( UalProject (..)
  , UalListItem (..)
  , UalSection (..)
  , UalPage (..)
  , UalSite (..)
  , ualToSite
  , ualSectionToSpec
  , ualThemeOf
  , ualSiteFromJSON
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Swwstructor.Block (NavLink (NavLink))
import Swwstructor.Content
  ( Cta (Cta)
  , HeroContent (HeroContent)
  , ImageRef (ImageRef)
  , PageSpec (PageSpec)
  , SectionSpec (..)
  , SiteSpec (..)
  , Story (..)
  , story
  )
import Swwstructor.Theme (Color (Color), Theme (themeAccent, themeBg, themeFg), defaultTheme)
import StickyWM (JSON (JStr), jarr, jstr, (.:), (.:?))

-- ---------------------------------------------------------------------------
-- The UAL input model
-- ---------------------------------------------------------------------------

data UalProject = UalProject
  { upTitle :: !Text
  , upBlurb :: !(Maybe Text)
  , upImage :: !(Maybe Text)
  , upHref :: !(Maybe Text)
  }
  deriving (Eq, Show)

data UalListItem = UalListItem
  { uliTitle :: !Text
  , uliNote :: !(Maybe Text)
  , uliHref :: !(Maybe Text)
  }
  deriving (Eq, Show)

data UalSection
  = UHero !Text !(Maybe Text) !(Maybe Text) !(Maybe (Text, Text))
  | UProjectsGrid !(Maybe Text) ![UalProject]
  | UTextColumns !(Maybe Text) ![Text]
  | UListSection !(Maybe Text) ![UalListItem]
  | UContact !(Maybe Text) !(Maybe Text) !(Maybe Text)
  deriving (Eq, Show)

data UalPage = UalPage
  { upgSlug :: !Text
  , upgTitle :: !Text
  , upgSections :: ![UalSection]
  }
  deriving (Eq, Show)

data UalSite = UalSite
  { ussTitle :: !Text
  , ussDescription :: !Text
  , ussBackground :: !(Maybe Text)
  , ussForeground :: !(Maybe Text)
  , ussAccent :: !(Maybe Text)
  , ussNav :: ![NavLink]
  , ussPages :: ![UalPage]
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- The mapping
-- ---------------------------------------------------------------------------

-- | Map a UAL site to a constructor 'SiteSpec'. A single UAL section may expand
-- into several constructor sections (e.g. a titled grid → a heading + a row).
ualToSite :: UalSite -> SiteSpec
ualToSite us =
  SiteSpec
    { siteTitle = ussTitle us
    , siteDescription = ussDescription us
    , siteBaseUrl = Nothing
    , siteTheme = ualThemeOf us
    , siteNav = ussNav us
    , siteFooter = Nothing
    , sitePages =
        [ PageSpec (upgSlug p) (upgTitle p) (concatMap ualSectionToSpec (upgSections p))
        | p <- ussPages us
        ]
    , sitePartials = []
    }

-- | Override only the palette fields the UAL config supplies.
ualThemeOf :: UalSite -> Theme
ualThemeOf us =
  defaultTheme
    { themeBg = maybe (themeBg defaultTheme) Color (ussBackground us)
    , themeFg = maybe (themeFg defaultTheme) Color (ussForeground us)
    , themeAccent = maybe (themeAccent defaultTheme) Color (ussAccent us)
    }

-- | Map one UAL section to one or more constructor 'SectionSpec's.
ualSectionToSpec :: UalSection -> [SectionSpec]
ualSectionToSpec sec = case sec of
  UHero headline sub im cta ->
    [Hero (HeroContent Nothing headline sub Nothing (withSrc im) (fmap (uncurry Cta) cta))]
  UProjectsGrid title projects ->
    titleProse title <> [StoryRow (map projectStory projects)]
  UTextColumns title cols -> [RichColumns title cols]
  UListSection title items ->
    titleProse title <> [StoryRow (map listStory items)]
  UContact h body email -> [Contact (maybe "Contact" id h) body email]
  where
    titleProse Nothing = []
    titleProse (Just t) = [ProseSection Nothing (Just t) ""]

withSrc :: Maybe Text -> Maybe ImageRef
withSrc = fmap (\u -> ImageRef (Just u) "" 1.5 360)

projectStory :: UalProject -> Story
projectStory pr =
  (story (upTitle pr))
    { storyHref = upHref pr
    , storyDek = upBlurb pr
    , storyImage = withSrc (upImage pr)
    }

listStory :: UalListItem -> Story
listStory it =
  (story (uliTitle it))
    { storyHref = uliHref it
    , storyDek = uliNote it
    }

-- ---------------------------------------------------------------------------
-- JSON decoder
-- ---------------------------------------------------------------------------

ualSiteFromJSON :: JSON -> Either String UalSite
ualSiteFromJSON v = do
  title <- txt v "title"
  let desc = optTxt v "description"
      theme = v .:? "theme"
      navJ = v .:? "navigation"
  nav <- case navJ of
    Just j -> mapM navOf =<< jarr j
    Nothing -> Right []
  pagesJ <- v .: "pages"
  pages <- mapM pageOf =<< jarr pagesJ
  pure
    UalSite
      { ussTitle = title
      , ussDescription = maybe "" id desc
      , ussBackground = theme >>= \t -> optTxt t "background"
      , ussForeground = theme >>= \t -> optTxt t "foreground"
      , ussAccent = theme >>= \t -> optTxt t "accent"
      , ussNav = nav
      , ussPages = pages
      }
  where
    navOf j = NavLink <$> txt j "label" <*> txt j "href"

    pageOf j = do
      slug <- txt j "slug"
      ptitle <- txt j "title"
      secsJ <- j .: "sections"
      secs <- mapM sectionOf =<< jarr secsJ
      pure (UalPage slug ptitle secs)

    sectionOf j = do
      ty <- jstr =<< j .: "type"
      case ty of
        "hero" -> do
          headline <- txt j "heading"
          let cta = case (optTxt j "ctaLabel", optTxt j "ctaUrl") of
                (Just l, Just h) -> Just (l, h)
                _ -> Nothing
          pure (UHero headline (optTxt j "subheading") (optTxt j "image") cta)
        "projects-grid" -> do
          projsJ <- j .: "projects"
          projs <- mapM projOf =<< jarr projsJ
          pure (UProjectsGrid (optTxt j "title") projs)
        "text-columns" -> do
          colsJ <- j .: "columns"
          cols <- mapM (fmap T.pack . jstr) =<< jarr colsJ
          pure (UTextColumns (optTxt j "title") cols)
        "list-section" -> do
          itemsJ <- j .: "items"
          items <- mapM itemOf =<< jarr itemsJ
          pure (UListSection (optTxt j "title") items)
        "contact" -> pure (UContact (optTxt j "heading") (optTxt j "description") (optTxt j "email"))
        other -> Left ("unknown UAL section type: " ++ other)

    projOf j = do
      t <- txt j "title"
      pure (UalProject t (optTxt j "blurb") (optTxt j "image") (optTxt j "href"))

    itemOf j = do
      t <- txt j "title"
      pure (UalListItem t (optTxt j "note") (optTxt j "href"))

txt :: JSON -> String -> Either String Text
txt j k = T.pack <$> (jstr =<< j .: k)

optTxt :: JSON -> String -> Maybe Text
optTxt j k = case j .:? k of
  Just (JStr s) -> Just (T.pack s)
  _ -> Nothing
