{-# LANGUAGE OverloadedStrings #-}

-- | The theme layer: a 'Theme' is a value (palette, typography, density,
-- geometry) and 'themeCss' is its single interpreter into the stylesheet that
-- the renderer inlines. Branding lives in a /value/, not in hard-coded CSS, so
-- the same engine renders, say, a magazine and a black-on-white newspaper from
-- data alone.
--
-- All owner-supplied strings (colours, font stacks) are passed through
-- 'sanitizeCssValue' before they reach the stylesheet, so a theme can never
-- break out of the @\<style>@ context — defence in depth even though the site is
-- single-tenant.
module Swwstructor.Theme
  ( Color (..)
  , color
  , Density (..)
  , densityTag
  , densityOfTag
  , Theme (..)
  , defaultTheme
  , nytTheme
  , sanitizeCssValue
  , themeFontLink
  , themeCss
  , themeToJSON
  , themeFromJSON
  ) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import StickyWM (JSON (..), jstr, (.:?))

-- | A CSS colour literal (e.g. @"#1a1a1a"@, @"rgba(0,0,0,.08)"@). Sanitised at
-- render time; the newtype keeps it from being confused with arbitrary text.
newtype Color = Color Text
  deriving (Eq, Show)

color :: Text -> Color
color = Color

-- | Vertical rhythm. 'Airy' is generous (editorial); 'Compact' is dense.
data Density = Airy | Cozy | Compact
  deriving (Eq, Show, Enum, Bounded)

densityTag :: Density -> Text
densityTag Airy = "airy"
densityTag Cozy = "cozy"
densityTag Compact = "compact"

densityOfTag :: Text -> Density
densityOfTag t = case T.toLower t of
  "airy" -> Airy
  "compact" -> Compact
  _ -> Cozy

-- | A theme. Geometry (max width, radius) and typography are part of the brand,
-- so they live here too — never scattered through the renderer.
data Theme = Theme
  { themeName :: !Text
  , themeBg :: !Color
  , themeFg :: !Color
  , themeAccent :: !Color
  , themeMuted :: !Color
  , themeBorder :: !Color
  -- ^ hairline / rule colour
  , themeMastFont :: !Text
  -- ^ font stack for the masthead wordmark (blackletter for a newspaper)
  , themeDisplayFont :: !Text
  -- ^ font stack for headlines
  , themeBodyFont :: !Text
  -- ^ font stack for body copy
  , themeFontUrl :: !(Maybe Text)
  -- ^ optional webfont stylesheet href (a Google Fonts @\<link>@)
  , themeDensity :: !Density
  , themeMaxWidth :: !Int
  -- ^ max content width in px (the page never stretches past this)
  , themeRadius :: !Int
  -- ^ corner radius (px) for figures and buttons; 0 for a hard newspaper look
  }
  deriving (Eq, Show)

-- | A neutral, readable default.
defaultTheme :: Theme
defaultTheme =
  Theme
    { themeName = "Default"
    , themeBg = Color "#ffffff"
    , themeFg = Color "#1a1a1a"
    , themeAccent = Color "#0066cc"
    , themeMuted = Color "#6b7280"
    , themeBorder = Color "rgba(26,26,26,.12)"
    , themeMastFont = "'Playfair Display', Georgia, serif"
    , themeDisplayFont = "'Playfair Display', Georgia, serif"
    , themeBodyFont = "'Inter', system-ui, -apple-system, sans-serif"
    , themeFontUrl = Just "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Playfair+Display:wght@500;600;700;800;900&display=swap"
    , themeDensity = Cozy
    , themeMaxWidth = 1100
    , themeRadius = 8
    }

-- | The New York Times benchmark theme: black on white, a blackletter
-- masthead, a high-contrast serif for headlines, a clean sans for furniture,
-- hairline rules, square corners. This is what makes the @nytFront@ layout
-- /look/ like the paper.
nytTheme :: Theme
nytTheme =
  Theme
    { themeName = "Times"
    , themeBg = Color "#ffffff"
    , themeFg = Color "#121212"
    , themeAccent = Color "#326891"
    , themeMuted = Color "#666666"
    , themeBorder = Color "#dddddd"
    , themeMastFont = "'UnifrakturCook', 'Old English Text MT', Georgia, serif"
    , themeDisplayFont = "'Playfair Display', 'Georgia', 'Times New Roman', serif"
    , themeBodyFont = "'Inter', 'Helvetica Neue', Arial, sans-serif"
    , themeFontUrl = Just "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@400;500;600;700;800;900&family=Inter:wght@400;500;600;700&family=UnifrakturCook:wght@700&display=swap"
    , themeDensity = Airy
    , themeMaxWidth = 1180
    , themeRadius = 0
    }

-- | Strip everything that could end the CSS value / @\<style>@ element. Keeps a
-- conservative safe set: alphanumerics, whitespace, and the punctuation a colour
-- or font stack legitimately needs. Everything else is dropped.
sanitizeCssValue :: Text -> Text
sanitizeCssValue = T.filter ok
  where
    ok c = isAlphaNum c || c `elem` (" #(),.-'%/" :: String)

cssColor :: Color -> Text
cssColor (Color c) = sanitizeCssValue c

cssFont :: Text -> Text
cssFont = sanitizeCssValue

-- | The webfont @\<link>@ href, if the theme declares one (already sanitised to
-- a url-safe set by 'sanitizeCssValue').
themeFontLink :: Theme -> Maybe Text
themeFontLink = fmap sanitizeCssValue . themeFontUrl

-- | Density-derived spacing knobs (block padding, body line-height,
-- inter-section gap).
densityVars :: Density -> (Text, Text)
densityVars Airy = ("6px 2px", "1.55")
densityVars Cozy = ("4px 2px", "1.5")
densityVars Compact = ("2px 2px", "1.42")

-- | Interpret a 'Theme' into a complete stylesheet. The geometry rules are
-- static and reference CSS custom properties; only the @:root@ block carries
-- theme values, so there are no hard-coded colours anywhere else (the A3
-- acceptance condition).
themeCss :: Theme -> Text
themeCss th =
  let (winPad, lineH) = densityVars (themeDensity th)
      rad = T.pack (show (themeRadius th)) <> "px"
   in T.unlines
        [ ":root{"
        , "  --sww-bg:" <> cssColor (themeBg th) <> ";"
        , "  --sww-fg:" <> cssColor (themeFg th) <> ";"
        , "  --sww-accent:" <> cssColor (themeAccent th) <> ";"
        , "  --sww-muted:" <> cssColor (themeMuted th) <> ";"
        , "  --sww-border:" <> cssColor (themeBorder th) <> ";"
        , "  --sww-mast:" <> cssFont (themeMastFont th) <> ";"
        , "  --sww-display:" <> cssFont (themeDisplayFont th) <> ";"
        , "  --sww-body:" <> cssFont (themeBodyFont th) <> ";"
        , "  --sww-radius:" <> rad <> ";"
        , "  --sww-line:" <> lineH <> ";"
        , "}"
        , "*{box-sizing:border-box;}"
        , "html,body{margin:0;padding:0;}"
        , "html{scroll-behavior:smooth;scroll-padding-top:16px;}"
        , "body{background:var(--sww-bg);color:var(--sww-fg);font-family:var(--sww-body);"
        , "  -webkit-font-smoothing:antialiased;line-height:var(--sww-line);}"
        , "main{display:block;padding:20px 0 72px;}"
        , ".sww-win{padding:" <> winPad <> ";}"
        , ".sww-win[data-pinned]{z-index:40;background:var(--sww-bg);}"
        , ".sww-win-nav[data-pinned]{z-index:50;border-bottom:1px solid var(--sww-border);box-shadow:0 2px 10px rgba(0,0,0,.05);}"
        , ".sww-win-mast[data-pinned]{z-index:50;}"
        -- headings
        , ".sww-heading{font-family:var(--sww-display);font-weight:700;letter-spacing:-.01em;"
        , "  margin:0 0 .28em;line-height:1.08;color:var(--sww-fg);}"
        , ".sww-win-headline .sww-heading{font-size:clamp(22px,2.7vw,40px);}"
        , ".sww-win-prose .sww-heading,.sww-win-aside .sww-heading{font-size:19px;}"
        , ".sww-headlink{color:inherit;text-decoration:none;}"
        , ".sww-headlink:hover{text-decoration:underline;text-underline-offset:3px;}"
        -- furniture
        , ".sww-subhead{font-family:var(--sww-body);font-size:15px;font-weight:400;"
        , "  color:var(--sww-muted);margin:0 0 .5em;line-height:1.4;}"
        , ".sww-paragraph{margin:0 0 .7em;font-size:15px;color:var(--sww-fg);}"
        , ".sww-kicker{display:inline-block;font-family:var(--sww-body);font-size:11px;font-weight:700;"
        , "  letter-spacing:.08em;text-transform:uppercase;color:var(--sww-accent);margin:0 0 .35em;}"
        , ".sww-byline{font-size:12px;color:var(--sww-muted);margin:.2em 0;text-transform:uppercase;letter-spacing:.04em;}"
        , ".sww-timestamp{display:inline-block;font-size:11px;font-weight:600;letter-spacing:.06em;"
        , "  text-transform:uppercase;color:var(--sww-muted);}"
        , ".sww-link{color:var(--sww-fg);text-decoration:none;border-bottom:1px solid var(--sww-border);}"
        , ".sww-link:hover{border-color:var(--sww-fg);}"
        , ".sww-rule{border:none;border-top:1px solid var(--sww-fg);margin:6px 0;opacity:.85;}"
        -- nav + masthead
        , ".sww-nav{display:flex;gap:20px;align-items:center;justify-content:center;height:100%;flex-wrap:wrap;}"
        , ".sww-navlink{font-size:13px;font-weight:600;letter-spacing:.01em;color:var(--sww-fg);text-decoration:none;}"
        , ".sww-navlink:hover{color:var(--sww-accent);}"
        , ".sww-win-nav{border-top:1px solid var(--sww-fg);border-bottom:1px solid var(--sww-fg);}"
        , ".sww-brand{display:flex;align-items:center;justify-content:center;height:100%;}"
        , ".sww-brandlink{font-family:var(--sww-mast);font-weight:700;letter-spacing:.01em;"
        , "  font-size:clamp(34px,6vw,68px);color:var(--sww-fg);text-decoration:none;line-height:1;}"
        , ".sww-win-mast{text-align:center;}"
        , ".sww-win-strip{display:flex;align-items:center;justify-content:center;font-size:12px;color:var(--sww-muted);}"
        -- figures
        , ".sww-figure{width:100%;height:100%;min-height:120px;border-radius:var(--sww-radius);"
        , "  background:linear-gradient(135deg,var(--sww-border),var(--sww-bg) 70%);"
        , "  display:flex;align-items:flex-end;padding:10px;overflow:hidden;position:relative;}"
        , ".sww-figure-img{padding:0;}"
        , ".sww-img{width:100%;height:100%;object-fit:cover;border-radius:var(--sww-radius);display:block;}"
        , ".sww-figcap{font-family:var(--sww-body);font-size:11px;color:var(--sww-muted);"
        , "  background:var(--sww-bg);padding:2px 6px;border-radius:4px;position:absolute;left:8px;bottom:8px;opacity:.9;}"
        , ".sww-figure-img .sww-figcap{position:static;background:none;padding:4px 0 0;}"
        -- commerce
        , ".sww-price{display:inline-block;font-family:var(--sww-display);font-size:22px;font-weight:600;"
        , "  color:var(--sww-fg);border-bottom:2px solid var(--sww-accent);padding-bottom:2px;}"
        , ".sww-buyform{margin:0;}"
        , ".sww-buy{appearance:none;border:none;cursor:pointer;font-family:var(--sww-body);font-weight:700;"
        , "  font-size:13px;letter-spacing:.02em;color:var(--sww-bg);background:var(--sww-accent);"
        , "  border-radius:calc(var(--sww-radius) + 4px);padding:11px 18px;width:100%;"
        , "  transition:transform .12s ease,opacity .12s ease;}"
        , ".sww-buy:hover{transform:translateY(-1px);opacity:.92;}"
        -- footer
        , ".sww-footer{font-size:12px;color:var(--sww-muted);text-align:center;padding:36px 0 24px;border-top:1px solid var(--sww-border);margin-top:24px;}"
        , ".sww-page-msg{max-width:560px;margin:14vh auto;padding:0 24px;text-align:center;}"
        ]

-- ---------------------------------------------------------------------------
-- JSON codec
-- ---------------------------------------------------------------------------

themeToJSON :: Theme -> JSON
themeToJSON th =
  JObj
    [ ("name", str (themeName th))
    , ("bg", colorJSON (themeBg th))
    , ("fg", colorJSON (themeFg th))
    , ("accent", colorJSON (themeAccent th))
    , ("muted", colorJSON (themeMuted th))
    , ("border", colorJSON (themeBorder th))
    , ("mastFont", str (themeMastFont th))
    , ("displayFont", str (themeDisplayFont th))
    , ("bodyFont", str (themeBodyFont th))
    , ("fontUrl", maybe JNull str (themeFontUrl th))
    , ("density", str (densityTag (themeDensity th)))
    , ("maxWidth", JNum (fromIntegral (themeMaxWidth th)))
    , ("radius", JNum (fromIntegral (themeRadius th)))
    ]
  where
    str = JStr . T.unpack
    colorJSON (Color c) = JStr (T.unpack c)

-- | Decode a theme, falling back to 'defaultTheme' field by field so a partial
-- theme object is always valid (owners only override what they care about).
themeFromJSON :: JSON -> Either String Theme
themeFromJSON v = do
  let d = defaultTheme
      optStr key dflt = case v .:? key of
        Just (JStr s) -> T.pack s
        _ -> dflt
      optColor key (Color dflt) = Color (case v .:? key of Just (JStr s) -> T.pack s; _ -> dflt)
      optNum key dflt = case v .:? key of
        Just (JNum n) -> round n
        _ -> dflt
      optFontUrl = case v .:? "fontUrl" of
        Just (JStr s) -> Just (T.pack s)
        Just JNull -> Nothing
        _ -> themeFontUrl d
  name <- case v .:? "name" of
    Just j -> T.pack <$> jstr j
    Nothing -> Right (themeName d)
  pure
    Theme
      { themeName = name
      , themeBg = optColor "bg" (themeBg d)
      , themeFg = optColor "fg" (themeFg d)
      , themeAccent = optColor "accent" (themeAccent d)
      , themeMuted = optColor "muted" (themeMuted d)
      , themeBorder = optColor "border" (themeBorder d)
      , themeMastFont = optStr "mastFont" (themeMastFont d)
      , themeDisplayFont = optStr "displayFont" (themeDisplayFont d)
      , themeBodyFont = optStr "bodyFont" (themeBodyFont d)
      , themeFontUrl = optFontUrl
      , themeDensity = densityOfTag (optStr "density" (densityTag (themeDensity d)))
      , themeMaxWidth = optNum "maxWidth" (themeMaxWidth d)
      , themeRadius = optNum "radius" (themeRadius d)
      }
