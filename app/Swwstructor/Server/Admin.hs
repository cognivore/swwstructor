{-# LANGUAGE OverloadedStrings #-}

-- | The constructor's admin: a content editor, not a settings page. The owner
-- writes content — pages, sections, headlines, stories, products, theme — and
-- the sticky/wm engine places it; the owner never touches layout. This module
-- renders the editor (dashboard, page editor with a live split preview, a form
-- per section kind with add/remove for list items, a theme editor, and the
-- Stripe-keys tab) and provides the PURE parsers that turn a submitted form back
-- into a 'SectionSpec' / 'Theme'. The server ("Swwstructor.Server.App") is a
-- thin shell that wires these to the edit operations in "Swwstructor.Edit".
module Swwstructor.Server.Admin
  ( -- * Pages
    adminLoginPage
  , dashboardPage
  , pageEditorPage
  , sectionFormPage
  , themeEditorPage
  , stripePage
  , templatePage
  , mediaLibraryPage
    -- * Pure form parsers
  , parseSection
  , parseTheme
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Swwstructor.Block (BuyTarget (BuyTarget), NavLink (NavLink), navHref, navLabel)
import Swwstructor.Content
  ( Cta (Cta)
  , HeroContent (..)
  , ImageRef (..)
  , PageSpec (pagePath, pageSections, pageTitle)
  , Partial (partialName, partialSections)
  , Product (..)
  , SectionSpec (..)
  , SiteSpec (siteDescription, sitePages, sitePartials, siteTitle)
  , Story (..)
  )
import Swwstructor.Plugins.StripeCatalog (CatalogItem (..))
import Swwstructor.Server.Media (MediaEntry (..))
import Swwstructor.Edit (isPartialPath, sectionKinds, sectionLabelOf)
import Swwstructor.Html (Html, el, elAttr, esc, htmlConcat, rawHtml, voidElAttr)
import Swwstructor.Money
  ( Cents (Cents)
  , Currency (EUR, GBP, JPY, USD)
  , Price (Price)
  , centsInt
  , currencyCode
  , currencyOfCode
  , priceCurrency
  )
import Swwstructor.Server.SecretStore (SecretBundle (sbStripePk, sbStripeSk, sbStripeWebhook))
import Swwstructor.Theme
  ( Color (Color)
  , Density (Airy, Compact, Cozy)
  , Theme (..)
  , densityTag
  )

-- ---------------------------------------------------------------------------
-- Shell + CSS (a neutral admin chrome, independent of the site theme so editing
-- a wild palette can never make the editor unusable)
-- ---------------------------------------------------------------------------

adminDoc :: Text -> Html -> Html
adminDoc title body =
  rawHtml "<!DOCTYPE html>"
    <> elAttr
      "html"
      [("lang", "en")]
      ( htmlConcat
          [ el "head" $
              htmlConcat
                [ voidElAttr "meta" [("charset", "utf-8")]
                , voidElAttr "meta" [("name", "viewport"), ("content", "width=device-width, initial-scale=1")]
                , el "title" (esc title)
                , elAttr "style" [] (rawHtml adminCss)
                ]
          , el "body" body
          ]
      )

adminCss :: Text
adminCss =
  T.unlines
    [ ":root{--ink:#16181d;--mut:#6b7280;--line:#e5e7eb;--bg:#f7f8fa;--card:#fff;--acc:#2563eb;--danger:#dc2626;--ok:#16a34a;--rad:10px}"
    , "*{box-sizing:border-box}body{margin:0;background:var(--bg);color:var(--ink);font:15px/1.5 system-ui,-apple-system,Segoe UI,Roboto,sans-serif}"
    , "a{color:var(--acc);text-decoration:none}a:hover{text-decoration:underline}"
    , ".topbar{display:flex;gap:18px;align-items:center;padding:12px 20px;background:var(--card);border-bottom:1px solid var(--line);position:sticky;top:0;z-index:5}"
    , ".topbar .brand{font-weight:800;letter-spacing:-.01em}.topbar a{font-weight:600;font-size:14px}.topbar .sp{flex:1}"
    , ".wrap{max-width:1180px;margin:0 auto;padding:22px 20px 80px}"
    , ".card{background:var(--card);border:1px solid var(--line);border-radius:var(--rad);padding:18px 20px;margin:0 0 16px}"
    , "h1{font-size:24px;margin:.1em 0 .5em}h2{font-size:17px;margin:0 0 12px}h3{font-size:14px;margin:0 0 8px;color:var(--mut);text-transform:uppercase;letter-spacing:.05em}"
    , "label{display:block;font-size:13px;font-weight:600;margin:12px 0 4px}"
    , "input[type=text],input[type=password],input[type=number],input[type=url],textarea,select{width:100%;padding:9px 11px;border:1px solid var(--line);border-radius:8px;font:14px system-ui,sans-serif;background:#fff;color:var(--ink)}"
    , "textarea{min-height:72px;resize:vertical}"
    , "input[type=color]{width:46px;height:34px;padding:2px;border:1px solid var(--line);border-radius:8px;vertical-align:middle}"
    , ".row{display:flex;gap:12px;flex-wrap:wrap}.row>*{flex:1;min-width:140px}"
    , ".inline{display:flex;gap:8px;align-items:center}.inline label{margin:0}"
    , "button{appearance:none;border:1px solid var(--line);background:#fff;color:var(--ink);font:600 14px system-ui;padding:9px 16px;border-radius:999px;cursor:pointer}"
    , "button:hover{border-color:#cbd0d8}"
    , "button.primary{background:var(--acc);border-color:var(--acc);color:#fff}"
    , "button.danger{background:#fff;border-color:#f0c2c2;color:var(--danger)}"
    , "button.ghost{background:transparent;border-color:transparent;color:var(--mut);padding:6px 8px}"
    , ".sec{display:flex;align-items:center;gap:10px;padding:10px 12px;border:1px solid var(--line);border-radius:8px;margin:0 0 8px;background:#fff}"
    , ".sec .kind{font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:.05em;color:var(--acc);background:#eef2ff;padding:3px 8px;border-radius:999px}"
    , ".sec .sum{flex:1;color:var(--ink);overflow:hidden;text-overflow:ellipsis;white-space:nowrap}"
    , ".sec form{display:inline;margin:0}"
    , "fieldset{border:1px solid var(--line);border-radius:10px;margin:14px 0;padding:6px 14px 14px}legend{font-size:12px;font-weight:700;color:var(--mut);padding:0 6px}"
    , ".split{display:grid;grid-template-columns:minmax(420px,1fr) 1fr;gap:18px;align-items:start}@media(max-width:900px){.split{grid-template-columns:1fr}}"
    , ".preview{position:sticky;top:70px}.preview iframe{width:100%;height:78vh;border:1px solid var(--line);border-radius:var(--rad);background:#fff}"
    , ".muted{color:var(--mut);font-size:13px}.flash{background:#ecfdf5;border:1px solid #a7f3d0;color:#065f46;padding:9px 12px;border-radius:8px;margin:0 0 12px;font-size:14px}"
    , ".pill{font-size:12px;font-weight:700;padding:2px 8px;border-radius:999px}.pill.on{background:#ecfdf5;color:var(--ok)}.pill.off{background:#f3f4f6;color:var(--mut)}"
    , ".actions{display:flex;gap:8px;flex-wrap:wrap;margin-top:16px;align-items:center}"
    , ".hint{font-size:12px;color:var(--mut);margin:4px 0 0}"
    , ".drop{border:2px dashed var(--line);border-radius:12px;padding:30px 16px;text-align:center;color:var(--mut);cursor:pointer;font-size:14px}"
    , ".drop.on{border-color:var(--acc);color:var(--acc);background:#eef2ff}"
    , "#uplog{list-style:none;margin:10px 0 0;padding:0;font-size:13px}#uplog li{padding:3px 0;color:var(--mut)}#uplog li.ok{color:var(--ok)}#uplog li.err{color:var(--danger)}"
    , ".mediagrid{display:grid;grid-template-columns:repeat(auto-fill,minmax(170px,1fr));gap:14px}"
    , ".mediacard{border:1px solid var(--line);border-radius:10px;overflow:hidden;background:#fff;display:flex;flex-direction:column}"
    , ".mediacard img{width:100%;height:128px;object-fit:cover;display:block;background:#f3f4f6}"
    , ".mediacard .meta{padding:8px 10px;font-size:12px;color:var(--mut);word-break:break-all}"
    , ".mediacard .meta code{color:var(--ink);font-size:11px}"
    , ".mediacard .bar{display:flex;gap:6px;padding:0 10px 10px;margin-top:auto}"
    , ".mediacard .bar button{padding:5px 10px;font-size:12px;border-radius:8px}"
    , "table.plist{width:100%;border-collapse:collapse;font-size:14px}table.plist th{font-size:12px;color:var(--mut);text-transform:uppercase;letter-spacing:.04em;text-align:left}"
    , "table.plist th,table.plist td{padding:8px 10px;border-bottom:1px solid var(--line)}table.plist code{font-size:12px;background:#f3f4f6;padding:1px 6px;border-radius:6px}"
    , "ol.steps{margin:0;padding-left:20px}ol.steps li{margin:0 0 10px}ol.steps b{display:block}"
    ]

topbar :: Html
topbar =
  elAttr "div" [("class", "topbar")] $
    htmlConcat
      [ elAttr "span" [("class", "brand")] (esc "swwstructor")
      , elAttr "a" [("href", "/admin")] (esc "Pages")
      , elAttr "a" [("href", "/admin/template")] (esc "Template")
      , elAttr "a" [("href", "/admin/media")] (esc "Media")
      , elAttr "a" [("href", "/admin/theme")] (esc "Theme")
      , elAttr "a" [("href", "/admin/stripe")] (esc "Shop & Stripe")
      , elAttr "span" [("class", "sp")] mempty
      , elAttr "a" [("href", "/"), ("target", "_blank")] (esc "View site \x2197")
      , elAttr "a" [("href", "/admin/logout")] (esc "Sign out")
      ]

flash :: Maybe Text -> Html
flash Nothing = mempty
flash (Just m) = elAttr "div" [("class", "flash")] (esc m)

-- ---------------------------------------------------------------------------
-- Form field helpers
-- ---------------------------------------------------------------------------

ipt :: Text -> Text -> Text -> Text -> Html
ipt typ name lbl val =
  htmlConcat
    [ elAttr "label" [] (esc lbl)
    , voidElAttr "input" [("type", typ), ("name", name), ("value", val), ("autocomplete", "off")]
    ]

txtArea :: Text -> Text -> Text -> Html
txtArea name lbl val =
  htmlConcat
    [ elAttr "label" [] (esc lbl)
    , elAttr "textarea" [("name", name)] (esc val)
    ]

chk :: Text -> Text -> Bool -> Html
chk name lbl on =
  elAttr "div" [("class", "inline")] $
    voidElAttr "input" ([("type", "checkbox"), ("name", name), ("value", "on")] <> [("checked", "checked") | on])
      <> elAttr "label" [] (esc lbl)

selField :: Text -> Text -> [(Text, Text)] -> Text -> Html
selField name lbl opts selected =
  htmlConcat
    [ elAttr "label" [] (esc lbl)
    , elAttr "select" [("name", name)] $
        htmlConcat
          [ elAttr "option" ([("value", v)] <> [("selected", "selected") | v == selected]) (esc t)
          | (v, t) <- opts
          ]
    ]

hidden :: Text -> Text -> Html
hidden name val = voidElAttr "input" [("type", "hidden"), ("name", name), ("value", val)]

btn :: Text -> Text -> Text -> Html
btn variant action lbl =
  elAttr "button" [("class", variant), ("type", "submit"), ("name", "action"), ("value", action)] (esc lbl)

fieldset' :: Text -> Html -> Html
fieldset' legend inner =
  elAttr "fieldset" [] (elAttr "legend" [] (esc legend) <> inner)

orEmpty :: Maybe Text -> Text
orEmpty = fromMaybe ""

ix :: Text -> Int -> Text -> Text
ix listName i f = listName <> "-" <> T.pack (show i) <> "-" <> f

-- ---------------------------------------------------------------------------
-- Login
-- ---------------------------------------------------------------------------

adminLoginPage :: Theme -> Maybe Text -> Html
adminLoginPage _ merr =
  adminDoc "swwstructor admin \xB7 sign in" $
    elAttr "div" [("class", "wrap"), ("style", "max-width:420px;margin-top:8vh")] $
      elAttr "div" [("class", "card")] $
        htmlConcat
          [ el "h1" (esc "swwstructor admin")
          , elAttr "p" [("class", "muted")] (esc "Sign in to edit your site.")
          , flash merr
          , elAttr "form" [("method", "post"), ("action", "/admin/login")] $
              htmlConcat
                [ ipt "password" "password" "Admin password" ""
                , elAttr "div" [("class", "actions")] (elAttr "button" [("class", "primary"), ("type", "submit")] (esc "Sign in"))
                ]
          ]

-- ---------------------------------------------------------------------------
-- Dashboard
-- ---------------------------------------------------------------------------

dashboardPage :: SiteSpec -> Maybe Text -> Html
dashboardPage site fl =
  adminDoc "swwstructor admin" $
    htmlConcat
      [ topbar
      , elAttr "div" [("class", "wrap")] $
          htmlConcat
            [ flash fl
            , el "h1" (esc "Your site")
            , elAttr "div" [("class", "card")] $
                htmlConcat
                  [ el "h2" (esc "Site details")
                  , elAttr "form" [("method", "post"), ("action", "/admin/site")] $
                      htmlConcat
                        [ ipt "text" "title" "Title" (siteTitle site)
                        , txtArea "description" "Description" (siteDescription site)
                        , elAttr "div" [("class", "actions")] (elAttr "button" [("class", "primary"), ("type", "submit")] (esc "Save"))
                        ]
                  ]
            , elAttr "div" [("class", "card")] $
                htmlConcat
                  [ el "h2" (esc "Pages")
                  , htmlConcat (map pageRow (sitePages site))
                  , elAttr "h3" [("style", "margin-top:16px")] (esc "Add a page")
                  , elAttr "form" [("method", "post"), ("action", "/admin/page/new")] $
                      elAttr "div" [("class", "row")] (ipt "text" "path" "Path (e.g. /about)" "" <> ipt "text" "title" "Title" "")
                        <> elAttr "div" [("class", "actions")] (elAttr "button" [("class", "primary"), ("type", "submit")] (esc "Add page"))
                  ]
            ]
      ]
  where
    pageRow pg =
      elAttr "div" [("class", "sec")] $
        htmlConcat
          [ elAttr "span" [("class", "kind")] (esc (pagePath pg))
          , elAttr "span" [("class", "sum")] (esc (pageTitle pg <> "  \x2014  " <> T.pack (show (length (pageSections pg))) <> " sections"))
          , elAttr "a" [("href", "/admin/page?path=" <> pagePath pg)] (esc "Edit")
          , elAttr "form" [("method", "post"), ("action", "/admin/page/delete")] (hidden "path" (pagePath pg) <> btn "danger" "delete" "Delete")
          ]

-- ---------------------------------------------------------------------------
-- Page editor (split: sections on the left, live preview on the right)
-- ---------------------------------------------------------------------------

pageEditorPage :: Text -> PageSpec -> Maybe Text -> Html
pageEditorPage path pg fl =
  adminDoc ("Edit " <> pageTitle pg) $
    htmlConcat
      [ topbar
      , elAttr "div" [("class", "wrap")] $
          htmlConcat
            [ flash fl
            , elAttr "div" [("style", "display:flex;align-items:baseline;gap:12px")] $
                el "h1" (esc heading)
                  <> elAttr "a" [("href", backHref), ("class", "muted")] (esc backLabel)
            , if container
                then elAttr "p" [("class", "muted")] (esc "These sections are reusable — included via \x201CInclude a partial\x201D, and (for \x201Cheader\x201D/\x201C\&footer\x201D) auto-applied to every page.")
                else mempty
            , elAttr "div" [("class", "split")] $
                htmlConcat
                  [ elAttr "div" [] ((if container then mempty else titleCard) <> sectionsCard <> addCard)
                  , previewPane path
                  ]
            ]
      ]
  where
    container = isPartialPath path
    heading = if container then pageTitle pg else "Page " <> path
    backHref = if container then "/admin/template" else "/admin"
    backLabel = if container then "\x2190 template" else "\x2190 all pages"
    titleCard =
      elAttr "div" [("class", "card")] $
        elAttr "form" [("method", "post"), ("action", "/admin/page/title")] $
          hidden "path" path
            <> ipt "text" "title" "Page title" (pageTitle pg)
            <> elAttr "div" [("class", "actions")] (elAttr "button" [("class", "primary"), ("type", "submit")] (esc "Save title"))
    sectionsCard =
      elAttr "div" [("class", "card")] $
        el "h2" (esc "Sections")
          <> case pageSections pg of
            [] -> elAttr "p" [("class", "muted")] (esc "No sections yet — add one below.")
            secs -> htmlConcat (zipWith sectionRow [(0 :: Int) ..] secs)
    sectionRow i sec =
      elAttr "div" [("class", "sec")] $
        htmlConcat
          [ elAttr "span" [("class", "kind")] (esc (sectionLabelOf sec))
          , elAttr "span" [("class", "sum")] (esc (sectionSummary sec))
          , moveForm i (-1) "\x2191"
          , moveForm i 1 "\x2193"
          , elAttr "a" [("href", "/admin/section?path=" <> path <> "&i=" <> T.pack (show i))] (esc "Edit")
          , elAttr "form" [("method", "post"), ("action", "/admin/section/delete")] (hidden "path" path <> hidden "i" (T.pack (show i)) <> btn "danger" "delete" "Delete")
          ]
    moveForm i d label =
      elAttr "form" [("method", "post"), ("action", "/admin/section/move")] $
        hidden "path" path <> hidden "i" (T.pack (show i)) <> hidden "d" (T.pack (show (d :: Int)))
          <> elAttr "button" [("class", "ghost"), ("type", "submit")] (esc label)
    addCard =
      elAttr "div" [("class", "card")] $
        el "h3" (esc "Add a section")
          <> elAttr "form" [("method", "post"), ("action", "/admin/section/new")] (
               hidden "path" path
                 <> selField "kind" "Kind (the engine places it for you)" sectionKinds defaultKind
                 <> elAttr "div" [("class", "actions")] (elAttr "button" [("class", "primary"), ("type", "submit")] (esc "Add section"))
             )

defaultKind :: Text
defaultKind = case sectionKinds of ((k, _) : _) -> k; [] -> "prose"

-- ---------------------------------------------------------------------------
-- Template tab (the partial / shared-block manager)
-- ---------------------------------------------------------------------------

templatePage :: SiteSpec -> Maybe Text -> Html
templatePage site fl =
  adminDoc "Template" $
    htmlConcat
      [ topbar
      , elAttr "div" [("class", "wrap"), ("style", "max-width:760px")] $
          htmlConcat
            [ flash fl
            , el "h1" (esc "Template")
            , elAttr "p" [("class", "muted")] (esc "Reusable blocks defined once. A partial named \x201Cheader\x201D appears at the top of every page and \x201C\&footer\x201D at the bottom — automatically. Include any other partial on a page with the \x201CInclude a partial\x201D section: define your products (or anything) once and show them on as many pages as you like.")
            , elAttr "div" [("class", "card")] $
                el "h2" (esc "Partials")
                  <> ( case sitePartials site of
                         [] -> elAttr "p" [("class", "muted")] (esc "No partials yet. Create one named \x201Cheader\x201D to share a header across every page.")
                         ps -> htmlConcat (map partialRow ps)
                     )
                  <> elAttr "h3" [("style", "margin-top:16px")] (esc "Create a partial")
                  <> elAttr "form" [("method", "post"), ("action", "/admin/partial/new")] (
                       elAttr "div" [("class", "row")] (ipt "text" "name" "Name (e.g. header, footer, shop)" "")
                         <> elAttr "div" [("class", "actions")] (elAttr "button" [("class", "primary"), ("type", "submit")] (esc "Create partial"))
                     )
            ]
      ]
  where
    partialRow p =
      let nm = partialName p
          auto
            | nm == "header" = "  \xB7  auto: top of every page"
            | nm == "footer" = "  \xB7  auto: bottom of every page"
            | otherwise = ""
       in elAttr "div" [("class", "sec")] $
            htmlConcat
              [ elAttr "span" [("class", "kind")] (esc nm)
              , elAttr "span" [("class", "sum")] (esc (T.pack (show (length (partialSections p))) <> " sections" <> auto))
              , elAttr "a" [("href", "/admin/page?path=@partial:" <> nm)] (esc "Edit")
              , elAttr "form" [("method", "post"), ("action", "/admin/partial/delete")] (hidden "name" nm <> btn "danger" "delete" "Delete")
              ]

sectionSummary :: SectionSpec -> Text
sectionSummary s = case s of
  Masthead t _ -> t
  NavStrip ls _ -> T.intercalate ", " (map navLabel ls)
  Ribbon ls -> T.intercalate ", " (map navLabel ls)
  Hero h -> heroHeadline h
  FeatureSplit _ _ m r -> headlines m <> " / rail " <> T.pack (show (length r))
  StoryRow ss -> headlines ss
  ProductGrid t _ ps -> fromMaybe "Shop" t <> " (" <> T.pack (show (length ps)) <> " products)"
  StripeCatalog t _ -> fromMaybe "Book a class" t <> " (live from your Stripe catalogue)"
  RichColumns t cs -> fromMaybe "Columns" t <> " (" <> T.pack (show (length cs)) <> ")"
  Gallery t imgs -> fromMaybe "Gallery" t <> " (" <> T.pack (show (length imgs)) <> ")"
  CtaBand h _ _ -> h
  Contact t _ _ -> t
  ProseSection _ h b -> fromMaybe (T.take 40 b) h
  FooterBand t -> t
  IncludePartial nm -> "Include: " <> (if T.null nm then "(choose a partial)" else nm)
  where
    headlines = T.intercalate " \xB7 " . map storyHeadline . take 2

-- ---------------------------------------------------------------------------
-- Section form (per kind)
-- ---------------------------------------------------------------------------

sectionFormPage :: [Text] -> Text -> Int -> SectionSpec -> Maybe Text -> Html
sectionFormPage partials path i sec fl =
  adminDoc "Edit section" $
    htmlConcat
      [ topbar
      , elAttr "div" [("class", "wrap")] $
          htmlConcat
            [ flash fl
            , elAttr "div" [("style", "display:flex;align-items:baseline;gap:12px")] $
                el "h1" (esc (sectionLabelOf sec))
                  <> elAttr "a" [("href", "/admin/page?path=" <> path), ("class", "muted")] (esc ("\x2190 back to " <> path))
            , elAttr "div" [("class", "split")] $
                htmlConcat
                  [ elAttr "div" [("class", "card")] $
                      elAttr "form" [("method", "post"), ("action", "/admin/section?path=" <> path <> "&i=" <> T.pack (show i))] $
                        htmlConcat
                          [ sectionFields partials sec
                          , elAttr "div" [("class", "actions")] $
                              btn "primary" "save" "Save"
                                <> elAttr "a" [("href", "/admin/page?path=" <> path), ("class", "muted")] (esc "Done")
                          ]
                  , previewPane path
                  ]
            ]
      ]

-- | The right-hand live preview pane. For a real page it embeds the page; for a
-- partial/header/footer (a synthetic path) it embeds the standalone render route.
previewPane :: Text -> Html
previewPane path =
  elAttr "div" [("class", "preview")] $
    elAttr "div" [("class", "card")] $
      el "h3" (esc "Live preview (sticky engine)")
        <> voidElAttr "iframe" [("src", previewSrc path), ("title", "preview")]

previewSrc :: Text -> Text
previewSrc path = if isPartialPath path then "/admin/render?path=" <> path else path

sectionFields :: [Text] -> SectionSpec -> Html
sectionFields partials s = case s of
  Masthead t tag ->
    ipt "text" "title" "Wordmark / title" t <> ipt "text" "tagline" "Tagline (optional)" (orEmpty tag)
  NavStrip ls sticky ->
    chk "sticky" "Pin to top for the whole page (always sticky)" sticky <> linkList "links" ls
  Ribbon ls -> linkList "links" ls
  Hero h ->
    ipt "text" "kicker" "Kicker (optional)" (orEmpty (heroKicker h))
      <> ipt "text" "headline" "Headline" (heroHeadline h)
      <> txtArea "dek" "Standfirst / dek" (orEmpty (heroDek h))
      <> txtArea "body" "Body" (orEmpty (heroBody h))
      <> imgFields "img" (heroImage h)
      <> ctaFields "cta" (heroCta h)
  FeatureSplit rho gut m r ->
    elAttr "div" [("class", "row")] (ipt "number" "rho" "Main column ratio (0-1)" (numT rho) <> ipt "number" "gutter" "Gutter (px)" (numT gut))
      <> el "h3" (esc "Main column stories")
      <> storyList "main" m
      <> el "h3" (esc "Rail stories")
      <> storyList "rail" r
  StoryRow ss -> storyList "stories" ss
  ProductGrid t intro ps ->
    ipt "text" "title" "Title (optional)" (orEmpty t)
      <> txtArea "intro" "Intro (optional)" (orEmpty intro)
      <> el "h3" (esc "Products")
      <> productList "products" ps
  StripeCatalog t intro ->
    elAttr "p" [("class", "muted")] (esc "This section shows your LIVE Stripe catalogue \x2014 names, prices and photos come straight from Stripe and refresh automatically. To change the products, edit them at dashboard.stripe.com \x2192 Product catalogue; there is nothing to maintain here.")
      <> ipt "text" "title" "Title above the grid (optional)" (orEmpty t)
      <> txtArea "intro" "Intro line (optional)" (orEmpty intro)
  RichColumns t cs ->
    ipt "text" "title" "Title (optional)" (orEmpty t) <> el "h3" (esc "Columns") <> textList "columns" cs
  Gallery t imgs ->
    ipt "text" "title" "Title (optional)" (orEmpty t) <> el "h3" (esc "Images") <> imageList "images" imgs
  CtaBand h b cta ->
    ipt "text" "headline" "Headline" h <> txtArea "body" "Body (optional)" (orEmpty b) <> ctaFields "cta" cta
  Contact t b email ->
    ipt "text" "title" "Title" t
      <> txtArea "body" "Body (optional)" (orEmpty b)
      <> ipt "text" "email" "Email (optional)" (orEmpty email)
      <> elAttr "p" [("class", "hint")] (esc "The address is never printed on the page \x2014 visitors see a \x201CShow email address\x201D button and get it only after an automatic spam-protection check (proof-of-work).")
  ProseSection k h b ->
    ipt "text" "kicker" "Kicker (optional)" (orEmpty k) <> ipt "text" "headline" "Heading (optional)" (orEmpty h) <> txtArea "body" "Body" b
  FooterBand t -> txtArea "text" "Footer text" t
  IncludePartial nm ->
    if null partials
      then elAttr "p" [("class", "muted")] (esc "No partials yet — create one in the Template tab, then include it here.")
      else
        elAttr "p" [("class", "muted")] (esc "Reuse a partial defined once in the Template tab. Edit the partial there; every page that includes it updates.")
          <> selField "partial" "Partial to include" [(p, p) | p <- partials] nm

-- nested editors --------------------------------------------------------------

storyList :: Text -> [Story] -> Html
storyList ln ss =
  hidden (ln <> "-count") (T.pack (show (length ss)))
    <> htmlConcat (zipWith (storyFields ln) [0 ..] ss)
    <> btn "ghost" ("add-" <> ln) "+ add story"

storyFields :: Text -> Int -> Story -> Html
storyFields ln i s =
  fieldset' ("Story " <> T.pack (show (i + 1))) $
    ipt "text" (ix ln i "kicker") "Kicker" (orEmpty (storyKicker s))
      <> ipt "text" (ix ln i "headline") "Headline" (storyHeadline s)
      <> ipt "text" (ix ln i "href") "Link (href)" (orEmpty (storyHref s))
      <> txtArea (ix ln i "dek") "Standfirst / dek" (orEmpty (storyDek s))
      <> elAttr "div" [("class", "row")] (ipt "text" (ix ln i "byline") "Byline" (orEmpty (storyByline s)) <> ipt "text" (ix ln i "timestamp") "Timestamp" (orEmpty (storyTimestamp s)))
      <> txtArea (ix ln i "body") "Body" (orEmpty (storyBody s))
      <> imgFields (ln <> "-" <> T.pack (show i) <> "-img") (storyImage s)
      <> chk (ix ln i "sticky") "Pin the image while this story is read (sticky)" (storyImageSticky s)
      <> btn "danger" ("del-" <> ln <> "-" <> T.pack (show i)) "Remove story"

productList :: Text -> [Product] -> Html
productList ln ps =
  hidden (ln <> "-count") (T.pack (show (length ps)))
    <> htmlConcat (zipWith (productFields ln) [0 ..] ps)
    <> btn "ghost" ("add-" <> ln) "+ add product"

productFields :: Text -> Int -> Product -> Html
productFields ln i p =
  let (amt, cur) = priceToInput (prodPrice p)
      BuyTarget tgt = prodBuy p
   in fieldset' ("Product " <> T.pack (show (i + 1))) $
        ipt "text" (ix ln i "name") "Name" (prodName p)
          <> txtArea (ix ln i "blurb") "Blurb" (prodBlurb p)
          <> elAttr "div" [("class", "row")] (ipt "text" (ix ln i "price") "Price (e.g. 85 or 12.50)" amt <> selField (ix ln i "currency") "Currency" currencyOptions cur)
          <> ipt "text" (ix ln i "buy") "Buy id (unique, for checkout)" tgt
          <> imgFields (ln <> "-" <> T.pack (show i) <> "-img") (prodImage p)
          <> btn "danger" ("del-" <> ln <> "-" <> T.pack (show i)) "Remove product"

textList :: Text -> [Text] -> Html
textList ln cs =
  hidden (ln <> "-count") (T.pack (show (length cs)))
    <> htmlConcat [fieldset' ("Column " <> T.pack (show (j + 1))) (txtArea (ix ln j "text") "Text" c <> btn "danger" ("del-" <> ln <> "-" <> T.pack (show j)) "Remove") | (j, c) <- zip [0 ..] cs]
    <> btn "ghost" ("add-" <> ln) "+ add column"

imageList :: Text -> [ImageRef] -> Html
imageList ln imgs =
  hidden (ln <> "-count") (T.pack (show (length imgs)))
    <> htmlConcat [fieldset' ("Image " <> T.pack (show (j + 1))) (imgInner (ix ln j) img <> btn "danger" ("del-" <> ln <> "-" <> T.pack (show j)) "Remove") | (j, img) <- zip [0 ..] imgs]
    <> btn "ghost" ("add-" <> ln) "+ add image"

linkList :: Text -> [NavLink] -> Html
linkList ln ls =
  hidden (ln <> "-count") (T.pack (show (length ls)))
    <> htmlConcat [fieldset' ("Link " <> T.pack (show (j + 1))) (elAttr "div" [("class", "row")] (ipt "text" (ix ln j "label") "Label" (navLabel l) <> ipt "text" (ix ln j "href") "Href" (navHref l)) <> btn "danger" ("del-" <> ln <> "-" <> T.pack (show j)) "Remove") | (j, l) <- zip [0 ..] ls]
    <> btn "ghost" ("add-" <> ln) "+ add link"

imgFields :: Text -> Maybe ImageRef -> Html
imgFields pfx mimg = fieldset' "Figure" (imgInner (\f -> pfx <> "-" <> f) (fromMaybe (ImageRef Nothing "" 1.5 360) mimg))

imgInner :: (Text -> Text) -> ImageRef -> Html
imgInner k img =
  ipt "text" (k "src") "Image URL (blank = a placeholder)" (orEmpty (imgSrc img))
    <> elAttr "p" [("class", "hint")] (esc "Upload photos in " <> elAttr "a" [("href", "/admin/media"), ("target", "_blank")] (esc "Media") <> esc ", then \x201C\&Copy URL\x201D and paste it here.")
    <> ipt "text" (k "caption") "Caption" (imgCaption img)
    <> elAttr "div" [("class", "row")] (ipt "number" (k "aspect") "Aspect (w/h)" (numT (imgAspect img)) <> ipt "number" (k "cap") "Max height (px)" (numT (imgCap img)))

ctaFields :: Text -> Maybe Cta -> Html
ctaFields pfx mcta =
  let Cta l h = fromMaybe (Cta "" "") mcta
   in fieldset' "Button (optional — leave label blank for none)" $
        elAttr "div" [("class", "row")] (ipt "text" (pfx <> "-label") "Label" l <> ipt "text" (pfx <> "-href") "Link" h)

-- ---------------------------------------------------------------------------
-- Theme editor
-- ---------------------------------------------------------------------------

themeEditorPage :: Theme -> Maybe Text -> Html
themeEditorPage th fl =
  adminDoc "Theme" $
    htmlConcat
      [ topbar
      , elAttr "div" [("class", "wrap"), ("style", "max-width:680px")] $
          htmlConcat
            [ flash fl
            , el "h1" (esc "Theme")
            , elAttr "p" [("class", "muted")] (esc "Colours, fonts and density. The engine handles all placement.")
            , elAttr "div" [("class", "card")] $
                elAttr "form" [("method", "post"), ("action", "/admin/theme")] $
                  htmlConcat
                    [ ipt "text" "name" "Name" (themeName th)
                    , el "h3" (esc "Palette")
                    , colorRow "bg" "Background" (themeBg th)
                    , colorRow "fg" "Text" (themeFg th)
                    , colorRow "accent" "Accent" (themeAccent th)
                    , colorRow "muted" "Muted" (themeMuted th)
                    , colorRow "border" "Rules / borders" (themeBorder th)
                    , el "h3" (esc "Typography")
                    , ipt "text" "mastFont" "Masthead font stack" (themeMastFont th)
                    , ipt "text" "displayFont" "Headline font stack" (themeDisplayFont th)
                    , ipt "text" "bodyFont" "Body font stack" (themeBodyFont th)
                    , ipt "text" "fontUrl" "Webfont URL (optional)" (orEmpty (themeFontUrl th))
                    , el "h3" (esc "Geometry")
                    , elAttr "div" [("class", "row")] $
                        selField "density" "Density" [("airy", "Airy"), ("cozy", "Cozy"), ("compact", "Compact")] (densityTag (themeDensity th))
                          <> ipt "number" "maxWidth" "Max width (px)" (T.pack (show (themeMaxWidth th)))
                          <> ipt "number" "radius" "Corner radius (px)" (T.pack (show (themeRadius th)))
                    , elAttr "div" [("class", "actions")] (elAttr "button" [("class", "primary"), ("type", "submit")] (esc "Save theme"))
                    ]
            ]
      ]
  where
    colorRow name lbl (Color c) =
      elAttr "div" [("class", "inline"), ("style", "margin:6px 0")] $
        voidElAttr "input" [("type", "color"), ("name", name <> "-pick"), ("value", hexOf c), ("oninput", "this.form." <> name <> ".value=this.value")]
          <> elAttr "div" [("style", "flex:1")] (ipt "text" name lbl c)

-- ---------------------------------------------------------------------------
-- Shop & Stripe tab
-- ---------------------------------------------------------------------------

-- | The shop tab: Stripe is the single source of truth — the live catalogue
-- (what the site downloads and shows), any content-defined products, and the
-- keys form.
stripePage :: Text -> SiteSpec -> [CatalogItem] -> SecretBundle -> Maybe Text -> Html
stripePage baseUrl site catalog bundle fl =
  adminDoc "Shop & Stripe" $
    htmlConcat
      [ topbar
      , elAttr "div" [("class", "wrap"), ("style", "max-width:760px")] $
          htmlConcat
            [ flash fl
            , el "h1" (esc "Shop & Stripe")
            , howCard
            , catalogCard
            , contentProductsCard
            , keysCard
            , webhookCard
            ]
      ]
  where
    products = siteProducts site
    skSet = isSet (sbStripeSk bundle)
    howCard =
      elAttr "div" [("class", "card")] $
        el "h2" (esc "How your shop works \x2014 Stripe is the single source of truth")
          <> elAttr "ol" [("class", "steps")] (htmlConcat
               [ elAttr "li" [] $
                   elAttr "b" [] (esc "Your products live in your Stripe catalogue.")
                     <> esc "Manage them at dashboard.stripe.com \x2192 Product catalogue: the name, the price and the photo you set there are exactly what the website shows. Add, retire or reprice a class in Stripe and the site follows automatically \x2014 nothing to edit here."
               , elAttr "li" [] $
                   elAttr "b" [] (esc "The website downloads and displays them.")
                     <> esc "Any page with a \x201CStripe catalogue\x201D section shows the live catalogue as a grid (three products = three columns), photos included, refreshed every few minutes."
               , elAttr "li" [] $
                   elAttr "b" [] (esc "Buy buttons charge the catalogue price.")
                     <> esc "Each button opens Stripe's secure checkout for that exact catalogue item \x2014 the amount is Stripe's own price, so it can never be altered. Sales, receipts and payouts all live in your Stripe dashboard."
               ])
    catalogCard =
      elAttr "div" [("class", "card")] $
        el "h2" (esc "Your Stripe catalogue (live)")
          <> ( case catalog of
                 [] ->
                   elAttr "p" [("class", "muted")] $
                     if skSet
                       then esc "Nothing came back from Stripe \x2014 either the catalogue is empty (create products at dashboard.stripe.com \x2192 Product catalogue, each with a default price) or the key below is for the wrong mode (test vs live)."
                       else esc "Connect your Secret key below and your Stripe products will appear here automatically."
                 items ->
                   elAttr "table" [("class", "plist")] $
                     elAttr "tr" [] (el "th" (esc "Product") <> el "th" (esc "Price") <> el "th" (esc "Photo") <> el "th" (esc "Price id"))
                       <> htmlConcat
                         [ elAttr "tr" [] $
                             el "td" (esc (ciName i))
                               <> el "td" (esc (fmtPrice (ciPrice i)))
                               <> el "td" (esc (maybe "\x2014" (const "from Stripe") (ciImage i)))
                               <> el "td" (elAttr "code" [] (esc (T.take 22 (ciPriceId i) <> "\x2026")))
                         | i <- items
                         ]
             )
          <> checkoutPill
    contentProductsCard
      | null products = mempty
      | otherwise =
          elAttr "div" [("class", "card")] $
            el "h2" (esc "Products defined in site content (legacy)")
              <> elAttr "p" [("class", "muted")] (esc "These are written into page sections rather than Stripe. They still work (the site sends their price to Stripe at purchase), but the catalogue above is the recommended, zero-maintenance way.")
              <> ( elAttr "table" [("class", "plist")] $
                     elAttr "tr" [] (el "th" (esc "Product") <> el "th" (esc "Price") <> el "th" (esc "Buy id") <> el "th" (esc "Defined in"))
                       <> htmlConcat
                         [ elAttr "tr" [] $
                             el "td" (esc nm)
                               <> el "td" (esc (fmtPrice pr))
                               <> el "td" (elAttr "code" [] (esc tgt))
                               <> el "td" (esc loc)
                         | (tgt, nm, pr, loc) <- products
                         ]
                 )
    checkoutPill =
      elAttr "p" [("style", "margin:14px 0 0")] $
        esc "Checkout is "
          <> ( if skSet
                 then elAttr "span" [("class", "pill on")] (esc "ready") <> esc " \x2014 buy buttons lead to a real Stripe checkout."
                 else elAttr "span" [("class", "pill off")] (esc "not connected") <> esc " \x2014 buy buttons will apologise until you paste your Secret key below."
             )
    keysCard =
      elAttr "div" [("class", "card")] $
        htmlConcat
          [ el "h2" (esc "Connect Stripe (your keys)")
          , elAttr "p" [("class", "muted")] (esc "Sign in at dashboard.stripe.com \x2192 Developers \x2192 API keys, and copy the two keys. Use the test keys (pk_test_/sk_test_) to try things out with card number 4242 4242 4242 4242, and switch to live keys when you're ready to take real payments. Keys are stored encrypted; a blank field keeps its current value, so you can rotate one at a time.")
          , statusRow "Publishable key (pk)" (isSet (sbStripePk bundle))
          , statusRow "Secret key (sk)" skSet
          , statusRow "Webhook signing secret" (isSet (sbStripeWebhook bundle))
          , elAttr "form" [("method", "post"), ("action", "/admin/secrets")] $
              ipt "text" "stripePk" "Publishable key (pk_...)" ""
                <> ipt "password" "stripeSk" "Secret key (sk_...)" ""
                <> ipt "password" "stripeWebhook" "Webhook signing secret (whsec_...)" ""
                <> elAttr "div" [("class", "actions")] (elAttr "button" [("class", "primary"), ("type", "submit")] (esc "Save keys"))
          ]
    webhookCard =
      elAttr "div" [("class", "card")] $
        htmlConcat
          [ el "h2" (esc "Webhook (optional, for payment confirmations)")
          , elAttr "p" [("class", "muted")] $
              esc "If you want this site to receive confirmation events from Stripe: in the Stripe dashboard go to Developers \x2192 Webhooks \x2192 Add endpoint, use the URL "
                <> elAttr "code" [] (esc (baseUrl <> "/stripe/webhook"))
                <> esc ", subscribe to checkout.session.completed, then paste the endpoint's signing secret (whsec_\x2026) above. Without it the shop still works \x2014 you simply rely on the Stripe dashboard and its emails to see payments."
          ]
    statusRow lbl on =
      elAttr "div" [("class", "sec")] $
        elAttr "span" [("class", "sum")] (esc lbl)
          <> elAttr "span" [("class", "pill " <> if on then "on" else "off")] (esc (if on then "configured" else "not set"))
    isSet (Just _) = True
    isSet Nothing = False

-- | Every product across pages AND partials, with where it is defined — the
-- admin-facing view of the server's buy registry.
siteProducts :: SiteSpec -> [(Text, Text, Price, Text)]
siteProducts site =
  [ (tgt, prodName p, prodPrice p, loc)
  | (loc, secs) <- containers
  , ProductGrid _ _ ps <- secs
  , p <- ps
  , let BuyTarget tgt = prodBuy p
  ]
  where
    containers =
      [("page " <> pagePath pg, pageSections pg) | pg <- sitePages site]
        <> [("partial \x201C" <> partialName pt <> "\x201D", partialSections pt) | pt <- sitePartials site]

fmtPrice :: Price -> Text
fmtPrice p = let (amt, cur) = priceToInput p in amt <> " " <> T.toUpper cur

-- ---------------------------------------------------------------------------
-- Media library (uploads)
-- ---------------------------------------------------------------------------

-- | The photo library: a multi-file drag-and-drop uploader that compresses
-- photos in the browser before they travel (canvas re-encode, capped at
-- 2000px), plus the grid of stored photos with copy-the-URL and delete.
mediaLibraryPage :: [MediaEntry] -> Maybe Text -> Html
mediaLibraryPage entries fl =
  adminDoc "Media" $
    htmlConcat
      [ topbar
      , elAttr "div" [("class", "wrap")] $
          htmlConcat
            [ flash fl
            , el "h1" (esc "Media")
            , elAttr "p" [("class", "muted")] (esc "Your photo library. Drop photos below (or click to choose several at once) \x2014 they are automatically resized and compressed for the web on your computer before uploading. Then press \x201C\&Copy URL\x201D on a photo and paste it into any section's \x201CImage URL\x201D field.")
            , uploaderCard
            , libraryCard
            , rawHtml uploaderJs
            ]
      ]
  where
    uploaderCard =
      elAttr "div" [("class", "card")] $
        htmlConcat
          [ elAttr "div" [("class", "drop"), ("id", "drop")] (esc "Drop photos here \x2014 or click to choose files")
          , voidElAttr "input" [("type", "file"), ("id", "pick"), ("multiple", "multiple"), ("accept", "image/*"), ("style", "display:none")]
          , elAttr "div" [("class", "inline"), ("style", "margin-top:10px")] $
              voidElAttr "input" [("type", "checkbox"), ("id", "docompress"), ("checked", "checked")]
                <> elAttr "label" [("for", "docompress")] (esc "Compress photos before upload (recommended)")
          , elAttr "ul" [("id", "uplog")] mempty
          ]
    libraryCard =
      elAttr "div" [("class", "card")] $
        el "h2" (esc ("Library (" <> T.pack (show (length entries)) <> ")"))
          <> ( case entries of
                 [] -> elAttr "p" [("class", "muted")] (esc "Nothing here yet \x2014 your uploaded photos will appear in this grid.")
                 es -> elAttr "div" [("class", "mediagrid")] (htmlConcat (map mediaCard es))
             )
    mediaCard e =
      let url = "/media/" <> meName e
       in elAttr "div" [("class", "mediacard")] $
            htmlConcat
              [ elAttr "a" [("href", url), ("target", "_blank")] (voidElAttr "img" [("src", url), ("loading", "lazy"), ("alt", meName e)])
              , elAttr "div" [("class", "meta")] (elAttr "code" [] (esc (meName e)) <> esc (" \xB7 " <> fmtBytes (meBytes e)))
              , elAttr "div" [("class", "bar")] $
                  elAttr "button" [("type", "button"), ("data-copy", url)] (esc "Copy URL")
                    <> elAttr "form" [("method", "post"), ("action", "/admin/media/delete")] (hidden "name" (meName e) <> btn "danger" "delete" "Delete")
              ]

fmtBytes :: Integer -> Text
fmtBytes n
  | n >= 1048576 = T.pack (show (n `div` 1048576)) <> "." <> T.pack (show ((n `mod` 1048576) * 10 `div` 1048576)) <> " MB"
  | n >= 1024 = T.pack (show (n `div` 1024)) <> " KB"
  | otherwise = T.pack (show n) <> " B"

-- | The uploader client. A fixed literal (never owner content): shrink on a
-- canvas (photos only; PNGs stay PNG so transparency survives, and only ever
-- shrink), then POST one file per request to @\/admin\/upload?json=1@.
uploaderJs :: Text
uploaderJs =
  T.concat
    [ "<script>(function(){"
    , "var drop=document.getElementById('drop'),pick=document.getElementById('pick'),log=document.getElementById('uplog');"
    , "if(!drop||!pick||!log)return;"
    , "function fmt(n){return n>1048576?(n/1048576).toFixed(1)+' MB':Math.round(n/1024)+' KB';}"
    , "function line(t){var li=document.createElement('li');li.textContent=t;log.appendChild(li);return li;}"
    , "async function shrink(f){"
    , "var doC=document.getElementById('docompress').checked;if(!doC)return f;"
    , "var jpegish=(f.type==='image/jpeg'||f.type==='image/webp'),png=(f.type==='image/png');"
    , "if(!jpegish&&!png)return f;"
    , "var img;try{img=await createImageBitmap(f);}catch(e){return f;}"
    , "var MAX=2000,sc=Math.min(1,MAX/Math.max(img.width,img.height));"
    , "if(sc===1&&(png||f.size<300*1024))return f;"
    , "var w=Math.max(1,Math.round(img.width*sc)),h=Math.max(1,Math.round(img.height*sc));"
    , "var cv=document.createElement('canvas');cv.width=w;cv.height=h;var cx=cv.getContext('2d');"
    , "if(!png){cx.fillStyle='#fff';cx.fillRect(0,0,w,h);}"
    , "cx.drawImage(img,0,0,w,h);"
    , "var type=png?'image/png':'image/jpeg';"
    , "var blob=await new Promise(function(r){cv.toBlob(r,type,0.82);});"
    , "if(blob&&blob.size<f.size){var stem=f.name.replace(/[.][A-Za-z0-9]+$/,'');"
    , "return new File([blob],stem+(png?'.png':'.jpg'),{type:type});}"
    , "return f;}"
    , "async function sendAll(files){"
    , "var list=Array.prototype.slice.call(files||[]);if(!list.length)return;var okc=0;"
    , "for(var i=0;i<list.length;i++){var f=list[i];var li=line(f.name+' \\u2014 preparing\\u2026');"
    , "try{var g=await shrink(f);"
    , "var note=(g!==f)?' (compressed '+fmt(f.size)+' \\u2192 '+fmt(g.size)+')':'';"
    , "li.textContent=f.name+' \\u2014 uploading'+note+'\\u2026';"
    , "var fd=new FormData();fd.append('file',g,g.name);"
    , "var res=await fetch('/admin/upload?json=1',{method:'POST',body:fd});"
    , "var j=await res.json();"
    , "if(j.saved&&j.saved.length){li.textContent=f.name+' \\u2014 uploaded'+note;li.className='ok';okc++;}"
    , "else{li.textContent=f.name+' \\u2014 failed: '+((j.errors&&j.errors[0])||('HTTP '+res.status));li.className='err';}"
    , "}catch(e){li.textContent=f.name+' \\u2014 failed: '+e;li.className='err';}}"
    , "if(okc){line('Refreshing the library\\u2026');setTimeout(function(){location.reload();},1200);}}"
    , "drop.addEventListener('click',function(){pick.click();});"
    , "pick.addEventListener('change',function(){sendAll(pick.files);pick.value='';});"
    , "['dragenter','dragover'].forEach(function(ev){drop.addEventListener(ev,function(e){e.preventDefault();drop.classList.add('on');});});"
    , "['dragleave','drop'].forEach(function(ev){drop.addEventListener(ev,function(e){e.preventDefault();drop.classList.remove('on');});});"
    , "drop.addEventListener('drop',function(e){if(e.dataTransfer)sendAll(e.dataTransfer.files);});"
    , "document.addEventListener('click',function(e){"
    , "var b=e.target&&e.target.closest?e.target.closest('[data-copy]'):null;if(!b)return;"
    , "var url=location.origin+b.getAttribute('data-copy');"
    , "var done=function(){b.textContent='Copied!';setTimeout(function(){b.textContent='Copy URL';},1200);};"
    , "if(navigator.clipboard&&navigator.clipboard.writeText){navigator.clipboard.writeText(url).then(done,function(){window.prompt('Copy this URL:',url);});}"
    , "else{window.prompt('Copy this URL:',url);}});"
    , "})();</script>"
    ]

-- ---------------------------------------------------------------------------
-- Pure parsers: form params -> SectionSpec / Theme
-- ---------------------------------------------------------------------------

-- | Parse a submitted section form back into a 'SectionSpec' of the same kind as
-- @old@ (the kind is fixed; the owner only edits content).
parseSection :: SectionSpec -> [(Text, Text)] -> SectionSpec
parseSection old ps = case old of
  Masthead{} -> Masthead (req "title") (opt "tagline")
  NavStrip{} -> NavStrip (links "links") (bool "sticky")
  Ribbon{} -> Ribbon (links "links")
  Hero{} -> Hero (HeroContent (opt "kicker") (req "headline") (opt "dek") (opt "body") (img "img") (cta "cta"))
  FeatureSplit{} -> FeatureSplit (dbl "rho" 0.62) (dbl "gutter" 28) (stories "main") (stories "rail")
  StoryRow{} -> StoryRow (stories "stories")
  ProductGrid{} -> ProductGrid (opt "title") (opt "intro") (products "products")
  StripeCatalog{} -> StripeCatalog (opt "title") (opt "intro")
  RichColumns{} -> RichColumns (opt "title") (texts "columns")
  Gallery{} -> Gallery (opt "title") (images "images")
  CtaBand{} -> CtaBand (req "headline") (opt "body") (cta "cta")
  Contact{} -> Contact (req "title") (opt "body") (opt "email")
  ProseSection{} -> ProseSection (opt "kicker") (opt "headline") (req "body")
  FooterBand{} -> FooterBand (req "text")
  IncludePartial{} -> IncludePartial (req "partial")
  where
    g k = lookup k ps
    req k = fromMaybe "" (g k)
    opt k = case fmap T.strip (g k) of Just v | not (T.null v) -> Just v; _ -> Nothing
    bool k = case g k of Just "on" -> True; Just "true" -> True; _ -> False
    dbl k d = maybe d id (g k >>= readD)
    count ln = maybe 0 id (g (ln <> "-count") >>= readInt)
    stories ln = [pStory ln j | j <- [0 .. count ln - 1]]
    products ln = [pProduct ln j | j <- [0 .. count ln - 1]]
    texts ln = [fromMaybe "" (g (ix ln j "text")) | j <- [0 .. count ln - 1]]
    images ln = [pImage (\f -> ix ln j f) | j <- [0 .. count ln - 1]]
    links ln = [NavLink (fromMaybe "" (g (ix ln j "label"))) (fromMaybe "/" (g (ix ln j "href"))) | j <- [0 .. count ln - 1]]
    img pfx = Just (pImage (\f -> pfx <> "-" <> f))
    cta pfx =
      let l = T.strip (fromMaybe "" (g (pfx <> "-label")))
          h = fromMaybe "" (g (pfx <> "-href"))
       in if T.null l then Nothing else Just (Cta l h)
    pStory ln j =
      Story
        { storyKicker = opt (ix ln j "kicker")
        , storyHeadline = fromMaybe "" (g (ix ln j "headline"))
        , storyHref = opt (ix ln j "href")
        , storyDek = opt (ix ln j "dek")
        , storyByline = opt (ix ln j "byline")
        , storyTimestamp = opt (ix ln j "timestamp")
        , storyImage = Just (pImage (\f -> ix ln j ("img-" <> f)))
        , storyImageSticky = bool (ix ln j "sticky")
        , storyBody = opt (ix ln j "body")
        }
    pProduct ln j =
      Product
        { prodName = fromMaybe "" (g (ix ln j "name"))
        , prodBlurb = fromMaybe "" (g (ix ln j "blurb"))
        , prodPrice = parsePrice (fromMaybe "0" (g (ix ln j "price"))) (fromMaybe "eur" (g (ix ln j "currency")))
        , prodImage = Just (pImage (\f -> ix ln j ("img-" <> f)))
        , prodBuy = BuyTarget (let t = T.strip (fromMaybe "" (g (ix ln j "buy"))) in if T.null t then "item-" <> T.pack (show j) else t)
        }
    pImage key =
      ImageRef
        { imgSrc = case fmap T.strip (g (key "src")) of Just v | not (T.null v) -> Just v; _ -> Nothing
        , imgCaption = fromMaybe "" (g (key "caption"))
        , imgAspect = maybe 1.5 id (g (key "aspect") >>= readD)
        , imgCap = maybe 360 id (g (key "cap") >>= readD)
        }

-- | Parse the theme form (field by field, falling back to the current theme).
parseTheme :: Theme -> [(Text, Text)] -> Theme
parseTheme th ps =
  th
    { themeName = req "name" (themeName th)
    , themeBg = col "bg" (themeBg th)
    , themeFg = col "fg" (themeFg th)
    , themeAccent = col "accent" (themeAccent th)
    , themeMuted = col "muted" (themeMuted th)
    , themeBorder = col "border" (themeBorder th)
    , themeMastFont = req "mastFont" (themeMastFont th)
    , themeDisplayFont = req "displayFont" (themeDisplayFont th)
    , themeBodyFont = req "bodyFont" (themeBodyFont th)
    , themeFontUrl = case fmap T.strip (lookup "fontUrl" ps) of Just v | not (T.null v) -> Just v; Just _ -> Nothing; Nothing -> themeFontUrl th
    , themeDensity = dens (lookup "density" ps)
    , themeMaxWidth = num "maxWidth" (themeMaxWidth th)
    , themeRadius = num "radius" (themeRadius th)
    }
  where
    req k d = case fmap T.strip (lookup k ps) of Just v | not (T.null v) -> v; _ -> d
    col k (Color d) = Color (req k d)
    num k d = maybe d round (lookup k ps >>= readD)
    dens (Just "airy") = Airy
    dens (Just "compact") = Compact
    dens (Just "cozy") = Cozy
    dens _ = themeDensity th

-- ---------------------------------------------------------------------------
-- Small parsing/format helpers
-- ---------------------------------------------------------------------------

readD :: Text -> Maybe Double
readD t = case reads (fixup (T.unpack (T.strip t))) of [(d, "")] -> Just d; _ -> Nothing
  where
    fixup ('.' : r) = '0' : '.' : r
    fixup ('-' : '.' : r) = '-' : '0' : '.' : r
    fixup x = x

readInt :: Text -> Maybe Int
readInt t = case reads (T.unpack (T.strip t)) of [(n, "")] -> Just n; _ -> Nothing

numT :: Double -> Text
numT d
  | d == fromIntegral r = T.pack (show r)
  | otherwise = T.pack (show d)
  where
    r = round d :: Int

currencyOptions :: [(Text, Text)]
currencyOptions = [(currencyCode c, T.toUpper (currencyCode c)) | c <- [EUR, GBP, USD, JPY]]

priceToInput :: Price -> (Text, Text)
priceToInput p =
  let c = centsInt (priceAmount' p)
      cur = priceCurrency p
      amt
        | cur == JPY = T.pack (show c)
        | otherwise = let (q, r) = c `divMod` 100 in T.pack (show q) <> "." <> pad2 r
   in (amt, currencyCode cur)
  where
    pad2 n = if n < 10 then "0" <> T.pack (show n) else T.pack (show n)
    priceAmount' (Price a _) = a

parsePrice :: Text -> Text -> Price
parsePrice amt code =
  let cur = fromMaybe EUR (currencyOfCode code)
      d = maybe 0 id (readD amt)
      n
        | cur == JPY = max 0 (round d)
        | otherwise = max 0 (round (d * 100))
   in Price (Cents n) cur

hexOf :: Text -> Text
hexOf c = if "#" `T.isPrefixOf` c && T.length c == 7 then c else "#888888"
