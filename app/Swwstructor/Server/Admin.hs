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
    ]

topbar :: Html
topbar =
  elAttr "div" [("class", "topbar")] $
    htmlConcat
      [ elAttr "span" [("class", "brand")] (esc "swwstructor")
      , elAttr "a" [("href", "/admin")] (esc "Pages")
      , elAttr "a" [("href", "/admin/template")] (esc "Template")
      , elAttr "a" [("href", "/admin/theme")] (esc "Theme")
      , elAttr "a" [("href", "/admin/stripe")] (esc "Stripe")
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
                then elAttr "p" [("class", "muted")] (esc "These sections are reusable — included via \x201CInclude a partial\x201D, and (for \x201Cheader\x201D/\x201Cfooter\x201D) auto-applied to every page.")
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
            , elAttr "p" [("class", "muted")] (esc "Reusable blocks defined once. A partial named \x201Cheader\x201D appears at the top of every page and \x201Cfooter\x201D at the bottom — automatically. Include any other partial on a page with the \x201CInclude a partial\x201D section: define your products (or anything) once and show them on as many pages as you like.")
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
  RichColumns t cs ->
    ipt "text" "title" "Title (optional)" (orEmpty t) <> el "h3" (esc "Columns") <> textList "columns" cs
  Gallery t imgs ->
    ipt "text" "title" "Title (optional)" (orEmpty t) <> el "h3" (esc "Images") <> imageList "images" imgs
  CtaBand h b cta ->
    ipt "text" "headline" "Headline" h <> txtArea "body" "Body (optional)" (orEmpty b) <> ctaFields "cta" cta
  Contact t b email ->
    ipt "text" "title" "Title" t <> txtArea "body" "Body (optional)" (orEmpty b) <> ipt "text" "email" "Email (optional)" (orEmpty email)
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
-- Stripe tab
-- ---------------------------------------------------------------------------

stripePage :: SecretBundle -> Maybe Text -> Html
stripePage bundle fl =
  adminDoc "Stripe keys" $
    htmlConcat
      [ topbar
      , elAttr "div" [("class", "wrap"), ("style", "max-width:620px")] $
          htmlConcat
            [ flash fl
            , el "h1" (esc "Stripe keys")
            , elAttr "p" [("class", "muted")] (esc "Use TEST keys for demos. Stored encrypted at rest; blank fields keep the existing value.")
            , elAttr "div" [("class", "card")] $
                htmlConcat
                  [ statusRow "Publishable key (pk)" (isSet (sbStripePk bundle))
                  , statusRow "Secret key (sk)" (isSet (sbStripeSk bundle))
                  , statusRow "Webhook signing secret" (isSet (sbStripeWebhook bundle))
                  , elAttr "form" [("method", "post"), ("action", "/admin/secrets")] $
                      ipt "text" "stripePk" "Publishable key (pk_...)" ""
                        <> ipt "password" "stripeSk" "Secret key (sk_...)" ""
                        <> ipt "password" "stripeWebhook" "Webhook signing secret (whsec_...)" ""
                        <> elAttr "div" [("class", "actions")] (elAttr "button" [("class", "primary"), ("type", "submit")] (esc "Save keys"))
                  , elAttr "p" [("class", "muted"), ("style", "margin-top:14px")] (rawHtml "Webhook endpoint: " <> elAttr "code" [] (esc "/stripe/webhook"))
                  ]
            ]
      ]
  where
    statusRow lbl on =
      elAttr "div" [("class", "sec")] $
        elAttr "span" [("class", "sum")] (esc lbl)
          <> elAttr "span" [("class", "pill " <> if on then "on" else "off")] (esc (if on then "configured" else "not set"))
    isSet (Just _) = True
    isSet Nothing = False

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
