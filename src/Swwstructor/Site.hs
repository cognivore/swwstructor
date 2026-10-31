{-# LANGUAGE OverloadedStrings #-}

-- | The render pipeline — the one place the layout interpreter (geometry) and
-- the content interpreter (HTML) compose, generalised from the reference renderer so
-- the branding comes from a 'Theme' value rather than hard-coded CSS.
--
-- 'renderStageWith' runs @StickyWM.solveWith@ over a section's layout 'Document'
-- to get a rectangle per window, then for each placed window runs
-- 'Swwstructor.Block.HtmlBlock' on that window's 'Block' — one
-- absolutely-positioned @div@ per @(rect, html)@ pair. The two-pass measure
-- (analytic first paint, then re-pack with real DOM heights) and the sticky
-- automaton are driven by the same dependency-free client script the reference
-- app uses, faithful to @StickyWM.Sticky.stickyStep@.
module Swwstructor.Site
  ( Analytics (..)
  , pageContentWidth
  , pagePad
  , renderStage
  , renderStageWith
  , renderFullPage
  , renderStaticPage
  , renderChrome
  , simplePage
  , messagePage
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Swwstructor.Block (Block, HtmlBlock (runHtmlBlock), buildBlock, heading, paragraph)
import Swwstructor.Html
  ( Html
  , el
  , elAttr
  , esc
  , htmlConcat
  , rawHtml
  , voidElAttr
  )
import Swwstructor.Templates (Section (secBlocks, secLayout))
import Swwstructor.Theme (Theme, themeCss, themeFontLink, themeMaxWidth)
import StickyWM
  ( Ctx (Ctx)
  , Placement
  , Rect (..)
  , Viewport (..)
  , Win
  , WinType (Figure)
  , build
  , solveWith
  , totalHeight
  , wId
  , wRefs
  , wSticky
  , wType
  , winTypeTag
  )

-- | The page gutter (each side). The stage is this much narrower than the
-- viewport and centred.
pagePad :: Double
pagePad = 32

-- | The content width the layout is solved at: viewport minus gutters, floored
-- at 320 and capped at the theme's max width so the page never over-stretches.
pageContentWidth :: Theme -> Viewport -> Double
pageContentWidth th vp =
  min (fromIntegral (themeMaxWidth th)) (max 320 (vpW vp - 2 * pagePad))

-- | The solved stage with no overrides (analytic measure / first paint / no-JS).
renderStage :: Theme -> Section -> Viewport -> Html
renderStage th = renderStageWith th M.empty

-- | The solved stage, packed with measured-height overrides (window id → real
-- pixel height). The @\/layout@ POST endpoint feeds this true DOM heights so
-- packing is exact and boxes never overlap.
renderStageWith :: Theme -> Map String Double -> Section -> Viewport -> Html
renderStageWith th overrides section vp =
  let contentW = pageContentWidth th vp
      placements = solveWith (Ctx vp overrides) (build (secLayout section)) contentW
      blocks = secBlocks section
      stageH = totalHeight placements + pagePad
      boxes = htmlConcat (map (renderBox blocks) placements)
   in elAttr
        "div"
        [ ("class", "sww-stage")
        , ("style", "position:relative;height:" <> px stageH <> ";width:" <> px contentW <> ";margin:0 auto;")
        ]
        boxes

-- | A complete HTML document: theme stylesheet, the solved stage, the footer,
-- and the relayout/sticky client.
renderFullPage :: Theme -> Maybe Analytics -> Text -> Maybe Text -> Section -> Viewport -> Html
renderFullPage th an title footer section vp =
  renderDocument th an title footer (el "main" (renderStage th section vp) <> relayoutScript th)

-- | A static, self-contained render (no relayout client) — for previewing a
-- partial/header/footer in the admin, where there is no public URL for the
-- client script to re-solve against. Shows the analytic first-paint layout.
renderStaticPage :: Theme -> Text -> Section -> Viewport -> Html
renderStaticPage th title section vp =
  renderChrome th title (el "main" (renderStage th section vp))

-- | One placed window → one absolutely-positioned div whose inner HTML is the
-- 'HtmlBlock' rendering of that window's 'Block'.
renderBox :: Map String Block -> Placement -> Html
renderBox blocks (w, r) =
  let inner = case M.lookup (wId w) blocks of
        Just b -> runHtmlBlock (buildBlock b :: HtmlBlock)
        Nothing -> mempty
   in elAttr
        "div"
        ( [ ("class", "sww-win sww-win-" <> winClass w)
          , ("id", T.pack (wId w))
          , ("data-win", T.pack (wId w))
          , ("style", boxStyleFor w r)
          ]
            <> stickyAttrs w
        )
        inner

-- | Carry the sticky model to the client: which windows pin, the pin offset, and
-- the referrer ids that define the container (the client derives the release
-- point = the bottom of that container).
stickyAttrs :: Win -> [(Text, Text)]
stickyAttrs w = case wSticky w of
  Nothing -> []
  Just off ->
    [ ("data-sticky", "1")
    , ("data-sticky-offset", T.pack (show (round off :: Int)))
    , ("data-sticky-refs", T.pack (unwords (wRefs w)))
    ]

winClass :: Win -> Text
winClass = T.pack . winTypeTag . wType

-- | Figures get a FIXED height (the solved aspect box) + overflow:hidden, so
-- the photo is cover-cropped to exactly the space the solver reserved — a
-- portrait can never stretch its box over the section below. Text windows
-- keep min-height: pass 2 measures their true height and re-packs.
boxStyleFor :: Win -> Rect -> Text
boxStyleFor w r
  | wType w == Figure = boxBase r <> "height:" <> px (rh r) <> ";overflow:hidden;"
  | otherwise = boxBase r <> "min-height:" <> px (rh r) <> ";"

boxBase :: Rect -> Text
boxBase r =
  T.concat
    [ "position:absolute;"
    , "left:" <> px (rx r) <> ";"
    , "top:" <> px (ry r) <> ";"
    , "width:" <> px (rw r) <> ";"
    ]

px :: Double -> Text
px d = T.pack (show (round d :: Int)) <> "px"

-- ---------------------------------------------------------------------------
-- The document shell
-- ---------------------------------------------------------------------------

-- | A bare themed document shell (head + stylesheet + body), no footer or
-- client script — for admin pages and other non-stage chrome.
renderChrome :: Theme -> Text -> Html -> Html
renderChrome th title body =
  renderDocument th Nothing title (Just "") body

renderDocument :: Theme -> Maybe Analytics -> Text -> Maybe Text -> Html -> Html
renderDocument th an title footer body =
  rawHtml "<!DOCTYPE html>"
    <> elAttr
      "html"
      [("lang", "en")]
      ( htmlConcat
          [ el "head" (headContent th an title)
          , el "body" (body <> footerHtml footer)
          ]
      )

-- | Deployment-level analytics: the tracker to run and the domain it reports
-- as. This is /config/, not content — it arrives from the unit's environment,
-- so it never round-trips through @site.json@ and no content edit can drop it.
-- Only public pages carry it; the admin and its previews do not.
data Analytics = Analytics
  { anSrc :: !Text
  -- ^ tracker script URL (e.g. a self-hosted Plausible @\/js\/plausible.js@)
  , anDomain :: !Text
  -- ^ the @data-domain@ the tracker reports as (the site's public domain)
  , anInline :: !(Maybe Text)
  -- ^ the tracker's source, to ship /in the page/ instead of loading it from
  -- 'anSrc'. Content blockers filter the well-known script URL, not the bytes,
  -- so inlining is what makes a first-party, cookie-free counter actually count.
  }
  deriving (Eq, Show)

-- | The analytics markup: either a plain tag that loads the tracker, or — when
-- the deployment pinned a copy of it — the tracker's own source inlined.
--
-- Inlined, it needs one companion tag. A Plausible-shaped tracker reads its
-- @data-domain@ off @querySelector('[src*=\"<endpoint>\"]')@, i.e. off the
-- script element that loaded it; inline, there is no such element and every
-- event would report a null domain. So we emit the tag it looks for as a data
-- block (a non-JS @type@ makes it inert: the browser never fetches @src@, it is
-- there to be /read/), and the tracker finds its domain exactly as it expects.
analyticsTag :: Maybe Analytics -> [Html]
analyticsTag Nothing = []
analyticsTag (Just a) = case anInline a of
  Nothing -> [elAttr "script" [("defer", ""), ("data-domain", anDomain a), ("src", anSrc a)] mempty]
  Just js ->
    [ elAttr "script" [("type", "application/json"), ("data-domain", anDomain a), ("src", anSrc a)] mempty
    , elAttr "script" [] (rawHtml js)
    ]

headContent :: Theme -> Maybe Analytics -> Text -> Html
headContent th an title =
  htmlConcat
    ( [ voidElAttr "meta" [("charset", "utf-8")]
      , voidElAttr "meta" [("name", "viewport"), ("content", "width=device-width, initial-scale=1")]
      , el "title" (esc title)
      ]
        <> fontLinks
        <> analyticsTag an
        <> [elAttr "style" [] (rawHtml (themeCss th))]
    )
  where
    fontLinks = case themeFontLink th of
      Nothing -> []
      Just href ->
        [ voidElAttr "link" [("rel", "preconnect"), ("href", "https://fonts.googleapis.com")]
        , voidElAttr "link" [("rel", "preconnect"), ("href", "https://fonts.gstatic.com"), ("crossorigin", "")]
        , voidElAttr "link" [("rel", "stylesheet"), ("href", href)]
        ]

footerHtml :: Maybe Text -> Html
footerHtml Nothing = elAttr "footer" [("class", "sww-footer")] (esc "made with swwstructor \xB7 the stickywebwm layout engine")
footerHtml (Just "") = mempty
footerHtml (Just t) = elAttr "footer" [("class", "sww-footer")] (esc t)

-- | A minimal branded one-message page — used for @\/success@, @\/cancel@, and
-- friendly error pages. Reuses the Block DSL and the same shell.
simplePage :: Theme -> Maybe Analytics -> Text -> Text -> Html
simplePage th an h body = messagePage th an h (runHtmlBlock (buildBlock (paragraph body) :: HtmlBlock))

-- | Like 'simplePage' but the body is arbitrary (trusted) markup — for pages
-- that need more than one paragraph (a ticket code, a booking summary).
messagePage :: Theme -> Maybe Analytics -> Text -> Html -> Html
messagePage th an h inner =
  renderDocument th an h Nothing $
    elAttr "main" [] $
      elAttr "div" [("class", "sww-page-msg")] $
        runHtmlBlock (buildBlock (heading h) :: HtmlBlock)
          <> inner
          <> elAttr "p" [] (elAttr "a" [("href", "/"), ("class", "sww-navlink")] (esc "\x2190 home"))

-- ---------------------------------------------------------------------------
-- The client: re-solve at the real width + drive sticky
-- ---------------------------------------------------------------------------

-- | A dependency-free client that re-solves the layout at the device width
-- (server-side, via @\/layout?w=@) on load and on resize, measures true DOM
-- heights to re-pack exactly, and drives the sticky automaton — faithful to
-- @StickyWM.Sticky.stickyStep@ (three phases, hysteresis GAP). Ported from the
-- reference app with @sww-@ class names and the theme's max width.
relayoutScript :: Theme -> Html
relayoutScript th =
  rawHtml $
    T.concat
      [ "<script>(function(){var MAX=" <> T.pack (show (themeMaxWidth th)) <> ";var lastW=-1;"
      , "var bookingValues={},bookingJump=false;"
      , "function readBookingLink(){var m=/^#booking=([A-Z0-9]{4}-[A-Z0-9]{4})$/.exec(location.hash);if(m){bookingValues.code=m[1];bookingJump=true;}}readBookingLink();"
      , "document.addEventListener('input',function(e){var f=e.target;if(f.closest&&f.closest('form.sww-sched')&&f.name){bookingValues[f.name]=f.value;}});"
      , "document.addEventListener('change',function(e){var f=e.target;if(f.closest&&f.closest('form.sww-sched')&&f.name){bookingValues[f.name]=f.value;}});"
      , "function fillBooking(stage){var form=stage.querySelector('form.sww-sched');if(!form)return;"
      , "form.querySelectorAll('input').forEach(function(f){if(Object.prototype.hasOwnProperty.call(bookingValues,f.name)){if(f.type==='radio')f.checked=f.value===bookingValues[f.name];else f.value=bookingValues[f.name];}});"
      , "if(bookingJump){bookingJump=false;form.scrollIntoView({block:'start'});}}"
      , "window.addEventListener('hashchange',function(){readBookingLink();var s=document.querySelector('.sww-stage');if(s)fillBooking(s);});"
      , "function apply(html){var cur=document.querySelector('.sww-stage');if(!cur)return null;"
      , "var tmp=document.createElement('div');tmp.innerHTML=html;var next=tmp.firstElementChild;"
      , "if(next){cur.replaceWith(next);return next;}return null;}"
      , "function measure(stage){var o={};var bs=stage.querySelectorAll('.sww-win');"
      , "for(var i=0;i<bs.length;i++){var b=bs[i];var id=b.getAttribute('data-win');if(!id)continue;"
      , "if(/sww-win-(figure|mast|nav)/.test(b.className))continue;"
      , "var mh=b.style.minHeight;b.style.minHeight='0px';"
      , "o[id]=Math.ceil(b.getBoundingClientRect().height);b.style.minHeight=mh;}return o;}"
      , "var GAP=24;"
      , "function sstep1(p,pin,rel,s){"
      , "if(p==='before')return s>=pin?'pinned':'before';"
      , "if(p==='pinned'){if(s<=pin-GAP)return 'before';if(s>=rel)return 'after';return 'pinned';}"
      , "return s<=rel-GAP?'pinned':'after';}"
      -- A scroll event can jump many pixels (or the stage can be swapped in at
      -- depth, e.g. the font-ready relayout), so run the automaton to a fixed
      -- point per event — stepping once per event can strand a figure pinned
      -- over the content below it.
      , "function sstep(p,pin,rel,s){for(var k=0;k<3;k++){var q=sstep1(p,pin,rel,s);if(q===p)break;p=q;}return p;}"
      , "function setupSticky(stage){var its=[];var bs=stage.querySelectorAll('[data-sticky=\"1\"]');"
      , "for(var i=0;i<bs.length;i++){var b=bs[i];"
      , "var off=parseFloat(b.getAttribute('data-sticky-offset'))||12;"
      , "var refs=(b.getAttribute('data-sticky-refs')||'').split(/\\s+/);"
      , "var top0=parseFloat(b.style.top)||0;var left0=b.style.left;var w0=b.style.width;"
      , "var h=b.getBoundingClientRect().height;var cb=top0+h;"
      , "for(var j=0;j<refs.length;j++){var rf=refs[j]&&document.getElementById(refs[j]);"
      , "if(rf)cb=Math.max(cb,rf.offsetTop+rf.offsetHeight);}"
      -- No travel = nothing to ride (the figure is as tall as its container):
      -- pinning could only overlap neighbours, so leave it in normal flow.
      , "if(cb-h-top0>6)its.push({el:b,off:off,top0:top0,left0:left0,w0:w0,h:h,cb:cb,ph:'before'});}"
      , "if(!its.length){window.__swwFrame=null;return;}"
      , "var sdoc=stage.getBoundingClientRect().top+window.pageYOffset;"
      , "function frame(){var s=window.pageYOffset;var sl=stage.getBoundingClientRect().left;"
      , "for(var i=0;i<its.length;i++){var it=its[i];"
      , "var pin=sdoc+it.top0-it.off;var rel=sdoc+it.cb-it.h-it.off;"
      , "it.ph=sstep(it.ph,pin,rel,s);var e=it.el;"
      , "if(it.ph==='pinned'){e.style.position='fixed';e.style.top=it.off+'px';"
      , "e.style.left=(sl+(parseFloat(it.left0)||0))+'px';e.style.width=it.w0;e.setAttribute('data-pinned','1');}"
      , "else if(it.ph==='after'){e.style.position='absolute';e.style.top=(it.cb-it.h)+'px';"
      , "e.style.left=it.left0;e.style.width=it.w0;e.removeAttribute('data-pinned');}"
      , "else{e.style.position='absolute';e.style.top=it.top0+'px';e.style.left=it.left0;"
      , "e.style.width=it.w0;e.removeAttribute('data-pinned');}}}"
      , "window.__swwFrame=frame;"
      , "if(!window.__swwBound){window.__swwBound=1;var tk=0;window.addEventListener('scroll',function(){"
      , "if(tk)return;tk=requestAnimationFrame(function(){tk=0;if(window.__swwFrame)window.__swwFrame();});},{passive:true});}"
      , "frame();}"
      -- Measure -> solve -> re-measure until heights converge (<=2px drift,
      -- max 3 solves). One pass is not enough when metrics move under us —
      -- most commonly a webfont arriving after the first measure — and any
      -- under-measured box would leave the next window overlapping it.
      , "function same(a,b){if(!b)return false;for(var k in a){if(Math.abs(a[k]-(b[k]||0))>2)return false;}return true;}"
      -- Re-fill any revealed email after a stage swap (the widget in the fresh
      -- server HTML is always the unrevealed button).
      , "function finish(stage){stage.style.visibility='visible';setupSticky(stage);"
      , "fillBooking(stage);if(window.__swwFillReveal)window.__swwFillReveal();}"
      , "function settle(stage,w,prev,n){"
      , "var ov=measure(stage);"
      , "if(n>=3||same(ov,prev)){finish(stage);return;}"
      , "fetch('/layout',{method:'POST',headers:{'Content-Type':'application/json'},"
      , "body:JSON.stringify({w:w,path:location.pathname,overrides:ov})}).then(function(r){return r.text();})"
      , ".then(function(h2){var s2=apply(h2);if(s2){s2.style.visibility='hidden';settle(s2,w,ov,n+1);}else{finish(stage);}})"
      , ".catch(function(){finish(stage);});}"
      , "function relayout(force){"
      , "var w=Math.min(document.documentElement.clientWidth||window.innerWidth||1180,MAX);"
      , "if(!force&&w===lastW)return;lastW=w;"
      , "fetch('/layout?w='+w+'&path='+encodeURIComponent(location.pathname)).then(function(r){return r.text();}).then(function(h){"
      , "var stage=apply(h);if(!stage)return;stage.style.visibility='hidden';"
      , "settle(stage,w,null,0);"
      , "}).catch(function(){var s=document.querySelector('.sww-stage');if(s)finish(s);});}"
      , "var t;window.addEventListener('resize',function(){clearTimeout(t);t=setTimeout(function(){relayout(false);},200);});"
      , "var init=document.querySelector('.sww-stage');if(init)init.style.visibility='hidden';"
      , "relayout(true);"
      -- fonts.ready can resolve before any webfont was even requested, so also
      -- re-solve whenever a batch of fonts actually finishes loading.
      , "if(document.fonts){document.fonts.ready.then(function(){relayout(true);});"
      , "if(document.fonts.addEventListener){document.fonts.addEventListener('loadingdone',function(){relayout(true);});}}"
      , "})();</script>"
      ]
