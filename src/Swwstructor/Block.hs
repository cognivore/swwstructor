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
  , SlotSpec (..)
  , slotIdOf
  , groupSlotsByArea
  , slotsLeft
  , fmtSlotDay
  , addDaysIso
  , mondayOf
  , weekSessionCount
  , weekIsClosed
  , slotVisible

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

import Data.Char (isDigit)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Swwstructor.Html
  ( Html
  , el
  , elAttr
  , esc
  , htmlConcat
  , rawHtml
  , voidElAttr
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

-- | A bookable class slot: a calendar date, a time window, the area the class
-- runs in (the precise venue is typically announced closer to the date), and
-- how many students fit. 'slotTaken' is NOT owner content: it is the live
-- reservation count, injected by the server when the page is resolved — the
-- content codec never persists it (the same marker-resolution idea as the
-- Stripe catalogue).
data SlotSpec = SlotSpec
  { slotDate :: !Text
  -- ^ ISO date, @YYYY-MM-DD@
  , slotStart :: !Text
  -- ^ start of class, @HH:MM@ (site-local wall clock)
  , slotEnd :: !Text
  -- ^ end of class, @HH:MM@
  , slotArea :: !Text
  -- ^ neighbourhood/area label, e.g. @\"Islington \/ Finsbury Park\"@
  , slotCap :: !Int
  -- ^ places in the class
  , slotTaken :: !Int
  -- ^ live reservations (server-injected; never persisted)
  }
  deriving (Eq, Show)

-- | The stable id a reservation form posts for a slot: @date-HHMM@.
slotIdOf :: SlotSpec -> Text
slotIdOf s = slotDate s <> "-" <> T.filter (/= ':') (slotStart s)

-- | Places still free (never negative).
slotsLeft :: SlotSpec -> Int
slotsLeft s = max 0 (slotCap s - slotTaken s)

-- | Group slots by area, preserving first-appearance order — one fieldset per
-- area (\"week\") in the reservation form.
groupSlotsByArea :: [SlotSpec] -> [(Text, [SlotSpec])]
groupSlotsByArea = foldr ins []
  where
    ins s acc = case lookup (slotArea s) acc of
      Just _ -> [(a, if a == slotArea s then s : ss else ss) | (a, ss) <- acc]
      Nothing -> (slotArea s, [s]) : acc

-- | @\"2026-09-01\"@ → @\"Tue 1 Sep\"@ (pure, no time dependency — Sakamoto's
-- day-of-week congruence). Malformed dates render verbatim.
fmtSlotDay :: Text -> Text
fmtSlotDay iso = case isoParts iso of
  Just (y, m, d) ->
    dayName (sakamoto y m d) <> " " <> T.pack (show d) <> " " <> monthName m
  _ -> iso
  where
    dayName n = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"] !! max 0 (min 6 n)

monthName :: Int -> Text
monthName m = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"] !! max 0 (min 11 (m - 1))

isoParts :: Text -> Maybe (Int, Int, Int)
isoParts iso = case mapM readIntT (T.splitOn "-" iso) of
  Just [y, m, d] | m >= 1 && m <= 12 && d >= 1 && d <= 31 -> Just (y, m, d)
  _ -> Nothing
  where
    readIntT t = case reads (T.unpack t) of [(n :: Int, "")] -> Just n; _ -> Nothing

sakamoto :: Int -> Int -> Int -> Int
sakamoto y m d =
  let t = [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
      y' = if m < 3 then y - 1 else y
   in (y' + y' `div` 4 - y' `div` 100 + y' `div` 400 + t !! (m - 1) + d) `mod` 7

-- | @YYYY-MM-DD@ plus N days (N may be negative) — pure Gregorian arithmetic.
-- Malformed dates pass through verbatim.
addDaysIso :: Int -> Text -> Text
addDaysIso n iso = case isoParts iso of
  Just (y, m, d) -> fmt (go n y m d)
  _ -> iso
  where
    go 0 y m d = (y, m, d)
    go k y m d
      | k > 0 = if d < monthLen y m then go (k - 1) y m (d + 1) else if m < 12 then go (k - 1) y (m + 1) 1 else go (k - 1) (y + 1) 1 1
      | otherwise = if d > 1 then go (k + 1) y m (d - 1) else if m > 1 then go (k + 1) y (m - 1) (monthLen y (m - 1)) else go (k + 1) (y - 1) 12 31
    monthLen y m
      | m == 2 = if leap y then 29 else 28
      | m `elem` [4, 6, 9, 11] = 30
      | otherwise = 31
    leap y = (y `mod` 4 == 0 && y `mod` 100 /= 0) || y `mod` 400 == 0
    fmt (y, m, d) = T.pack (pad 4 y <> "-" <> pad 2 m <> "-" <> pad 2 d)
    pad w v = let s = show v in replicate (w - length s) '0' <> s

-- | The Monday that starts the (ISO, Monday-based) week a date falls in —
-- the calendar's week key.
mondayOf :: Text -> Text
mondayOf iso = case isoParts iso of
  Just (y, m, d) -> addDaysIso (negate ((sakamoto y m d + 6) `mod` 7)) iso
  _ -> iso

-- | Sessions already underway in a week: distinct slots with at least one
-- reservation. Adding a student to one of these costs the teacher nothing;
-- a booking on an empty slot opens a NEW session.
weekSessionCount :: [SlotSpec] -> Text -> Int
weekSessionCount slots wk =
  length [() | s <- slots, mondayOf (slotDate s) == wk, slotTaken s > 0]

-- | Whether a week has hit the per-week session cap (0 = no cap): no NEW
-- sessions may open, though started ones can still fill.
weekIsClosed :: Int -> [SlotSpec] -> Text -> Bool
weekIsClosed maxWeekly slots wk =
  maxWeekly > 0 && weekSessionCount slots wk >= maxWeekly

-- | Whether a slot is offered to students right now: it has places, and it
-- is not an unopened slot in a week that already teaches at capacity. A slot
-- an individual (whole-class) ticket claimed has no places, so it simply
-- disappears.
slotVisible :: Int -> [SlotSpec] -> SlotSpec -> Bool
slotVisible maxWeekly slots s =
  slotsLeft s > 0
    && not (slotTaken s == 0 && weekIsClosed maxWeekly slots (mondayOf (slotDate s)))

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

  -- | The owner's email address, protected: renders a reveal widget whose
  -- client must solve a server-issued proof-of-work before the address is
  -- fetched. The address itself NEVER enters a block or the served HTML —
  -- harvesters reading the page (or the content JSON routes) get nothing.
  emailReveal :: repr

  -- | The ticket-redemption form: a month-grid calendar of slots (live
  -- \"spots free\" on each date; full or week-capped slots disappear), then
  -- ticket code + checkout email + name, POSTing to @\/schedule@. Plain
  -- HTML — no client JS, so it survives the relayout stage swap by
  -- construction (form-post-and-navigate, like 'buyButton'). The 'Int' is
  -- the per-week session cap (0 = none).
  scheduleForm :: Int -> [SlotSpec] -> repr

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
  | BEmailReveal
  | BScheduleForm !Int ![SlotSpec]
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
  emailReveal = BEmailReveal
  scheduleForm = BScheduleForm
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
buildBlock BEmailReveal = emailReveal
buildBlock (BScheduleForm mw slots) = scheduleForm mw slots
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
  subhead t = HtmlBlock $ markdownProse "sww-subhead" t
  kicker t = HtmlBlock $ elAttr "span" [("class", "sww-kicker")] (esc t)
  paragraph t = HtmlBlock $ markdownProse "sww-paragraph" t
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
  emailReveal =
    HtmlBlock $
      elAttr "div" [("class", "sww-reveal"), ("data-sww-reveal", "1")] (
        elAttr "button" [("class", "sww-buy sww-reveal-btn"), ("type", "button")] (esc "Show email address")
          <> elAttr "noscript" [] (esc "Please enable JavaScript to see the address (spam protection).")
      )
        <> rawHtml powClientJs
  scheduleForm maxWeekly slots =
    HtmlBlock $
      elAttr "form" [("class", "sww-sched"), ("method", "post"), ("action", "/schedule")] $
        calendar
          <> (if anyVisible then mempty else elAttr "p" [("class", "sww-schednote sww-schedsoldout")] (esc "Every class is now fully booked \x2014 get in touch and we'll find you a date."))
          <> elAttr "p" [("class", "sww-schednote")] (esc "The exact address is emailed about a week before each class \x2014 for now, pick the week and area that suit you.")
          <> elAttr "div" [("class", "sww-schedfields")] (
               field "Ticket code (from your booking email)" [("name", "code"), ("required", "required"), ("placeholder", "e.g. K7Q2-M4XW"), ("autocomplete", "off")]
                 <> field "Email used at checkout" [("type", "email"), ("name", "email"), ("required", "required"), ("autocomplete", "email")]
                 <> field "Your name" [("name", "name"), ("required", "required"), ("autocomplete", "name")]
             )
          <> elAttr "button" [("class", "sww-buy sww-schedsubmit"), ("type", "submit")] (esc "Reserve my place")
    where
      field lbl attrs =
        elAttr "label" [("class", "sww-schedfield")] (elAttr "span" [] (esc lbl) <> voidElAttr "input" attrs)
      visible = slotVisible maxWeekly slots
      anyVisible = any visible slots
      -- One grid row per calendar week (Monday-based), in date order.
      mondays = dedupe (map (mondayOf . slotDate) slots)
      dedupe = foldr (\x acc -> if x `elem` acc then acc else x : acc) []
      calendar =
        elAttr "div" [("class", "sww-cal")] $
          dowHeader <> htmlConcat (map weekBlock mondays)
      dowHeader =
        elAttr "div" [("class", "sww-caldows")] $
          htmlConcat [elAttr "span" [] (esc d) | d <- ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]]
      weekSlots wk = [s | s <- slots, mondayOf (slotDate s) == wk]
      weekBlock wk =
        let ss = weekSlots wk
            areas = T.intercalate " \xB7 " (dedupe (map slotArea ss))
            range = case map slotDate ss of
              [] -> ""
              ds -> fmtSlotDay (minimum ds) <> " \x2013 " <> fmtSlotDay (maximum ds)
            closedNote
              | weekIsClosed maxWeekly slots wk && not (any visible ss) = " \xB7 fully booked"
              | weekIsClosed maxWeekly slots wk = " \xB7 last places"
              | otherwise = ""
         in elAttr "div" [("class", "sww-calweekhead")] (esc (areas <> " \xB7 " <> range) <> elAttr "em" [] (esc closedNote))
              <> elAttr "div" [("class", "sww-calgrid")] (htmlConcat [dayCell (addDaysIso i wk) | i <- [0 .. 6]])
      dayCell d =
        let ss = [s | s <- slots, slotDate s == d]
            offered = filter visible ss
            dayNum = T.dropWhile (== '0') (T.takeWhileEnd (/= '-') d)
            cellClass = if null offered then "sww-calday sww-calday-off" else "sww-calday"
         in elAttr "div" [("class", cellClass)] $
              elAttr "span" [("class", "sww-caldate")] (esc dayNum)
                <> htmlConcat (map slotChip offered)
      slotChip s =
        let left = slotsLeft s
            freeLabel
              | left == 1 = "1 spot free"
              | otherwise = T.pack (show left) <> " spots free"
         in elAttr "label" [("class", "sww-calslot")] $
              voidElAttr "input" [("type", "radio"), ("name", "slot"), ("value", slotIdOf s), ("required", "required")]
                <> elAttr "span" [("class", "sww-calslottime")] (esc (slotStart s <> "\x2013" <> slotEnd s))
                <> elAttr "em" [("class", "sww-calslotfree")] (esc freeLabel)
  ruleLine = HtmlBlock $ elAttr "hr" [("class", "sww-rule")] mempty

-- | The proof-of-work reveal client — a fixed literal (never owner content),
-- bound once per page however many widgets render. Solves the server's
-- sha256 leading-zero-bits challenge in concurrent WebCrypto batches
-- (roughly a second of work), then swaps the widget for the mailto link.
powClientJs :: Text
powClientJs =
  T.concat
    [ "<script>(function(){"
    , "if(window.__swwPowBound)return;window.__swwPowBound=1;"
    , "window.__swwFillReveal=function(){var em=window.__swwRevealedEmail;if(!em)return;"
    , "var all=document.querySelectorAll('[data-sww-reveal]');"
    , "for(var q=0;q<all.length;q++){var bx=all[q];var a=document.createElement('a');"
    , "a.className='sww-link';a.href='mailto:'+em;a.textContent=em;"
    , "while(bx.firstChild)bx.removeChild(bx.firstChild);bx.appendChild(a);}};"
    , "async function sha(s){var b=new TextEncoder().encode(s);"
    , "var d=await crypto.subtle.digest('SHA-256',b);return new Uint8Array(d);}"
    , "function zbits(a){var n=0;for(var i=0;i<a.length;i++){var x=a[i];"
    , "if(x===0){n+=8;continue;}while(!(x&128)){n++;x=(x<<1)&255;}break;}return n;}"
    , "async function solve(ch,bits,onp){var nonce=0;for(;;){"
    , "var batch=[];for(var j=0;j<128;j++)batch.push(sha(ch+':'+(nonce+j)));"
    , "var rs=await Promise.all(batch);"
    , "for(var k=0;k<rs.length;k++){if(zbits(rs[k])>=bits)return nonce+k;}"
    , "nonce+=128;if(onp&&nonce%2048===0)onp(nonce);}}"
    , "document.addEventListener('click',async function(e){"
    , "var b=e.target&&e.target.closest?e.target.closest('.sww-reveal-btn'):null;if(!b)return;"
    , "var box=b.closest('[data-sww-reveal]');if(!box)return;"
    , "b.disabled=true;var t0=b.textContent;b.textContent='One moment\\u2026';"
    , "try{"
    , "var c=await fetch('/email-challenge').then(function(r){return r.json();});"
    , "var nonce=await solve(c.challenge,c.bits,function(n){b.textContent='One moment\\u2026';});"
    , "var r=await fetch('/email-reveal',{method:'POST',headers:{'Content-Type':'application/json'},"
    , "body:JSON.stringify({challenge:c.challenge,nonce:''+nonce})});"
    , "var j=await r.json();"
    -- Remember the revealed address and (re-)fill every live widget: the
    -- relayout client swaps the whole stage on resize / font arrival, which
    -- would otherwise put the button back after a successful reveal. The
    -- relayout calls window.__swwFillReveal after every swap.
    , "if(j&&j.email){window.__swwRevealedEmail=j.email;window.__swwFillReveal();}"
    , "else{b.textContent='Could not verify \\u2014 tap to retry';b.disabled=false;}"
    , "}catch(err){b.textContent=t0;b.disabled=false;}"
    , "});})();</script>"
    ]

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
-- Markdown (the small subset an owner types into a prose box)
-- ---------------------------------------------------------------------------

-- | Render an owner's prose as HTML. A blank line starts a new paragraph, a
-- single newline is a line break, lines opening with @-@ \/ @*@ \/ @+@ (or
-- @1.@) become a list, and @[text](url)@, @**bold**@ and @*italic*@ work
-- inline; a backslash escapes any of @\\ [ ] * -@. Everything else is literal
-- text. Every scrap still goes through 'esc' and every href through 'safeHref',
-- so this stays as injection-safe as the plain 'esc' it replaces.
--
-- The @cls@ is the class the paragraphs carry, so the same renderer serves both
-- the body ('paragraph') and the standfirst ('subhead').
markdownProse :: Text -> Text -> Html
markdownProse cls = htmlConcat . map (mdBlockHtml cls) . mdGroup . T.lines . T.replace "\r" ""

-- | A paragraph (its lines) or a list (ordered?, its items).
data MdBlock = MdPara [Text] | MdList Bool [Text]

mdGroup :: [Text] -> [MdBlock]
mdGroup [] = []
mdGroup (l : ls)
  | T.null (T.strip l) = mdGroup ls
  | Just (ord, item) <- mdBullet l =
      let (items, rest) = spanItems ord ls
       in MdList ord (item : items) : mdGroup rest
  | otherwise =
      let (cont, rest) = break (\x -> T.null (T.strip x) || isBullet x) ls
       in MdPara (l : cont) : mdGroup rest
  where
    isBullet x = case mdBullet x of Just _ -> True; Nothing -> False
    spanItems ord (x : xs) | Just (o, i) <- mdBullet x, o == ord =
      let (is, rest) = spanItems ord xs in (i : is, rest)
    spanItems _ xs = ([], xs)

-- | @Just (ordered?, the item's text)@ when the line opens a list item.
mdBullet :: Text -> Maybe (Bool, Text)
mdBullet ln
  | any (`T.isPrefixOf` t) ["- ", "* ", "+ "] = Just (False, T.stripStart (T.drop 2 t))
  | not (T.null digits), Just item <- T.stripPrefix ". " afterDigits = Just (True, T.stripStart item)
  | not (T.null digits), Just item <- T.stripPrefix ") " afterDigits = Just (True, T.stripStart item)
  | otherwise = Nothing
  where
    t = T.stripStart ln
    (digits, afterDigits) = T.span isDigit t

mdBlockHtml :: Text -> MdBlock -> Html
mdBlockHtml cls (MdPara ls) =
  elAttr "p" [("class", cls)] (htmlConcat (intersperse (rawHtml "<br>") (map mdInline ls)))
mdBlockHtml _ (MdList ord items) =
  elAttr (if ord then "ol" else "ul") [("class", "sww-list")] (htmlConcat (map (el "li" . mdInline) items))

-- | The inline pass: literal text up to the next marker, then that marker.
-- Every branch consumes at least the marker character, so this terminates.
mdInline :: Text -> Html
mdInline t =
  let (plain, rest) = T.break (`T.elem` "\\[*") t
   in esc plain <> case T.uncons rest of
        Nothing -> mempty
        Just ('\\', more) -> case T.uncons more of
          Just (c, more') | c `T.elem` "\\[]*-" -> esc (T.singleton c) <> mdInline more'
          _ -> esc "\\" <> mdInline more
        Just ('[', _) -> case mdLink rest of
          Just h -> h
          Nothing -> esc "[" <> mdInline (T.drop 1 rest)
        Just ('*', _)
          | "**" `T.isPrefixOf` rest -> emph "strong" "**" (T.drop 2 rest)
          | otherwise -> emph "em" "*" (T.drop 1 rest)
        Just (c, more) -> esc (T.singleton c) <> mdInline more
  where
    -- An unclosed (or empty) run is not emphasis — the marker stays literal.
    emph tag close body = case T.breakOn close body of
      (inner, r)
        | not (T.null inner) && not (T.null r) ->
            el tag (mdInline inner) <> mdInline (T.drop (T.length close) r)
      _ -> esc close <> mdInline body

-- | @[label](href)@ at the head of the text, plus whatever follows it.
mdLink :: Text -> Maybe Html
mdLink t = do
  let (label, afterLabel) = T.breakOn "]" (T.drop 1 t)
  afterOpen <- T.stripPrefix "](" afterLabel
  let (href, afterHref) = T.breakOn ")" afterOpen
  if T.null label || T.null afterHref
    then Nothing
    else
      Just $
        elAttr "a" [("class", "sww-link"), ("href", safeHref (T.strip href))] (mdInline label)
          <> mdInline (T.drop 1 afterHref)

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
  emailReveal = PlainBlock "(email address \x2014 revealed on the website)"
  scheduleForm _ slots =
    PlainBlock ("(schedule a class \x2014 " <> T.pack (show (length slots)) <> " slots)")
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
  BEmailReveal -> tagged "emailReveal" []
  BScheduleForm mw slots -> tagged "scheduleForm" [("maxWeekly", JNum (fromIntegral mw)), ("slots", JArr (map slotJSON slots))]
  BRuleLine -> tagged "ruleLine" []
  where
    tagged k extra = JObj (("b", JStr k) : extra)
    -- The block codec is a render-state codec, so 'slotTaken' rides along
    -- (unlike the content codec, which never persists it).
    slotJSON s =
      JObj
        [ ("date", JStr (T.unpack (slotDate s)))
        , ("start", JStr (T.unpack (slotStart s)))
        , ("end", JStr (T.unpack (slotEnd s)))
        , ("area", JStr (T.unpack (slotArea s)))
        , ("cap", JNum (fromIntegral (slotCap s)))
        , ("taken", JNum (fromIntegral (slotTaken s)))
        ]

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
    "emailReveal" -> Right BEmailReveal
    "scheduleForm" -> do
      arr <- jarr' =<< v .: "slots"
      let mw = case v .:? "maxWeekly" of
            Just (JNum n) -> round n
            _ -> 0
      BScheduleForm mw <$> mapM slotOf arr
    "ruleLine" -> Right BRuleLine
    other -> Left ("unknown block kind: " ++ other)
  where
    txt key = T.pack <$> (jstr =<< v .: key)
    jarr' (JArr xs) = Right xs
    jarr' _ = Left "expected array"
    slotOf sv = do
      d <- T.pack <$> (jstr =<< sv .: "date")
      st <- T.pack <$> (jstr =<< sv .: "start")
      en <- T.pack <$> (jstr =<< sv .: "end")
      area <- T.pack <$> (jstr =<< sv .: "area")
      cap <- jnum =<< sv .: "cap"
      let taken = case sv .:? "taken" of
            Just (JNum n) -> round n
            _ -> 0
      pure (SlotSpec d st en area (round cap) taken)

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
