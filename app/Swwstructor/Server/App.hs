{-# LANGUAGE OverloadedStrings #-}

-- | The scotty host. Serves any page of the /live/ site (held in a writable
-- store, edited through the admin), the two-pass @\/layout@ re-solve, Stripe
-- Checkout via @\/buy\/:id@, webhooks, and the admin content editor
-- (@\/admin\/*@): dashboard, page editor, per-section forms, theme, Stripe keys.
-- The owner writes content; the engine places it. Route order matters: every
-- specific route precedes the single-segment catch-all @\/:slug@.
module Swwstructor.Server.App
  ( Stage (..)
  , AppEnv (..)
  , swwApp
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteArray as BA
import Data.Bits (testBit)
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', readIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word8)
import Network.HTTP.Client (Manager)
import Network.HTTP.Types.Status (status303, status400, status404, status500, status503)
import Network.Wai (rawPathInfo)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.RequestSizeLimit
  ( defaultRequestSizeLimitSettings
  , requestSizeLimitMiddleware
  , setMaxLengthForRequest
  )
import Network.Wai.Parse (FileInfo (fileContent, fileName))
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Web.Scotty
  ( ActionM
  , ScottyM
  , body
  , captureParam
  , file
  , files
  , formParamMaybe
  , formParams
  , get
  , header
  , html
  , middleware
  , post
  , queryParamMaybe
  , raw
  , redirect
  , setHeader
  , status
  , text
  )

import Swwstructor.Checkout
  ( CheckoutItem (ItemAdHoc, ItemPrice)
  , CheckoutRequest (CheckoutRequest)
  , CheckoutSession (csUrl)
  , LineItem (liName)
  , Url (Url)
  )
import Swwstructor.Content
  ( Availability (OnSale)
  , PageSpec (PageSpec, pagePath, pageSections, pageTitle)
  , Partial (partialSections)
  , SectionSpec (Contact, ScheduleClass, StripeCatalog)
  , SiteSpec (siteFooter, sitePages, sitePartials, siteTheme)
  , SlotSpec (slotArea, slotDate, slotEnd, slotStart, slotTaken)
  , allSectionsOf
  , slotIdOf
  )
import Swwstructor.Edit
  ( addListItem
  , addPage
  , addPartial
  , addSection
  , containerTitle
  , deleteListItem
  , deletePage
  , deletePartial
  , deleteSection
  , isPartialPath
  , moveSection
  , partialNames
  , sectionAt
  , sectionsOf
  , setPageTitle
  , setSectionAt
  , setSiteMeta
  , setTheme
  )
import Swwstructor.Html (Html, el, elAttr, esc, htmlConcat, renderHtml)
import Swwstructor.Media (contentTypeFor, isSafeMediaName)
import Swwstructor.Money (Quantity (Quantity))
import Swwstructor.Plugins.Stripe (siteBuyRegistry)
import Swwstructor.Plugins.StripeCatalog (CatalogItem (ciAvailability, ciName, ciPriceId, ciSeats), catalogSection, isPriceId)
import Swwstructor.Server.Catalog (CatalogCache, getCatalog)
import Swwstructor.Server.Admin
  ( adminLoginPage
  , appendGeneratedSlots
  , bookingsPage
  , dashboardPage
  , mediaLibraryPage
  , pageEditorPage
  , parseSection
  , parseTheme
  , sectionFormPage
  , stripePage
  , templatePage
  , themeEditorPage
  , venuesPage
  )
import Swwstructor.Block (fmtSlotDay)
import Swwstructor.Server.Booking
  ( BookingStore
  , RedeemError (AlreadyRedeemed, CodeNotFound, EmailMismatch, SlotFull, SlotUnknown, WeekFull)
  , Redemption (redSlotId)
  , Ticket (tkCode, tkProduct, tkRedemption)
  , issueTicket
  , listTickets
  , redeemTicket
  , takenFor
  )
import Swwstructor.Server.Media (deleteMedia, listAssets, listMedia, resolveMedia, saveMedia)
import Swwstructor.Server.Notify
  ( BookingCtx (bcVenueNoun)
  , ensureReservationSideEffects
  , notifyTicketIssued
  , siteMaxWeekly
  , siteSlots
  )
import Swwstructor.Server.Venues
  ( Venue (Venue)
  , VenueStore
  , deleteVenue
  , listVenues
  , upsertVenue
  )
import Swwstructor.Server.SecretStore
  ( MasterKey
  , SecretBundle (..)
  , SecretValue (SecretValue)
  , genSessionSeed
  , hmacTokenHex
  , loadBundle
  , saveBundle
  , secretText
  )
import Swwstructor.Server.Store (SiteStore, modifySite, readSite)
import Swwstructor.Server.Stripe
  ( SessionInfo (siEmail, siId, siPaid, siProduct, siSeats)
  , peekEventType
  , stripeCreate
  , stripeGetSession
  , verifyStripeSig
  , webhookSessionOf
  )
import Swwstructor.Site (Analytics, messagePage, renderFullPage, renderStageWith, renderStaticPage, simplePage)
import Swwstructor.Templates (Section (secLayout), expandSection, renderSections, resolvedSection)
import Swwstructor.Theme (Theme)
import StickyWM
  ( JSON (JArr, JNum, JObj, JStr)
  , Viewport (Viewport)
  , decode
  , documentToJSON
  , encode
  , jnum
  , jstr
  , (.:)
  , (.:?)
  )

data Stage = Test | Prod
  deriving (Eq, Show)

-- | Everything a request handler needs. The site itself lives in 'aeStore'
-- (mutable + persisted), so content edits are immediately live.
data AppEnv = AppEnv
  { aeStore :: !SiteStore
  , aeBaseUrl :: !Text
  , aeStage :: !Stage
  , aeSecretFile :: !FilePath
  , aeMasterKey :: !MasterKey
  , aeSession :: !(IORef Integer)
  , aeAdminPassword :: !Text
  , aeManager :: !Manager
  , aeSiteDir :: !FilePath
  -- ^ the deployed content template dir (its @assets/@ are served read-only)
  , aeDataDir :: !FilePath
  -- ^ the writable state dir (secrets.enc, site.json, @uploads/@)
  , aeCatalog :: !CatalogCache
  -- ^ the cached live Stripe catalogue (for @stripeCatalog@ sections)
  , aeSpentPow :: !(IORef (Map Text UTCTime))
  -- ^ proof-of-work challenges already redeemed (challenge -> issue time),
  -- pruned as they expire — each challenge reveals the email at most once
  , aeBookings :: !BookingStore
  -- ^ tickets + reservations (for @scheduleClass@ sections)
  , aeBookingCtx :: !BookingCtx
  -- ^ the notification context (calendar invites + booking mail)
  , aeVenues :: !VenueStore
  -- ^ the venue address book (admin-only; feeds venue announcements)
  , aeAnalytics :: !(Maybe Analytics)
  -- ^ the analytics tag public pages carry (deployment config, not content)
  }

currentSite :: AppEnv -> ActionM SiteSpec
currentSite env = liftIO (readSite (aeStore env))

-- | The site with dynamic sections resolved: every 'StripeCatalog' marker is
-- replaced with a live 'ProductGrid' built from the owner's Stripe catalogue,
-- and every 'ScheduleClass' section gets its live reservation counts. Public
-- rendering uses THIS; the admin editor sees the raw markers (it edits their
-- title/intro/slots, never the injected live data).
currentSiteResolved :: AppEnv -> ActionM SiteSpec
currentSiteResolved env = do
  site0 <- currentSite env
  site1 <-
    if not (anySection isCatalogMarker site0)
      then pure site0
      else do
        items <- liftIO (fetchCatalogFor env)
        if null items
          then pure site0 -- template renders the book-by-email fallback
          else pure (mapAllSections (swapMarker items) site0)
  if not (anySection isSchedule site1)
    then pure site1
    else do
      tickets <- liftIO (listTickets (aeBookings env))
      pure (mapAllSections (fillSchedule tickets) site1)
  where
    isCatalogMarker StripeCatalog{} = True
    isCatalogMarker _ = False
    swapMarker items (StripeCatalog t i) = catalogSection t i items
    swapMarker _ s = s
    isSchedule ScheduleClass{} = True
    isSchedule _ = False
    fillSchedule tickets (ScheduleClass t i mw slots) =
      ScheduleClass t i mw [s {slotTaken = takenFor tickets s} | s <- slots]
    fillSchedule _ s = s

fetchCatalogFor :: AppEnv -> IO [CatalogItem]
fetchCatalogFor env =
  getCatalog
    (aeCatalog env)
    (aeManager env)
    (sbStripeSk <$> loadBundle (aeSecretFile env) (aeMasterKey env))

anySection :: (SectionSpec -> Bool) -> SiteSpec -> Bool
anySection p site =
  any (any p . pageSections) (sitePages site)
    || any (any p . partialSections) (sitePartials site)

mapAllSections :: (SectionSpec -> SectionSpec) -> SiteSpec -> SiteSpec
mapAllSections f site =
  site
    { sitePages = [pg {pageSections = map f (pageSections pg)} | pg <- sitePages site]
    , sitePartials = [pt {partialSections = map f (partialSections pt)} | pt <- sitePartials site]
    }

pageByPath :: SiteSpec -> Text -> Maybe PageSpec
pageByPath site p = lookup (norm p) [(norm (pagePath pg), pg) | pg <- sitePages site]
  where
    norm x = if x == "/" then "/" else "/" <> T.dropWhile (== '/') x

-- ---------------------------------------------------------------------------
-- The application
-- ---------------------------------------------------------------------------

swwApp :: AppEnv -> ScottyM ()
swwApp env = do
  -- Bound request bodies before any parser sees them (DoS guard for the
  -- unauthenticated /layout and /stripe/webhook endpoints). The admin uploader
  -- gets a larger budget; everything else stays tight.
  middleware
    ( requestSizeLimitMiddleware
        ( setMaxLengthForRequest
            ( \req ->
                pure . Just $
                  if rawPathInfo req == "/admin/upload"
                    then 64 * 1024 * 1024
                    else 1024 * 1024
            )
            defaultRequestSizeLimitSettings
        )
    )
  -- Answer HEAD as GET-minus-body (uptime checks, link unfurlers).
  middleware autohead

  get "/health" (text "ok")

  get "/" (servePage env "/")

  -- Owner-uploaded media (the admin's photo library) and, as a fallback when
  -- no reverse proxy serves them, the deployed template's static assets.
  get "/media/:file" (serveMediaFile env)
  get "/assets/:file" (serveAssetFile env)

  get "/layout" $ do
    mw <- queryParamMaybe "w"
    mp <- queryParamMaybe "path"
    serveStage env (fromMaybe "/" mp) (saneW (fromMaybe 1180 mw)) M.empty

  post "/layout" $ do
    raw_ <- body
    let (w, p, overrides) = parseRelayout (TL.unpack (TLE.decodeUtf8 raw_))
    serveStage env p (saneW w) overrides

  -- The email-protection pair: a signed challenge, and the reveal that
  -- requires its proof-of-work. The address appears in no served page.
  get "/email-challenge" (handleEmailChallenge env)
  post "/email-reveal" (handleEmailReveal env)

  post "/buy/:id" (handleBuy env)
  get "/success" (handleSuccess env)
  get "/cancel" $ do
    site <- currentSite env
    htmlOf (simplePage (siteTheme site) (aeAnalytics env) "Checkout cancelled" "No charge was made — you can return any time.")

  post "/schedule" (handleSchedule env)

  post "/stripe/webhook" (handleWebhook env)

  -- Admin: auth
  get "/admin/login" $ do
    site <- currentSite env
    htmlOf (adminLoginPage (siteTheme site) Nothing)
  post "/admin/login" (handleLogin env)
  get "/admin/logout" $ do
    liftIO (modifyIORef' (aeSession env) (+ 1)) -- revoke every live session cookie
    setHeader "Set-Cookie" (TL.fromStrict (clearCookie env))
    redirect "/admin/login"

  -- Admin: content editor (all gated)
  get "/admin" (adminGate env (dashboard env Nothing))
  post "/admin/site" (adminGate env (handleSiteMeta env))
  post "/admin/page/new" (adminGate env (handlePageNew env))
  post "/admin/page/delete" (adminGate env (handlePageDelete env))
  post "/admin/page/title" (adminGate env (handlePageTitle env))
  get "/admin/page" (adminGate env (handlePageEditor env))
  post "/admin/section/new" (adminGate env (handleSectionNew env))
  get "/admin/section" (adminGate env (handleSectionForm env))
  post "/admin/section" (adminGate env (handleSectionSave env))
  post "/admin/section/delete" (adminGate env (handleSectionDelete env))
  post "/admin/section/move" (adminGate env (handleSectionMove env))
  get "/admin/theme" (adminGate env (handleThemeForm env Nothing))
  post "/admin/theme" (adminGate env (handleThemeSave env))
  get "/admin/stripe" (adminGate env (handleStripe env Nothing))
  get "/admin/bookings" (adminGate env (handleBookings env Nothing))
  get "/admin/venues" (adminGate env (handleVenues env Nothing))
  post "/admin/venues/save" (adminGate env (handleVenueSave env))
  post "/admin/venues/delete" (adminGate env (handleVenueDelete env))
  post "/admin/secrets" (adminGate env (handleSaveSecrets env))
  get "/admin/media" (adminGate env (handleMediaPage env Nothing))
  post "/admin/upload" (adminGate env (handleUpload env))
  post "/admin/media/delete" (adminGate env (handleMediaDelete env))
  get "/admin/preview.json" (adminGate env (handlePreviewJson env))
  -- the template engine: partials (header/footer + reusable includes)
  get "/admin/template" (adminGate env (handleTemplate env Nothing))
  post "/admin/partial/new" (adminGate env (handlePartialNew env))
  post "/admin/partial/delete" (adminGate env (handlePartialDelete env))
  get "/admin/render" (adminGate env (handleRender env))

  -- The single-segment catch-all MUST be last.
  get "/:slug" $ do
    slug <- captureParam "slug"
    servePage env ("/" <> slug)

-- ---------------------------------------------------------------------------
-- Public rendering
-- ---------------------------------------------------------------------------

servePage :: AppEnv -> Text -> ActionM ()
servePage env path = do
  site <- currentSiteResolved env
  case pageByPath site path of
    -- resolvedSection applies the template engine: header + body (includes
    -- expanded) + footer.
    Just pg -> htmlOf (renderFullPage (siteTheme site) (aeAnalytics env) (pageTitle pg) (siteFooter site) (resolvedSection site pg) defaultVp)
    Nothing -> do
      status status404
      htmlOf (simplePage (siteTheme site) (aeAnalytics env) "Not found" "That page does not exist.")

serveStage :: AppEnv -> Text -> Double -> Map String Double -> ActionM ()
serveStage env path width overrides = do
  site <- currentSiteResolved env
  case pageByPath site path of
    Just pg -> htmlOf (renderStageWith (siteTheme site) overrides (resolvedSection site pg) (Viewport width 900))
    Nothing -> status status404 >> text "no such page"

defaultVp :: Viewport
defaultVp = Viewport 1180 900

-- ---------------------------------------------------------------------------
-- Checkout
-- ---------------------------------------------------------------------------

handleBuy :: AppEnv -> ActionM ()
handleBuy env = do
  pid <- captureParam "id"
  site <- currentSite env
  -- Resolve the target server-side: a content-defined product (inline
  -- price_data) or a Stripe catalogue price id — but only one that actually
  -- appears in the owner's own catalogue and is currently offered.
  mItem <- case M.lookup pid (siteBuyRegistry site) of
    Just li -> pure (Just (ItemAdHoc li))
    Nothing
      | isPriceId pid -> do
          items <- liftIO (fetchCatalogFor env)
          pure $
            if pid `elem` [ciPriceId i | i <- items, ciAvailability i == OnSale]
              then Just (ItemPrice pid (Quantity 1))
              else Nothing
      | otherwise -> pure Nothing
  case mItem of
    Nothing -> do
      status status404
      htmlOf (simplePage (siteTheme site) (aeAnalytics env) "Unknown item" "That item is not for sale.")
    Just item -> do
      bundle <- liftIO (loadBundle (aeSecretFile env) (aeMasterKey env))
      case sbStripeSk bundle of
        Nothing ->
          htmlOf (simplePage (siteTheme site) (aeAnalytics env) "Checkout unavailable" "The shop owner has not connected Stripe yet.")
        Just sk -> do
          -- The product's display name and seat semantics ride on the
          -- session as metadata, and the success URL carries the session
          -- id — all three feed ticket issuance after payment.
          (productName, seats) <- case item of
            ItemAdHoc li -> pure (liName li, "1")
            ItemPrice priceId _ -> do
              items <- liftIO (fetchCatalogFor env)
              pure $ case [i | i <- items, ciPriceId i == priceId] of
                (i : _) -> (ciName i, ciSeats i)
                [] -> (priceId, "1")
          let req =
                CheckoutRequest
                  (item :| [])
                  (Url (aeBaseUrl env <> "/success?session_id={CHECKOUT_SESSION_ID}"))
                  (Url (aeBaseUrl env <> "/cancel"))
                  [("sww_product", productName), ("sww_seats", seats)]
          res <- liftIO (stripeCreate (aeManager env) sk req)
          case res of
            Right session -> do
              let Url u = csUrl session
              status status303
              setHeader "Location" (TL.fromStrict u)
            Left err -> do
              liftIO (putStrLn ("[checkout error] " <> show err))
              status status500
              htmlOf (simplePage (siteTheme site) (aeAnalytics env) "Checkout error" "Something went wrong creating the checkout. Please try again.")

handleWebhook :: AppEnv -> ActionM ()
handleWebhook env = do
  raw_ <- body
  msig <- header "Stripe-Signature"
  bundle <- liftIO (loadBundle (aeSecretFile env) (aeMasterKey env))
  let rawStrict = TL.toStrict (TLE.decodeUtf8 raw_)
      rawBytes = TE.encodeUtf8 rawStrict
      evType = peekEventType rawBytes
  case sbStripeWebhook bundle of
    Nothing -> do
      liftIO (putStrLn "[stripe webhook] REJECTED: no webhook secret configured")
      status status503
      text "webhook not configured"
    Just whsec -> case msig of
      Nothing -> do
        liftIO (putStrLn "[stripe webhook] REJECTED: missing Stripe-Signature")
        status status400
        text "missing signature"
      Just sig ->
        if verifyStripeSig (secretText whsec) rawBytes (TL.toStrict sig)
          then do
            liftIO (putStrLn ("[stripe webhook] verified event: " <> T.unpack evType))
            -- Fulfilment: a completed, paid checkout issues a ticket (and
            -- its email). Idempotent against the success page doing the same.
            when (evType == "checkout.session.completed") $
              case webhookSessionOf rawBytes of
                Just info | siPaid info, Just email <- siEmail info ->
                  liftIO (void (fulfillPaidSession env (siId info) email (fromMaybe "Class" (siProduct info)) (fromMaybe "1" (siSeats info))))
                _ -> liftIO (putStrLn "[stripe webhook] completed session without payment/email — skipped")
            text "ok"
          else do
            liftIO (putStrLn "[stripe webhook] REJECTED: bad signature")
            status status400
            text "bad signature"

-- | Issue (or find) the ticket for a paid session and send its email.
-- Shared by the webhook and the success page; both may run — 'issueTicket'
-- is idempotent on the session id, the ticket mail on its ledger note.
fulfillPaidSession :: AppEnv -> Text -> Text -> Text -> Text -> IO Ticket
fulfillPaidSession env sessionId email productName seats = do
  et <- issueTicket (aeBookings env) sessionId email productName seats
  case et of
    Left err -> do
      putStrLn ("[booking] ticket issuance failed: " <> T.unpack err)
      ioError (userError (T.unpack err))
    Right (t, fresh) -> do
      when fresh (putStrLn ("[booking] ticket " <> T.unpack (tkCode t) <> " issued for " <> T.unpack productName))
      notifyTicketIssued (aeBookingCtx env) t
      pure t

-- ---------------------------------------------------------------------------
-- Success page: verify the session, hand over the ticket code
-- ---------------------------------------------------------------------------

handleSuccess :: AppEnv -> ActionM ()
handleSuccess env = do
  site <- currentSite env
  let th = siteTheme site
      plainThanks = htmlOf (simplePage th (aeAnalytics env) "Thank you" "Your payment was received. A receipt is on its way.")
  msess <- queryParamMaybe "session_id"
  bundle <- liftIO (loadBundle (aeSecretFile env) (aeMasterKey env))
  case (msess, sbStripeSk bundle) of
    (Just sid, Just sk) -> do
      einfo <- liftIO (stripeGetSession (aeManager env) sk sid)
      case einfo of
        Left err -> do
          liftIO (putStrLn ("[booking] success-page session lookup failed: " <> T.unpack err))
          plainThanks
        Right info
          | not (siPaid info) ->
              htmlOf (simplePage th (aeAnalytics env) "Almost there" "Your payment is still processing — your class ticket will arrive by email as soon as it completes.")
          | Just email <- siEmail info -> do
              mt <- liftIO (fulfillSafe env (siId info) email (fromMaybe "Class" (siProduct info)) (fromMaybe "1" (siSeats info)))
              case mt of
                Nothing -> plainThanks
                Just t -> htmlOf (ticketPage th (aeAnalytics env) t)
          | otherwise -> plainThanks
    _ -> plainThanks

fulfillSafe :: AppEnv -> Text -> Text -> Text -> Text -> IO (Maybe Ticket)
fulfillSafe env sid email productName seats = do
  r <- try (fulfillPaidSession env sid email productName seats) :: IO (Either SomeException Ticket)
  case r of
    Right t -> pure (Just t)
    Left e -> Nothing <$ putStrLn ("[booking] fulfilment failed: " <> show e)

-- | The post-payment page: the ticket code, and where to cash it in.
ticketPage :: Theme -> Maybe Analytics -> Ticket -> Html
ticketPage th an t =
  messagePage th an "Thank you — you have a ticket!" $
    htmlConcat
      [ para ("Your payment for \x201C" <> tkProduct t <> "\x201D is confirmed. This is your class ticket code (also on its way to your inbox):")
      , elAttr "p" [("class", "sww-price"), ("style", "font-size:28px;letter-spacing:.06em")] (esc (tkCode t))
      , para "Now pick the date of your class — choose a slot and enter this code together with the email address you used at checkout."
      , elAttr "p" [] (elAttr "a" [("href", "/classes"), ("class", "sww-buy sww-schedsubmit"), ("style", "display:inline-block;text-decoration:none")] (esc "Schedule your class \x2192"))
      ]
  where
    para txt = elAttr "p" [("class", "sww-paragraph")] (esc txt)

-- ---------------------------------------------------------------------------
-- Schedule: cash a ticket in for a slot
-- ---------------------------------------------------------------------------

handleSchedule :: AppEnv -> ActionM ()
handleSchedule env = do
  site <- currentSite env
  let th = siteTheme site
  code <- fromMaybe "" <$> formParamMaybe "code"
  email <- fromMaybe "" <$> formParamMaybe "email"
  name <- fromMaybe "" <$> formParamMaybe "name"
  slotId <- fromMaybe "" <$> formParamMaybe "slot"
  let slots = siteSlots site
      oops msg = do
        status status400
        htmlOf (simplePage th (aeAnalytics env) "Could not reserve that" msg)
  if T.null (T.strip code) || T.null (T.strip email) || T.null (T.strip name) || T.null slotId
    then oops "Please fill in the ticket code, your email address, your name, and pick a slot."
    else do
      res <- liftIO (redeemTicket (aeBookings env) slots (siteMaxWeekly site) code email name slotId)
      case res of
        Left CodeNotFound -> oops "That ticket code was not recognised — check it against your booking email (the code looks like XXXX-XXXX)."
        Left EmailMismatch -> oops "That email address does not match the one used at checkout. Enter the address your receipt was sent to."
        Left (AlreadyRedeemed r) ->
          oops ("This ticket is already booked in" <> maybe "." (\s -> " for " <> slotLabel s <> ".") (slotById slots (redSlotId r)) <> " If you need to change the date, just reply to your confirmation email.")
        Left SlotUnknown -> oops "That slot is no longer offered — please go back and pick another."
        Left SlotFull -> oops "Sorry — that class filled up moments ago. Please go back and pick another slot."
        Left WeekFull -> oops "That week's classes are now all scheduled — please pick a date in another week, or join one of the classes already on the calendar."
        Right t -> do
          -- Calendar invite + confirmation mail; failures are retried by the
          -- reminder loop, so the student's reservation stands regardless.
          r <- liftIO (try (ensureReservationSideEffects (aeBookingCtx env) t) :: IO (Either SomeException ()))
          case r of
            Left e -> liftIO (putStrLn ("[booking] side effects deferred to the retry loop: " <> show e))
            Right () -> pure ()
          case tkRedemption t >>= (slotById slots . redSlotId) of
            Just s ->
              htmlOf . messagePage th (aeAnalytics env) "You're booked!" $
                htmlConcat
                  [ elAttr "p" [("class", "sww-paragraph")] (esc ("See you on " <> slotLabel s <> " \x2014 " <> slotArea s <> "."))
                  , elAttr "p" [("class", "sww-paragraph")] (esc ("A confirmation email and a Google Calendar invitation are on their way. The exact " <> bcVenueNoun (aeBookingCtx env) <> " address is emailed about a week before class."))
                  ]
            Nothing -> htmlOf (simplePage th (aeAnalytics env) "You're booked!" "A confirmation email and a calendar invitation are on their way.")

slotById :: [SlotSpec] -> Text -> Maybe SlotSpec
slotById slots sid = listToMaybe [s | s <- slots, slotIdOf s == sid]

slotLabel :: SlotSpec -> Text
slotLabel s = fmtSlotDay (slotDate s) <> ", " <> slotStart s <> "\x2013" <> slotEnd s

-- ---------------------------------------------------------------------------
-- Email protection: proof-of-work gated reveal
-- ---------------------------------------------------------------------------

-- | Leading zero bits a solution digest must carry. 16 bits = ~65k hashes on
-- average — around a second of WebCrypto work for a visitor, an expensive
-- proposition for a bulk address harvester (and each success is single-use).
powDifficulty :: Int
powDifficulty = 16

-- | How long an issued challenge stays valid, in seconds.
powTtl :: Integer
powTtl = 900

-- | Issue a challenge: @epoch.rand.hmac@ — self-authenticating (HMAC under
-- the master key), so the server keeps no state until one is redeemed.
handleEmailChallenge :: AppEnv -> ActionM ()
handleEmailChallenge env = do
  now <- liftIO getCurrentTime
  rand <- liftIO genSessionSeed
  let epoch = floor (utcTimeToPOSIXSeconds now) :: Integer
      base = T.pack (show epoch) <> "." <> T.pack (show (abs rand))
      mac = hmacTokenHex (aeMasterKey env) (TE.encodeUtf8 ("pow:" <> base))
      challenge = base <> "." <> mac
  jsonOut
    (JObj [("challenge", JStr (T.unpack challenge)), ("bits", JNum (fromIntegral powDifficulty))])

-- | Redeem a challenge: verify its signature and freshness, that it has not
-- been used before, and that sha256(challenge:nonce) clears the difficulty —
-- then (and only then) hand out the owner's address from the content.
handleEmailReveal :: AppEnv -> ActionM ()
handleEmailReveal env = do
  raw_ <- body
  now <- liftIO getCurrentTime
  let parsed = do
        v <- either (const (Left "bad json")) Right (decode (TL.unpack (TLE.decodeUtf8 raw_)))
        ch <- either (const (Left "missing challenge")) (fmap T.pack . jstr) (v .: "challenge")
        nonce <- either (const (Left "missing nonce")) (fmap T.pack . jstr) (v .: "nonce")
        pure (ch, nonce)
  case parsed of
    Left e -> badRequest (T.pack e)
    Right (challenge, nonce) ->
      case T.splitOn "." challenge of
        [epochS, randS, mac] -> do
          let base = epochS <> "." <> randS
              expect = hmacTokenHex (aeMasterKey env) (TE.encodeUtf8 ("pow:" <> base))
              epochOk = case reads (T.unpack epochS) of
                [(e :: Integer, "")] ->
                  let nowE = floor (utcTimeToPOSIXSeconds now) :: Integer
                   in e <= nowE + 60 && nowE - e <= powTtl
                _ -> False
          if not (ctEq expect mac) || not epochOk
            then badRequest "stale or invalid challenge"
            else do
              fresh <- liftIO $ atomicModifyIORef' (aeSpentPow env) $ \spent ->
                let pruned = M.filter (\t -> now `lessThanTtl` t) spent
                 in if M.member challenge pruned
                      then (pruned, False)
                      else (M.insert challenge now pruned, True)
              if not fresh
                then badRequest "challenge already used"
                else
                  if powZeroBits (challenge <> ":" <> nonce) < powDifficulty
                    then badRequest "proof of work does not satisfy the difficulty"
                    else do
                      site <- currentSite env
                      case [e | Contact _ _ (Just e) <- allSectionsOf site] of
                        (email : _) -> jsonOut (JObj [("email", JStr (T.unpack email))])
                        [] -> status status404 >> jsonOut (JObj [("error", JStr "no address configured")])
        _ -> badRequest "malformed challenge"
  where
    lessThanTtl now issued = floor (utcTimeToPOSIXSeconds now) - (floor (utcTimeToPOSIXSeconds issued) :: Integer) <= powTtl
    badRequest msg = status status400 >> jsonOut (JObj [("error", JStr (T.unpack msg))])

-- | Leading zero bits of @sha256(input)@.
powZeroBits :: Text -> Int
powZeroBits input =
  let digest = hash (TE.encodeUtf8 input) :: Digest SHA256
      bytes = BA.unpack digest :: [Word8]
   in go 0 bytes
  where
    go acc [] = acc
    go acc (w : ws)
      | w == 0 = go (acc + 8) ws
      | otherwise = acc + length (takeWhile (\i -> not (testBit w i)) [7, 6 .. 0])

jsonOut :: JSON -> ActionM ()
jsonOut j = do
  setHeader "Content-Type" "application/json; charset=utf-8"
  raw (TLE.encodeUtf8 (TL.pack (encode j)))

-- ---------------------------------------------------------------------------
-- Media: public serving + the admin library
-- ---------------------------------------------------------------------------

-- | Serve an uploaded file. Upload names embed a content-hash, so the response
-- is immutable-cacheable.
serveMediaFile :: AppEnv -> ActionM ()
serveMediaFile env = do
  name <- captureParam "file"
  mpath <- liftIO (resolveMedia (aeDataDir env) name)
  case mpath of
    Nothing -> status status404 >> text "no such media"
    Just path -> do
      maybe (pure ()) (setHeader "Content-Type" . TL.fromStrict) (contentTypeFor name)
      setHeader "Cache-Control" "public, max-age=31536000, immutable"
      file path

-- | Serve a template asset (dev fallback; in production a reverse proxy
-- usually short-circuits @\/assets\/*@ before it reaches the app).
serveAssetFile :: AppEnv -> ActionM ()
serveAssetFile env = do
  name <- captureParam "file"
  let path = aeSiteDir env </> "assets" </> T.unpack name
  ok <- if isSafeMediaName name then liftIO (doesFileExist path) else pure False
  if not ok
    then status status404 >> text "no such asset"
    else do
      maybe (pure ()) (setHeader "Content-Type" . TL.fromStrict) (contentTypeFor name)
      setHeader "Cache-Control" "public, max-age=604800"
      file path

handleMediaPage :: AppEnv -> Maybe Text -> ActionM ()
handleMediaPage env mfl = do
  entries <- liftIO (listMedia (aeDataDir env))
  builtins <- liftIO (listAssets (aeSiteDir env))
  htmlOf (mediaLibraryPage entries builtins mfl)

-- | Accept one or many multipart files (the uploader sends one per request;
-- a no-JS form may send several). Responds with JSON when asked
-- (@?json=1@), else redirects back to the library.
handleUpload :: AppEnv -> ActionM ()
handleUpload env = do
  fs <- files
  results <-
    liftIO $
      mapM
        ( \(_, fi) ->
            saveMedia
              (aeDataDir env)
              (TE.decodeUtf8With lenientDecode (fileName fi))
              (BL.toStrict (fileContent fi))
        )
        fs
  let saved = [n | Right n <- results]
      errs = [e | Left e <- results] <> ["no file received" | null results]
  wantJson <- queryParamMaybe "json"
  case (wantJson :: Maybe Text) of
    Just _ -> do
      setHeader "Content-Type" "application/json; charset=utf-8"
      raw . TLE.encodeUtf8 . TL.pack . encode $
        JObj
          [ ("saved", JArr (map (JStr . T.unpack) saved))
          , ("errors", JArr (map (JStr . T.unpack) errs))
          ]
    Nothing -> redirect "/admin/media"

handleMediaDelete :: AppEnv -> ActionM ()
handleMediaDelete env = do
  name <- fromMaybe "" <$> formParamMaybe "name"
  _ <- liftIO (deleteMedia (aeDataDir env) name)
  redirect "/admin/media"

-- ---------------------------------------------------------------------------
-- Admin: content editor handlers
-- ---------------------------------------------------------------------------

dashboard :: AppEnv -> Maybe Text -> ActionM ()
dashboard env mfl = do
  site <- currentSite env
  htmlOf (dashboardPage site mfl)

handleSiteMeta :: AppEnv -> ActionM ()
handleSiteMeta env = do
  title <- fromMaybe "" <$> formParamMaybe "title"
  desc <- fromMaybe "" <$> formParamMaybe "description"
  _ <- liftIO (modifySite (aeStore env) (setSiteMeta title desc))
  redirect "/admin"

handlePageNew :: AppEnv -> ActionM ()
handlePageNew env = do
  path <- fromMaybe "" <$> formParamMaybe "path"
  title <- fromMaybe "" <$> formParamMaybe "title"
  if T.null (T.strip path)
    then redirect "/admin"
    else do
      _ <- liftIO (modifySite (aeStore env) (addPage path (if T.null (T.strip title) then path else title)))
      redirect "/admin"

handlePageDelete :: AppEnv -> ActionM ()
handlePageDelete env = do
  path <- fromMaybe "/" <$> formParamMaybe "path"
  _ <- liftIO (modifySite (aeStore env) (deletePage path))
  redirect "/admin"

handlePageTitle :: AppEnv -> ActionM ()
handlePageTitle env = do
  path <- fromMaybe "/" <$> formParamMaybe "path"
  title <- fromMaybe "" <$> formParamMaybe "title"
  _ <- liftIO (modifySite (aeStore env) (setPageTitle path title))
  redirect (TL.fromStrict ("/admin/page?path=" <> path))

handlePageEditor :: AppEnv -> ActionM ()
handlePageEditor env = do
  path <- fromMaybe "/" <$> queryParamMaybe "path"
  site <- currentSite env
  if isPartialPath path
    then -- a partial/header/footer: edit it with the same UI via a synthetic page
      htmlOf (pageEditorPage path (PageSpec path (containerTitle path site) (sectionsOf path site)) Nothing)
    else case pageByPath site path of
      Just pg -> htmlOf (pageEditorPage path pg Nothing)
      Nothing -> redirect "/admin"

handleSectionNew :: AppEnv -> ActionM ()
handleSectionNew env = do
  path <- fromMaybe "/" <$> formParamMaybe "path"
  kind <- fromMaybe "prose" <$> formParamMaybe "kind"
  _ <- liftIO (modifySite (aeStore env) (addSection path kind))
  site <- currentSite env
  let n = length (sectionsOf path site)
  redirect (TL.fromStrict ("/admin/section?path=" <> path <> "&i=" <> T.pack (show (max 0 (n - 1)))))

handleSectionForm :: AppEnv -> ActionM ()
handleSectionForm env = do
  path <- fromMaybe "/" <$> queryParamMaybe "path"
  i <- fromMaybe 0 <$> queryParamMaybe "i"
  site <- currentSite env
  case sectionAt path i site of
    Just sec -> htmlOf (sectionFormPage (partialNames site) path i sec Nothing)
    Nothing -> redirect (TL.fromStrict ("/admin/page?path=" <> path))

handleSectionSave :: AppEnv -> ActionM ()
handleSectionSave env = do
  path <- fromMaybe "/" <$> queryParamMaybe "path"
  i <- fromMaybe 0 <$> queryParamMaybe "i"
  action <- fromMaybe "save" <$> formParamMaybe "action"
  ps <- formParams
  site <- currentSite env
  case sectionAt path i site of
    Nothing -> redirect (TL.fromStrict ("/admin/page?path=" <> path))
    Just old -> do
      let parsed = parseSection old ps
          newSec
            | action == "gen-slots" = appendGeneratedSlots ps parsed
            | otherwise = applyAction action parsed
      _ <- liftIO (modifySite (aeStore env) (setSectionAt path i newSec))
      redirect (TL.fromStrict ("/admin/section?path=" <> path <> "&i=" <> T.pack (show i)))
  where
    applyAction a sec
      | a == "save" = sec
      | "add-" `T.isPrefixOf` a = addListItem (T.drop 4 a) sec
      | "del-" `T.isPrefixOf` a =
          case parseDel (T.drop 4 a) of
            Just (ln, j) -> deleteListItem ln j sec
            Nothing -> sec
      | otherwise = sec
    parseDel s =
      let (a, b) = T.breakOnEnd "-" s
       in case (T.stripSuffix "-" a, readIntT b) of
            (Just ln, Just j) -> Just (ln, j)
            _ -> Nothing

handleSectionDelete :: AppEnv -> ActionM ()
handleSectionDelete env = do
  path <- fromMaybe "/" <$> formParamMaybe "path"
  i <- fromMaybe 0 <$> formParamMaybe "i"
  _ <- liftIO (modifySite (aeStore env) (deleteSection path i))
  redirect (TL.fromStrict ("/admin/page?path=" <> path))

handleSectionMove :: AppEnv -> ActionM ()
handleSectionMove env = do
  path <- fromMaybe "/" <$> formParamMaybe "path"
  i <- fromMaybe 0 <$> formParamMaybe "i"
  d <- fromMaybe 0 <$> formParamMaybe "d"
  _ <- liftIO (modifySite (aeStore env) (moveSection path i d))
  redirect (TL.fromStrict ("/admin/page?path=" <> path))

handleThemeForm :: AppEnv -> Maybe Text -> ActionM ()
handleThemeForm env mfl = do
  site <- currentSite env
  htmlOf (themeEditorPage (siteTheme site) mfl)

handleThemeSave :: AppEnv -> ActionM ()
handleThemeSave env = do
  ps <- formParams
  site <- currentSite env
  let th' = parseTheme (siteTheme site) ps
  _ <- liftIO (modifySite (aeStore env) (setTheme th'))
  htmlOf (themeEditorPage th' (Just "Saved."))

handlePreviewJson :: AppEnv -> ActionM ()
handlePreviewJson env = do
  path <- fromMaybe "/" <$> queryParamMaybe "path"
  site <- currentSiteResolved env
  case pageByPath site path of
    Just pg -> do
      setHeader "Content-Type" "application/json; charset=utf-8"
      raw (TLE.encodeUtf8 (TL.pack (encode (documentToJSON (secLayout (resolvedSection site pg))))))
    Nothing -> status status404 >> text "no such page"

-- | Standalone render of a container's own sections (a partial / header /
-- footer), so the editor's live preview works for things that have no public
-- URL. Includes inside the partial are expanded; the page header/footer are NOT
-- wrapped around it (you are previewing the block itself).
handleRender :: AppEnv -> ActionM ()
handleRender env = do
  path <- fromMaybe "/" <$> queryParamMaybe "path"
  site <- currentSiteResolved env
  let expanded = concatMap (expandSection site []) (sectionsOf path site)
  htmlOf (renderStaticPage (siteTheme site) (containerTitle path site) (renderSections expanded) defaultVp)

handleTemplate :: AppEnv -> Maybe Text -> ActionM ()
handleTemplate env mfl = do
  site <- currentSite env
  htmlOf (templatePage site mfl)

handlePartialNew :: AppEnv -> ActionM ()
handlePartialNew env = do
  name <- fromMaybe "" <$> formParamMaybe "name"
  _ <- liftIO (modifySite (aeStore env) (addPartial (T.strip name)))
  redirect "/admin/template"

handlePartialDelete :: AppEnv -> ActionM ()
handlePartialDelete env = do
  name <- fromMaybe "" <$> formParamMaybe "name"
  _ <- liftIO (modifySite (aeStore env) (deletePartial name))
  redirect "/admin/template"

-- ---------------------------------------------------------------------------
-- Admin: auth + Stripe keys
-- ---------------------------------------------------------------------------

adminGate :: AppEnv -> ActionM () -> ActionM ()
adminGate env act = do
  mcookie <- header "Cookie"
  epoch <- liftIO (readIORef (aeSession env))
  if maybe False (cookieHasToken (sessionToken env epoch) . TL.toStrict) mcookie
    then act
    else redirect "/admin/login"

handleLogin :: AppEnv -> ActionM ()
handleLogin env = do
  mpw <- formParamMaybe "password"
  if maybe False (ctEq (aeAdminPassword env)) mpw
    then do
      epoch <- liftIO (readIORef (aeSession env))
      setHeader "Set-Cookie" (TL.fromStrict (cookieFor env (sessionToken env epoch)))
      redirect "/admin"
    else do
      status status400
      site <- currentSite env
      htmlOf (adminLoginPage (siteTheme site) (Just "Incorrect password."))

handleStripe :: AppEnv -> Maybe Text -> ActionM ()
handleStripe env mfl = do
  site <- currentSite env
  bundle <- liftIO (loadBundle (aeSecretFile env) (aeMasterKey env))
  catalog <- liftIO (fetchCatalogFor env)
  htmlOf (stripePage (aeBaseUrl env) site catalog bundle mfl)

handleBookings :: AppEnv -> Maybe Text -> ActionM ()
handleBookings env mfl = do
  site <- currentSite env
  tickets <- liftIO (listTickets (aeBookings env))
  bundle <- liftIO (loadBundle (aeSecretFile env) (aeMasterKey env))
  htmlOf (bookingsPage tickets (siteSlots site) bundle mfl)

handleVenues :: AppEnv -> Maybe Text -> ActionM ()
handleVenues env mfl = do
  site <- currentSite env
  venues <- liftIO (listVenues (aeVenues env))
  let areas = dedupe [slotArea s | s <- siteSlots site]
  htmlOf (venuesPage (bcVenueNoun (aeBookingCtx env)) venues areas mfl)
  where
    dedupe = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

handleVenueSave :: AppEnv -> ActionM ()
handleVenueSave env = do
  vid <- fromMaybe "" <$> formParamMaybe "id"
  name <- fromMaybe "" <$> formParamMaybe "name"
  addr <- fromMaybe "" <$> formParamMaybe "address"
  area <- fromMaybe "" <$> formParamMaybe "area"
  mapU <- fromMaybe "" <$> formParamMaybe "mapUrl"
  notes <- fromMaybe "" <$> formParamMaybe "notes"
  active <- formParamMaybe "active" :: ActionM (Maybe Text)
  if T.null (T.strip name)
    then handleVenues env (Just "A venue needs at least a name.")
    else do
      liftIO $
        upsertVenue
          (aeVenues env)
          (Venue (T.strip vid) (T.strip name) (T.strip addr) (T.strip area) (T.strip mapU) (T.strip notes) (isOn active))
      handleVenues env (Just "Saved.")
  where
    isOn (Just "on") = True
    isOn (Just "true") = True
    isOn _ = False

handleVenueDelete :: AppEnv -> ActionM ()
handleVenueDelete env = do
  vid <- fromMaybe "" <$> formParamMaybe "id"
  liftIO (deleteVenue (aeVenues env) vid)
  handleVenues env (Just "Deleted.")

handleSaveSecrets :: AppEnv -> ActionM ()
handleSaveSecrets env = do
  bundle <- liftIO (loadBundle (aeSecretFile env) (aeMasterKey env))
  pk <- formParamMaybe "stripePk"
  sk <- formParamMaybe "stripeSk"
  wh <- formParamMaybe "stripeWebhook"
  gid <- formParamMaybe "googleClientId"
  gsec <- formParamMaybe "googleClientSecret"
  gtok <- formParamMaybe "googleRefreshToken"
  -- A non-empty field updates that key; a blank field keeps the existing one.
  let pick mval old = case fmap T.strip mval of
        Just v | not (T.null v) -> Just (SecretValue v)
        _ -> old
      updated =
        bundle
          { sbStripePk = pick pk (sbStripePk bundle)
          , sbStripeSk = pick sk (sbStripeSk bundle)
          , sbStripeWebhook = pick wh (sbStripeWebhook bundle)
          , sbGoogleClientId = pick gid (sbGoogleClientId bundle)
          , sbGoogleClientSecret = pick gsec (sbGoogleClientSecret bundle)
          , sbGoogleRefreshToken = pick gtok (sbGoogleRefreshToken bundle)
          }
  res <- liftIO (saveBundle (aeSecretFile env) (aeMasterKey env) updated)
  let mfl = case res of
        Right () -> Just "Saved."
        Left _ -> Just "Could not save keys (see server log)."
  case res of
    Left e -> liftIO (putStrLn ("[admin] save failed: " <> T.unpack e))
    Right () -> pure ()
  from <- queryParamMaybe "from"
  case (from :: Maybe Text) of
    Just "bookings" -> handleBookings env mfl
    _ -> do
      bundle' <- liftIO (loadBundle (aeSecretFile env) (aeMasterKey env))
      site <- currentSite env
      catalog <- liftIO (fetchCatalogFor env)
      htmlOf (stripePage (aeBaseUrl env) site catalog bundle' mfl)

-- ---------------------------------------------------------------------------
-- Cookies
-- ---------------------------------------------------------------------------

sessionToken :: AppEnv -> Integer -> Text
sessionToken env epoch =
  hmacTokenHex (aeMasterKey env) (TE.encodeUtf8 ("sess:" <> T.pack (show epoch)))

cookieFor :: AppEnv -> Text -> Text
cookieFor env tok =
  "sww_admin=" <> tok <> "; Path=/; HttpOnly; SameSite=Lax" <> secureFlag env

clearCookie :: AppEnv -> Text
clearCookie env =
  "sww_admin=; Path=/; Max-Age=0; HttpOnly; SameSite=Lax" <> secureFlag env

secureFlag :: AppEnv -> Text
secureFlag env = if "https://" `T.isPrefixOf` aeBaseUrl env then "; Secure" else ""

cookieHasToken :: Text -> Text -> Bool
cookieHasToken tok cookieHeader =
  any (ctEq tok) [T.drop (T.length "sww_admin=") (T.strip kv) | kv <- T.splitOn ";" cookieHeader, "sww_admin=" `T.isPrefixOf` T.strip kv]

ctEq :: Text -> Text -> Bool
ctEq a b = BA.constEq (TE.encodeUtf8 a) (TE.encodeUtf8 b)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

htmlOf :: Html -> ActionM ()
htmlOf = html . TL.fromStrict . renderHtml

saneW :: Double -> Double
saneW w
  | w < 320 = 320
  | w > 4096 = 4096
  | otherwise = w

readIntT :: Text -> Maybe Int
readIntT t = case reads (T.unpack (T.strip t)) of [(n, "")] -> Just n; _ -> Nothing

parseRelayout :: String -> (Double, Text, Map String Double)
parseRelayout s = case decode s of
  Left _ -> (1180, "/", M.empty)
  Right j -> (w, p, ov)
    where
      w = either (const 1180) id (jnum =<< j .: "w")
      p = case j .:? "path" of
        Just (JStr t) -> T.pack t
        _ -> "/"
      ov = case j .: "overrides" of
        Right (JObj kvs) -> M.fromList [(k, d) | (k, v) <- kvs, Right d <- [jnum v]]
        _ -> M.empty
