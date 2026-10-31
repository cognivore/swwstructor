{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Control.Exception (IOException, SomeException, try)
import Control.Monad (when)
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
import Data.List (sortOn, nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Clock (UTCTime (..), getCurrentTime, addUTCTime)
import Data.Time.Calendar (fromGregorian, toGregorian, addDays)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word8)
import Network.HTTP.Client (Manager)
import Network.HTTP.Types.Status (Status, status200, status303, status400, status403, status404, status409, status500, status503)
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
  , finish
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
  , SectionSpec (Contact, ScheduleClass, StripeCatalog, UpcomingClasses)
  , SiteSpec (siteFooter, sitePages, sitePartials, siteTheme)
  , SlotSpec (slotArea, slotDate, slotDescription, slotEnd, slotStart, slotTaken, slotCap)
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
import Swwstructor.Html (Html, elAttr, esc, htmlConcat, renderHtml)
import Swwstructor.Media (contentTypeFor, isSafeMediaName)
import Swwstructor.Money (Quantity (Quantity), Price (..), Cents (..), currencyCode)
import Swwstructor.Plugins.Stripe (siteBuyRegistry)
import Swwstructor.Plugins.StripeCatalog (CatalogItem (ciAvailability, ciName, ciPriceId, ciSeats, ciPrice), catalogSection, isPriceId)
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
import Swwstructor.Block (fmtSlotDay, markdownPlain, ClassOffer (..), ClassTicketOption (..))
import Swwstructor.Server.Booking
  ( BookingStore
  , RedeemError (AlreadyRedeemed, CodeNotFound, SlotFull, SlotUnknown, WeekFull)
  , Redemption (redSlotId, redName)
  , Ticket (tkCode, tkProduct, tkRedemption)
  , issueTicket
  , isTicketCode
  , listTickets
  , newTicketCode
  , redeemTicket
  , readReservationEmails
  , configureReservationEmails
  , queueTestEmail
  , queueMissingReservationEmails
  , retryReservationEmail
  , CheckoutHold, HoldState (..), chCode, chSlot, chPriceId, chState, chCreatedAt, chExpiresAt, chSeats
  , beginCheckoutHold, lookupCheckoutHold, bookingAvailability, holdErrorMessage
  , setPreferredName
  , listPendingCheckoutHolds
  )
import Swwstructor.Server.ReservationEmail (parseRecipients, parseMailbox)
import Swwstructor.Server.Media (deleteMedia, listAssets, listMedia, resolveMedia, saveMedia)
import Swwstructor.Server.Notify
  ( BookingCtx (bcSiteName, bcVenueNoun, bcVenues, bcTimeZone, bcTzOffset)
  , CalendarTicket (..)
  , CalendarStatus (..)
  , bookingIcs
  , ensureReservationSideEffects
  , deliverReservationEmails
  , siteMaxWeekly
  , siteSlots
  )
import Swwstructor.Server.Venues
  ( Venue (Venue, vnMapUrl)
  , VenueStore
  , deleteVenue
  , listVenues
  , upsertVenue
  , venueForArea
  , venueLocation
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
import Swwstructor.Server.PaymentCodes
  ( PaymentCode (..), PaymentCodeStore, newPaymentCode, isPaymentCode
  , lookupPaymentCode, issuePaymentCode, selectPaymentPreview
  )
import Swwstructor.Server.Stripe
  ( PaymentInfo (piReference, piId, piProduct, piSeats, piSucceeded)
  , CheckoutReference (..)
  , peekEventType
  , stripeCreate
  , verifyStripeSig
  , webhookPaymentOf
  )
import Swwstructor.Server.DirectStripe
  ( DirectStripe (..), DirectSession (..), SessionState (..)
  , isDatedPayment, directPaymentOf, directSessionOf
  )
import Swwstructor.Server.DirectBooking
  ( DirectBooking (..), CheckoutProgress (..), resumeCheckout, confirmPayment, applySession )
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
  , aePaymentCodes :: !PaymentCodeStore
  -- ^ paid references and calendar previews, with no lesson entitlement
  , aeBookingCtx :: !BookingCtx
  -- ^ the notification context (calendar invites + booking mail)
  , aeVenues :: !VenueStore
  -- ^ the venue address book (admin-only; feeds venue announcements)
  , aeAnalytics :: !(Maybe Analytics)
  -- ^ the analytics tag public pages carry (deployment config, not content)
  , aeDirectStripe :: !DirectStripe
  , aeDatedCatalog :: !(IO (Either Text [CatalogItem]))
  -- ^ A fresh catalogue read for admission; a stale display cache cannot
  -- authorise a purchase. Replaceable in application tests.
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
  items <- if anySection needsCatalog site0 then liftIO (fetchCatalogFor env) else pure []
  slots <- liftIO (bookingAvailability (aeBookings env) (siteSlots site0))
  venues <- liftIO (bcVenues (aeBookingCtx env))
  now <- liftIO getCurrentTime
  let visible = filter (slotIsUpcoming (aeBookingCtx env) now) slots
      decorate s = s {slotArea = venueLocation venues (slotArea s)}
      fill (StripeCatalog title intro) | not (null items) = catalogSection title intro items
      fill (ScheduleClass title intro weekly ss) =
        ScheduleClass title intro weekly [decorate s | s <- visible, slotIdOf s `elem` map slotIdOf ss]
      fill (UpcomingClasses title intro prices _) =
        UpcomingClasses title intro prices
          [ ClassOffer (decorate s) options
          | s <- sortOn (\slot -> (slotDate slot, slotStart slot)) visible
          , slotTaken s < slotCap s
          , weeklyAllows (siteMaxWeekly site0) slots s
          , let options = sortOn ctoSeats
                  [ ClassTicketOption (ciPriceId item) (ciPrice item) seats
                  | item <- items, ciPriceId item `elem` prices, ciAvailability item == OnSale
                  , Just seats <- [ordinarySeats (ciSeats item)], seats <= slotCap s - slotTaken s
                  ]
          , not (null options)
          ]
      fill other = other
  pure (mapAllSections fill site0)
  where
    needsCatalog StripeCatalog{} = True
    needsCatalog UpcomingClasses{} = True
    needsCatalog _ = False

ordinarySeats :: Text -> Maybe Int
ordinarySeats value = case reads (T.unpack value) of
  [(n, "")] | n == 1 || n == 2 -> Just n
  _ -> Nothing

weeklyAllows :: Int -> [SlotSpec] -> SlotSpec -> Bool
weeklyAllows maximumClasses slots chosen = maximumClasses <= 0 || slotTaken chosen > 0 ||
  length [() | s <- slots, weekOf (slotDate s) == weekOf (slotDate chosen), slotTaken s > 0] < maximumClasses
  where
    weekOf date = case reads (T.unpack date) of
      [(day, "")] -> let (year, week, _) = toWeekDate day in (year, week)
      _ -> (0, 0)

-- Current London civil time includes BST automatically. Other deployments
-- retain their explicitly configured offset. All class times are local.
slotIsUpcoming :: BookingCtx -> UTCTime -> SlotSpec -> Bool
slotIsUpcoming ctx now slot =
  slotDate slot <> " " <> slotStart slot > T.pack (formatTime defaultTimeLocale "%F %H:%M" local)
  where
    local = addUTCTime (fromIntegral (offsetMinutes * 60)) now
    (year, _, _) = toGregorian (utctDay now)
    lastSunday month = let end = addDays (-1) (fromGregorian year (month + 1) 1)
                           (_, _, weekday) = toWeekDate end
                       in addDays (negate (fromIntegral (weekday `mod` 7))) end
    summerStart = UTCTime (lastSunday 3) 3600
    summerEnd = UTCTime (lastSunday 10) 3600
    offsetMinutes :: Int
    offsetMinutes
      | bcTimeZone ctx == "Europe/London" = if now >= summerStart && now < summerEnd then 60 else 0
      | otherwise = case T.unpack (bcTzOffset ctx) of
          sign : h1 : h2 : ':' : m1 : m2 : [] -> case (reads [h1,h2], reads [m1,m2]) of
            ([(hours, "")], [(minutes, "")]) -> (if sign == '-' then negate else id) (hours * 60 + minutes)
            _ -> 0
          _ -> 0

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
  post "/book" (handleBook env)
  get "/checkout/:code" (handleCheckoutPage env)
  post "/checkout/:code/cancel" (handleCheckoutCancel env)
  get "/success" (handleSuccess env)
  get "/cancel" $ do
    site <- currentSite env
    htmlOf (simplePage (siteTheme site) (aeAnalytics env) "Checkout cancelled" "No charge was made — you can return any time.")

  post "/schedule" (handleSchedule env)
  get "/booking/:code" (handleBookingPage env)
  post "/booking/:code/name" (handlePreferredName env)
  get "/booking/:code/calendar.ics" (handleBookingIcs env)

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
  get "/admin/checkouts" (adminGate env (handleCheckouts env))
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
  post "/admin/reservation-emails/save" (reservationEmailGate env (handleReservationEmailsSave env))
  post "/admin/reservation-emails/test" (reservationEmailGate env (handleReservationEmailsTest env))
  post "/admin/reservation-emails/retry" (reservationEmailGate env (handleReservationEmailsRetry env))
  post "/admin/reservation-emails/booking" (reservationEmailGate env (handleReservationEmailsBooking env))
  post "/admin/reservation-emails/account" (reservationEmailGate env (handleReservationEmailAccount env))
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
  ensureBookingCookie env
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

directBooking :: AppEnv -> DirectBooking
directBooking env = DirectBooking (aeBookings env) (aeDirectStripe env) (aeBaseUrl env) (bcSiteName (aeBookingCtx env))

tryStorage :: IO a -> IO (Either IOException a)
tryStorage = try

-- The signed browser attempt makes a double click/retry reuse one checkout.
-- It is not an account: the resulting ordinary ticket remains transferable.
bookingCookieCode :: AppEnv -> ActionM (Maybe Text)
bookingCookieCode env = do
  cookies <- maybe "" TL.toStrict <$> header "Cookie"
  now <- floor . utcTimeToPOSIXSeconds <$> liftIO getCurrentTime
  pure $ do
    cookie <- listToMaybe [T.drop 12 (T.strip c) | c <- T.splitOn ";" cookies, "sww_booking=" `T.isPrefixOf` T.strip c]
    case T.splitOn ":" cookie of
      [code, timestamp, mac] | isTicketCode code -> case reads (T.unpack timestamp) of
        [(issued, "")] | issued <= now, now - issued <= (7200 :: Integer), ctEq mac (bookingMac env code timestamp) -> Just code
        _ -> Nothing
      _ -> Nothing

bookingMac :: AppEnv -> Text -> Text -> Text
bookingMac env code timestamp = hmacTokenHex (aeMasterKey env) (TE.encodeUtf8 ("dated-attempt:" <> code <> ":" <> timestamp))

setBookingCookie :: AppEnv -> Text -> ActionM ()
setBookingCookie env code = do
  timestamp <- T.pack . show . (floor :: Double -> Integer) . realToFrac . utcTimeToPOSIXSeconds <$> liftIO getCurrentTime
  setHeader "Set-Cookie" (TL.fromStrict ("sww_booking=" <> code <> ":" <> timestamp <> ":" <> bookingMac env code timestamp <> "; Path=/; Max-Age=7200; HttpOnly; SameSite=Lax" <> secureFlag env))
  setHeader "Cache-Control" "private, no-store"

ensureBookingCookie :: AppEnv -> ActionM ()
ensureBookingCookie env = do
  code <- bookingCookieCode env
  hold <- maybe (pure Nothing) (liftIO . lookupCheckoutHold (aeBookings env)) code
  case (code, chState <$> hold) of
    (Just _, Nothing) -> pure ()
    (Just _, Just HoldCreating) -> pure ()
    (Just _, Just HoldCheckout{}) -> pure ()
    _ -> liftIO (newTicketCode (aeBookings env)) >>= setBookingCookie env

sameOrigin :: AppEnv -> ActionM Bool
sameOrigin env = do
  origin <- header "Origin"
  pure (maybe True ((== T.dropWhileEnd (== '/') (aeBaseUrl env)) . TL.toStrict) origin)

handleBook :: AppEnv -> ActionM ()
handleBook env = do
  originOk <- sameOrigin env
  code <- bookingCookieCode env
  sid <- fromMaybe "" <$> formParamMaybe "slot"
  pid <- fromMaybe "" <$> formParamMaybe "price"
  quotedAmount <- formParamMaybe (TL.fromStrict ("expectedAmount_" <> pid))
  quotedCurrency <- formParamMaybe (TL.fromStrict ("expectedCurrency_" <> pid))
  fallbackAmount <- formParamMaybe "expectedAmount"
  fallbackCurrency <- formParamMaybe "expectedCurrency"
  let amount = fromMaybe (fromMaybe "" fallbackAmount) quotedAmount :: Text
      currency = fromMaybe (fromMaybe "" fallbackCurrency) quotedCurrency :: Text
  if not originOk then status status403 >> text "Please book from this website."
  else case code of
    Nothing -> checkoutMessage env status400 "Choose your class" "Please return to the class list and press Book again so we can keep your checkout safe to retry." Nothing
    Just attempt -> do
      previous <- liftIO (lookupCheckoutHold (aeBookings env) attempt)
      case previous of
        Just hold
          | slotIdOf (chSlot hold) == sid && chPriceId hold == pid -> runCheckout env hold
          | otherwise -> checkoutMessage env status409 "You already have a checkout open" "Finish or cancel your previous checkout before choosing another class. This prevents two purchases by accident." (Just attempt)
        Nothing -> do
          site <- currentSite env
          now <- liftIO getCurrentTime
          let allowed = nub (concat [prices | UpcomingClasses _ _ prices _ <- allSectionsOf site])
              selected = slotById (siteSlots site) sid
          if T.length sid > 64 || T.length pid > 255 || pid `notElem` allowed || maybe True (not . slotIsUpcoming (aeBookingCtx env) now) selected
            then checkoutMessage env status400 "Choose another class" "That class or ticket option is no longer offered. Please choose an available date." Nothing
            else do
              catalog <- liftIO (aeDatedCatalog env)
              case catalog of
                Left _ -> checkoutMessage env status503 "Checkout is temporarily unavailable" "We couldn't check the current ticket price. No place has been reserved and no payment was started. Please try again shortly." Nothing
                Right items -> case [item | item <- items, ciPriceId item == pid, ciAvailability item == OnSale, ordinarySeats (ciSeats item) /= Nothing] of
                  [] -> checkoutMessage env status409 "Ticket option unavailable" "That ticket option is no longer on sale. Please return to the class list for the current options." Nothing
                  item : _ -> do
                    let Price (Cents priceAmount) priceCurrency = ciPrice item
                    if amount /= T.pack (show priceAmount) || currency /= currencyCode priceCurrency
                      then checkoutMessage env status409 "The ticket price has changed" "Please return to the class list to review the current price before paying. No checkout was started." Nothing
                      else do
                        venues <- liftIO (bcVenues (aeBookingCtx env))
                        let expiry = floor (utcTimeToPOSIXSeconds now) + 1860
                            promisedSlots = [s {slotArea = venueLocation venues (slotArea s)} | s <- siteSlots site]
                        held <- liftIO (try (beginCheckoutHold (aeBookings env) promisedSlots (siteMaxWeekly site) attempt sid pid (ciName item) (ciSeats item) (ciPrice item) expiry))
                        case held of
                          Left (_ :: IOException) -> checkoutMessage env status503 "We couldn't save your checkout" "No payment was started. Please try again shortly." Nothing
                          Right (Left err) -> checkoutMessage env status409 "Choose another class" (holdErrorMessage err) Nothing
                          Right (Right hold) -> runCheckout env hold

runCheckout :: AppEnv -> CheckoutHold -> ActionM ()
runCheckout env hold = do
  result <- liftIO (try (resumeCheckout (directBooking env) hold))
  case result of
    Left (_ :: IOException) -> checkoutMessage env status503 "Your checkout needs a moment" "We couldn't save the checkout result. Your places remain protected while we check it. Use the same checkout to continue." (Just (chCode hold))
    Right (Left _) -> checkoutMessage env status503 "Your checkout needs a moment" "We couldn't confirm the payment service's response. Please return to this checkout shortly; you won't need to start another purchase." (Just (chCode hold))
    Right (Right (ReadyToPay (Url url))) -> status status303 >> setHeader "Location" (TL.fromStrict url)
    Right (Right (BookingPaid ticket)) -> status status303 >> setHeader "Location" (TL.fromStrict ("/booking/" <> tkCode ticket))
    Right (Right CheckoutEnded) -> do
      liftIO (newTicketCode (aeBookings env)) >>= setBookingCookie env
      checkoutMessage env status409 "Checkout ended" "This checkout has expired without payment. Your places have been released; choose a class to start again." Nothing
    Right (Right (CheckoutPending message)) -> checkoutMessage env status200 "Confirming your payment" message (Just (chCode hold))

checkoutMessage :: AppEnv -> Status -> Text -> Text -> Maybe Text -> ActionM ()
checkoutMessage env response title message code = do
  site <- currentSite env
  status response
  setHeader "Cache-Control" "private, no-store"
  htmlOf (messagePage (siteTheme site) (aeAnalytics env) title (para message <>
    maybe mempty (\c -> elAttr "p" [] (elAttr "a" [("href", "/checkout/" <> c), ("class", "sww-buy")] (esc "Return to my checkout"))) code <>
    elAttr "p" [] (elAttr "a" [("href", "/classes"), ("class", "sww-link")] (esc "See upcoming classes"))))

handleCheckoutPage :: AppEnv -> ActionM ()
handleCheckoutPage env = do
  code <- captureParam "code"
  found <- liftIO (lookupCheckoutHold (aeBookings env) code)
  case found of
    Nothing -> checkoutMessage env status404 "Checkout not found" "Please return to the class list to choose a date." Nothing
    Just hold -> do
      result <- liftIO (tryStorage (resumeCheckout (directBooking env) hold))
      case result of
        Right (Right (BookingPaid ticket)) -> status status303 >> setHeader "Location" (TL.fromStrict ("/booking/" <> tkCode ticket))
        Right (Right CheckoutEnded) -> do
          liftIO (newTicketCode (aeBookings env)) >>= setBookingCookie env
          checkoutMessage env status200 "Checkout ended" "This checkout ended without payment. Choose an upcoming class whenever you're ready." Nothing
        Right (Right (ReadyToPay (Url url))) -> do
          site <- currentSite env
          setHeader "Cache-Control" "private, no-store"
          htmlOf (messagePage (siteTheme site) (aeAnalytics env) "Your class is held" $
            para (slotLabel (chSlot hold) <> ". Finish checkout to confirm your place, or cancel to choose another date.") <>
            elAttr "p" [] (elAttr "a" [("href", url), ("class", "sww-buy")] (esc "Continue to payment")) <>
            elAttr "form" [("method", "post"), ("action", "/checkout/" <> code <> "/cancel")] (
              elAttr "input" [("type", "hidden"), ("name", "csrf"), ("value", checkoutActionToken env "cancel" code)] mempty <>
              elAttr "button" [("type", "submit"), ("class", "sww-buy sww-buy-off")] (esc "Cancel checkout and release my places")))
        _ -> checkoutMessage env status503 "Checking your checkout" "Your places remain protected while we confirm the payment service's response. Refresh this page shortly." (Just code)

checkoutActionToken :: AppEnv -> Text -> Text -> Text
checkoutActionToken env purpose code = hmacTokenHex (aeMasterKey env) (TE.encodeUtf8 ("booking-action:" <> purpose <> ":" <> code))

handleCheckoutCancel :: AppEnv -> ActionM ()
handleCheckoutCancel env = do
  code <- captureParam "code"
  token <- fromMaybe "" <$> formParamMaybe "csrf"
  originOk <- sameOrigin env
  if not originOk || not (ctEq token (checkoutActionToken env "cancel" code))
    then status status403 >> text "Please cancel from your checkout page."
    else do
      found <- liftIO (lookupCheckoutHold (aeBookings env) code)
      case found of
        Just hold | HoldCheckout sid _ <- chState hold -> do
          ended <- liftIO (directExpire (aeDirectStripe env) sid)
          checked <- case ended of
            Right session -> pure (Right session)
            Left _ -> liftIO (directRetrieve (aeDirectStripe env) sid)
          result <- case checked of
            Right session -> liftIO (applySession (directBooking env) hold session)
            Left _ -> pure (Left "unresolved")
          case result of
            Right CheckoutEnded -> do
              liftIO (newTicketCode (aeBookings env)) >>= setBookingCookie env
              status status303
              setHeader "Location" "/classes"
            Right (BookingPaid ticket) -> status status303 >> setHeader "Location" (TL.fromStrict ("/booking/" <> tkCode ticket))
            _ -> checkoutMessage env status503 "Cancellation is being checked" "We haven't confirmed cancellation yet. Your places remain held; return to this checkout shortly." (Just code)
        Just hold -> runCheckout env hold
        Nothing -> checkoutMessage env status404 "Checkout not found" "Please return to the class list." Nothing

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
          -- The ticket code is minted now and rides through Stripe with the
          -- product's name and seat semantics as PaymentIntent metadata: the
          -- payment-succeeded webhook turns it into a ticket without the
          -- site ever reading who paid. The PaymentIntent description puts
          -- the code on Stripe's receipt — how the student gets it.
          (productName, seats) <- case item of
            ItemAdHoc li -> pure (liName li, "1")
            ItemPrice priceId _ -> do
              items <- liftIO (fetchCatalogFor env)
              pure $ case [i | i <- items, ciPriceId i == priceId] of
                (i : _) -> (ciName i, ciSeats i)
                [] -> (priceId, "1")
          reference <- case item of
            ItemPrice _ _ -> ClassCode <$> liftIO (newTicketCode (aeBookings env))
            ItemAdHoc _ -> PaymentOnlyCode <$> liftIO (newPaymentCode (aePaymentCodes env))
          -- Separate metadata keys preserve the distinction even if an older
          -- server receives this webhook after a rollback.
          let (code, fields, description) = case reference of
                ClassCode c -> (c, [("sww_code", c), ("sww_seats", seats)], "Select the time for your class: " <> aeBaseUrl env <> bookingLink c <> " — " <> bcSiteName (aeBookingCtx env) <> " ticket " <> c)
                PaymentOnlyCode c -> (c, [("sww_payment_code", c)], bcSiteName (aeBookingCtx env) <> " payment-only code " <> c <> ": not eligible for a lesson")
              req =
                CheckoutRequest
                  (item :| [])
                  (Url (aeBaseUrl env <> "/success?code=" <> code))
                  (Url (aeBaseUrl env <> "/cancel"))
                  ([("sww_product", productName)] <> fields)
                  (Just description)
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
  let rawBytes = BL.toStrict raw_
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
            -- Fulfilment: a succeeded PaymentIntent carrying our code issues
            -- the ticket. Only this event type is ever parsed (and only its
            -- id, status and our metadata); everything else is acknowledged
            -- unread, so no customer data is processed here.
            when (evType == "payment_intent.succeeded") $
              if isDatedPayment rawBytes then case directPaymentOf rawBytes of
                Nothing -> status status400 >> text "dated payment evidence is incomplete" >> finish
                Just payment -> do
                  recorded <- liftIO (tryStorage (confirmPayment (directBooking env) payment))
                  case recorded of
                    Right (Right _) -> pure ()
                    _ -> status status503 >> text "booking recording deferred; please retry" >> finish
              else case webhookPaymentOf rawBytes of
                Just info | piSucceeded info -> case piReference info of
                  Just (ClassCode code) -> do
                    et <- liftIO (issueTicket (aeBookings env) (piId info) code (fromMaybe "Class" (piProduct info)) (fromMaybe "1" (piSeats info)))
                    case et of
                      Right _ -> pure ()
                      Left _ -> status status503 >> text "ticket recording deferred; please retry" >> finish
                  Just (PaymentOnlyCode code) -> do
                    result <- liftIO (issuePaymentCode (aePaymentCodes env) (piId info) code (fromMaybe "Payment" (piProduct info)))
                    case result of
                      Right _ -> pure ()
                      Left err -> do
                        liftIO (putStrLn ("[payment code] issuance failed: " <> T.unpack err))
                        status status503
                        text "payment recording deferred; please retry"
                        finish
                  Nothing -> liftIO (putStrLn "[stripe webhook] no unambiguous recognised reference — skipped")
                _ -> liftIO (putStrLn "[stripe webhook] payment without a ticket code — not ours, skipped")
            when (evType == "checkout.session.expired" && isDatedPayment rawBytes) $
              case directSessionOf rawBytes of
                Just session | dsState session == SessionExpired -> do
                  hold <- liftIO (lookupCheckoutHold (aeBookings env) (dsCode session))
                  case hold of
                    Just saved -> do
                      result <- liftIO (tryStorage (applySession (directBooking env) saved session))
                      case result of
                        Right (Right _) -> pure ()
                        _ -> status status503 >> text "checkout reconciliation deferred; please retry" >> finish
                    Nothing -> status status503 >> text "checkout reference not yet available; please retry" >> finish
                _ -> status status400 >> text "checkout expiry evidence is incomplete" >> finish
            text "ok"
          else do
            liftIO (putStrLn "[stripe webhook] REJECTED: bad signature")
            status status400
            text "bad signature"

-- ---------------------------------------------------------------------------
-- Success page: hand over the ticket code
-- ---------------------------------------------------------------------------

-- | Stripe returns the buyer here with the code minted for their checkout.
-- Once the payment webhook has issued the ticket the page confirms it;
-- until then (usually seconds) it shows the code as pending.
handleSuccess :: AppEnv -> ActionM ()
handleSuccess env = do
  site <- currentSite env
  let th = siteTheme site
  mcode <- queryParamMaybe "code"
  tickets <- liftIO (listTickets (aeBookings env))
  payment <- maybe (pure Nothing) (liftIO . lookupPaymentCode (aePaymentCodes env)) mcode
  case mcode of
    Just code
      | Just p <- payment -> htmlOf (paymentCodePage th (aeAnalytics env) p)
      | (t : _) <- [t | t <- tickets, tkCode t == code] -> htmlOf (ticketPage th (aeAnalytics env) t)
      | isPaymentCode code -> htmlOf (messagePage th (aeAnalytics env) "Your payment check is being confirmed" (para "Refresh this page in a moment. This payment-only code will be recognised once Stripe confirms payment; it cannot reserve a lesson." <> codeLine code))
      | isTicketCode code -> htmlOf (pendingTicketPage th (aeAnalytics env) code)
    _ -> htmlOf (simplePage th (aeAnalytics env) "Thank you" "Your payment was received. Stripe's receipt is on its way.")

paymentCodePage :: Theme -> Maybe Analytics -> PaymentCode -> Html
paymentCodePage th an p = messagePage th an "Payment recorded — payment-only code" $
  para ("Your payment for \x201C" <> pcProduct p <> "\x201D is recorded. This code is for that payment only and is not eligible for a lesson:")
    <> codeLine (pcCode p)
    <> para "Try it on the Classes page to exercise the calendar download. The preview is marked [PAYMENT PENDING] and reserves no places."
    <> elAttr "p" [] (elAttr "a" [("href", "/classes"), ("class", "sww-buy")] (esc "Try the calendar preview"))

-- | The post-payment page: the ticket code, and where to cash it in.
ticketPage :: Theme -> Maybe Analytics -> Ticket -> Html
ticketPage th an t =
  messagePage th an "Thank you — you have a ticket!" $
    htmlConcat
      [ para ("Your payment for \x201C" <> tkProduct t <> "\x201D is confirmed. This is your class ticket code \x2014 it is printed on your Stripe receipt too:")
      , codeLine (tkCode t)
      , para "Choose your class and tell us the name you'd like to be called. Your ticket code is filled in for you."
      , scheduleButton (tkCode t)
      ]

pendingTicketPage :: Theme -> Maybe Analytics -> Text -> Html
pendingTicketPage th an code =
  messagePage th an "Thank you — your ticket is on its way" $
    htmlConcat
      [ para "Your payment is being confirmed (this usually takes a few seconds). Your class ticket code is:"
      , codeLine code
      , para "It is printed on your Stripe receipt as well. It works as soon as the payment confirms \x2014 refresh this page in a moment to see it confirmed, then pick the date of your class."
      , scheduleButton code
      ]

para :: Text -> Html
para txt = elAttr "p" [("class", "sww-paragraph")] (esc txt)

codeLine :: Text -> Html
codeLine c = elAttr "p" [("class", "sww-price"), ("style", "font-size:28px;letter-spacing:.06em")] (esc c)

bookingLink :: Text -> Text
bookingLink code = "/classes#booking=" <> code

scheduleButton :: Text -> Html
scheduleButton code =
  elAttr "p" [] (elAttr "a" [("href", bookingLink code), ("class", "sww-buy sww-schedsubmit"), ("style", "display:inline-block;text-decoration:none")] (esc "Select the time for your class"))

-- ---------------------------------------------------------------------------
-- Schedule: cash a ticket in for a slot
-- ---------------------------------------------------------------------------

handleSchedule :: AppEnv -> ActionM ()
handleSchedule env = do
  site <- currentSite env
  code <- fromMaybe "" <$> formParamMaybe "code"
  name <- fromMaybe "" <$> formParamMaybe "name"
  slotId <- fromMaybe "" <$> formParamMaybe "slot"
  payment <- liftIO (lookupPaymentCode (aePaymentCodes env) code)
  case payment of
    Just p -> case slotById (siteSlots site) slotId of
      Nothing -> do
        status status400
        htmlOf (simplePage (siteTheme site) (aeAnalytics env) "Payment code recognised" (paymentOnlyMessage p <> " Pick a class date to try the calendar preview."))
      Just _ -> do
        saved <- liftIO (selectPaymentPreview (aePaymentCodes env) (pcCode p) slotId)
        case saved of
          Left err -> do
            liftIO (putStrLn ("[payment preview] " <> T.unpack err))
            status status503
            htmlOf (simplePage (siteTheme site) (aeAnalytics env) "Preview unavailable" "Your payment is recorded, but the calendar preview could not be saved. No place was reserved. Please try again.")
          Right _ -> do
            status status303
            setHeader "Location" (TL.fromStrict ("/booking/" <> pcCode p))
    Nothing -> reserveLesson env site code name slotId

paymentOnlyMessage :: PaymentCode -> Text
paymentOnlyMessage p = "We found your code, but it is for \x201C" <> pcProduct p <> "\x201D only and isn't eligible for a lesson. No place has been reserved."

reserveLesson :: AppEnv -> SiteSpec -> Text -> Text -> Text -> ActionM ()
reserveLesson env site code name slotId = do
  now <- liftIO getCurrentTime
  let th = siteTheme site
      slots = siteSlots site
      oops msg = do
        status status400
        htmlOf (simplePage th (aeAnalytics env) "Could not reserve that" msg)
  if maybe True (not . slotIsUpcoming (aeBookingCtx env) now) (slotById slots slotId)
    then oops "That class is no longer available to book. Please choose an upcoming date; your ticket has not been redeemed."
  else if T.null (T.strip code) || T.null (T.strip name) || T.null slotId
    then oops "Please pick a class and fill in your ticket code and the name you'd like to be called."
    else do
      res <- liftIO (redeemTicket (aeBookings env) slots (siteMaxWeekly site) code name slotId)
      case res of
        Left CodeNotFound -> oops "That ticket code was not recognised \x2014 check it against your Stripe receipt (the code looks like XXXX-XXXX). If you have only just paid, give it a minute and try again."
        Left (AlreadyRedeemed r) ->
          oops ("This ticket is already booked in" <> maybe "." (\s -> " for " <> slotLabel s <> ".") (slotById slots (redSlotId r)) <> " If you need to change the date, get in touch.")
        Left SlotUnknown -> oops "That slot is no longer offered — please go back and pick another."
        Left SlotFull -> oops "Sorry — that class filled up moments ago. Please go back and pick another slot."
        Left WeekFull -> oops "That week's classes are now all scheduled — please pick a date in another week, or join one of the classes already on the calendar."
        Right t -> do
          -- The staff calendar event; failures are retried by the loop, so
          -- the student's reservation stands regardless.
          r <- liftIO (try (ensureReservationSideEffects (aeBookingCtx env) t) :: IO (Either SomeException ()))
          case r of
            Left e -> liftIO (putStrLn ("[booking] side effects deferred to the retry loop: " <> show e))
            Right () -> pure ()
          status status303
          setHeader "Location" (TL.fromStrict ("/booking/" <> tkCode t))

-- | A reservation's own page, reached by its code (the code is the only key
-- the site has): the class, its venue address, and the calendar file to save.
handleBookingPage :: AppEnv -> ActionM ()
handleBookingPage env = do
  setHeader "Cache-Control" "private, no-store"
  code <- captureParam "code"
  pending <- liftIO (lookupCheckoutHold (aeBookings env) code)
  case pending of
    Just hold | chState hold /= HoldReleased -> do
      result <- liftIO (tryStorage (resumeCheckout (directBooking env) hold))
      case result of
        Right (Right BookingPaid{}) -> pure ()
        Right (Right CheckoutEnded) -> checkoutMessage env status200 "Checkout ended" "Your checkout ended without payment. Please choose a class to start again." Nothing >> finish
        _ -> do
          setHeader "Refresh" "5"
          checkoutMessage env status200 "Confirming your booking" "Your places are held while we confirm payment. This page updates automatically; you do not need to choose a date again." (Just code)
          finish
    Just _ -> checkoutMessage env status200 "Checkout ended" "Your checkout ended without payment. Please choose a class to start again." Nothing >> finish
    Nothing -> pure ()
  site <- currentSite env
  let th = siteTheme site
  found <- lookupBooking env
  case found of
    Nothing -> do
      status status404
      htmlOf (simplePage th (aeAnalytics env) "No such booking" "That booking code was not recognised \x2014 check it against your Stripe receipt.")
    Just (LessonBooking t Nothing) -> htmlOf (ticketPage th (aeAnalytics env) t)
    Just (PaymentPreview p Nothing) -> htmlOf (paymentCodePage th (aeAnalytics env) p)
    Just (LessonBooking t (Just s)) -> renderDetails (tkCode t) (tkProduct t) s "You're booked!" (preferredNameForm env t)
    Just (PaymentPreview p (Just s)) -> renderDetails (pcCode p) (pcProduct p) s "[PAYMENT PENDING] Calendar preview" (para (paymentOnlyMessage p))
  where
    renderDetails code productName s title notice = do
      site <- currentSite env
      venues <- liftIO (bcVenues (aeBookingCtx env))
      let whereHtml = para ("Where: " <> venueLocation venues (slotArea s))
            <> case venueForArea venues (slotArea s) of
              Just v | not (T.null (vnMapUrl v)) -> elAttr "p" [("class", "sww-paragraph")] (elAttr "a" [("class", "sww-link"), ("href", vnMapUrl v)] (esc "Open the map"))
              _ -> mempty
      htmlOf . messagePage (siteTheme site) (aeAnalytics env) title $
        htmlConcat
          [ notice
          , para ("When: " <> slotLabel s)
          , if T.null (T.strip (slotDescription s)) then mempty else para ("What: " <> productName <> " \x2014 " <> markdownPlain (slotDescription s))
          , whereHtml
          , para ("Code: " <> code <> ". Keep it: this page lives at " <> aeBaseUrl env <> "/booking/" <> code <> ".")
          , elAttr "p" [] (elAttr "a" [("href", "/booking/" <> code <> "/calendar.ics"), ("class", "sww-buy sww-schedsubmit"), ("style", "display:inline-block;text-decoration:none")] (esc "Add to my calendar"))
          ]

preferredNameForm :: AppEnv -> Ticket -> Html
preferredNameForm env ticket =
  elAttr "form" [("method", "post"), ("action", "/booking/" <> tkCode ticket <> "/name"), ("style", "margin:1.5rem 0")] $
    para "Your booking is confirmed. If you'd like, tell us the name to use in class. You can share this booking with someone else if it's a gift." <>
    elAttr "input" [("type", "hidden"), ("name", "csrf"), ("value", checkoutActionToken env "name" (tkCode ticket))] mempty <>
    elAttr "label" [] (esc "Name for class (optional) " <>
      elAttr "input" [("type", "text"), ("name", "name"), ("maxlength", "100"), ("autocomplete", "given-name"), ("value", maybe "" redName (tkRedemption ticket))] mempty) <>
    elAttr "button" [("type", "submit"), ("class", "sww-buy"), ("style", "margin-left:0.5rem")] (esc "Save name")

handlePreferredName :: AppEnv -> ActionM ()
handlePreferredName env = do
  code <- captureParam "code"
  name <- fromMaybe "" <$> formParamMaybe "name"
  token <- fromMaybe "" <$> formParamMaybe "csrf"
  originOk <- sameOrigin env
  if not originOk || not (ctEq token (checkoutActionToken env "name" code))
    then status status403 >> text "Please save the name from your booking page."
    else do
      result <- liftIO (try (setPreferredName (aeBookings env) code name))
      case result of
        Right (Right _) -> status status303 >> setHeader "Location" (TL.fromStrict ("/booking/" <> code))
        Right (Left err) -> checkoutMessage env status400 "Name not saved" (holdErrorMessage err) Nothing
        Left (_ :: IOException) -> checkoutMessage env status503 "Name not saved" "Your booking is still confirmed, but the name couldn't be saved. Please try again shortly." Nothing

handleBookingIcs :: AppEnv -> ActionM ()
handleBookingIcs env = do
  found <- lookupBooking env
  let entry = case found of
        Just (LessonBooking t (Just s)) -> Just (CalendarTicket (tkCode t) (tkProduct t) ConfirmedClass, s)
        Just (PaymentPreview p (Just s)) -> Just (CalendarTicket (pcCode p) (pcProduct p) PaymentPending, s)
        _ -> Nothing
  case entry of
    Just (t, s) -> do
      let ctx = aeBookingCtx env
      now <- liftIO getCurrentTime
      venues <- liftIO (bcVenues ctx)
      setHeader "Content-Type" "text/calendar; charset=utf-8"
      setHeader "Content-Disposition" ("attachment; filename=\"" <> TL.fromStrict (calendarCode t) <> ".ics\"")
      raw (BL.fromStrict (TE.encodeUtf8 (bookingIcs ctx venues now t s)))
    _ -> status status404 >> text "no such booking"

data BookingView = LessonBooking !Ticket !(Maybe SlotSpec) | PaymentPreview !PaymentCode !(Maybe SlotSpec)

-- | Both code kinds reach the same page and calendar handler, while only
-- LessonBooking can carry a reservation. Preview selection is separate state.
lookupBooking :: AppEnv -> ActionM (Maybe BookingView)
lookupBooking env = do
  code <- T.toUpper . T.strip <$> captureParam "code"
  site <- currentSite env
  tickets <- liftIO (listTickets (aeBookings env))
  payment <- liftIO (lookupPaymentCode (aePaymentCodes env) code)
  hold <- liftIO (lookupCheckoutHold (aeBookings env) code)
  pure $ case payment of
    Just p -> Just (PaymentPreview p (pcPreviewSlot p >>= slotById (siteSlots site)))
    Nothing -> case [t | t <- tickets, tkCode t == code] of
      (t : _) -> Just (LessonBooking t (case hold of
        Just saved | HoldConfirmed _ <- chState saved -> Just (chSlot saved)
        _ -> tkRedemption t >>= (slotById (siteSlots site) . redSlotId)))
      [] -> Nothing

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
  emails <- liftIO (readReservationEmails (aeBookings env))
  epoch <- liftIO (readIORef (aeSession env))
  htmlOf (bookingsPage tickets (siteSlots site) bundle emails (reservationEmailCsrf env epoch) mfl)

handleCheckouts :: AppEnv -> ActionM ()
handleCheckouts env = do
  site <- currentSite env
  holds <- liftIO (listPendingCheckoutHolds (aeBookings env))
  now <- floor . utcTimeToPOSIXSeconds <$> liftIO getCurrentTime
  setHeader "Cache-Control" "private, no-store"
  htmlOf (messagePage (siteTheme site) Nothing "Checkouts holding places" $
    para "These purchases have not been confirmed yet. Their places are held until Stripe confirms payment or expiry. The service checks them automatically every minute." <>
    (if null holds then para "No pending checkouts." else htmlConcat (map (entry now) holds)) <>
    elAttr "p" [] (elAttr "a" [("href", "/admin/bookings"), ("class", "sww-link")] (esc "Back to bookings")))
  where
    entry now hold = elAttr "div" [("style", "border-top:1px solid;padding:1rem 0")] $
      para (chCode hold <> " · " <> slotLabel (chSlot hold) <> " · " <> chSeats hold <> " place(s)") <>
      para ("Started: " <> chCreatedAt hold <> ". Stripe price: " <> chPriceId hold <> ".") <>
      para (case chState hold of
        HoldCreating | now > chExpiresAt hold + 22 * 3600 -> "Operator action required: checkout creation is unresolved beyond its safe retry window. Check Stripe using this ticket reference before releasing capacity; do not create a replacement purchase."
        HoldCreating -> "Checkout creation is being reconciled. Its request identity is retained so retries cannot duplicate the purchase."
        HoldCheckout sid _ -> "Awaiting payment or verified expiry. Stripe session: " <> sid
        _ -> "Reconciliation completed; refresh this page.")

reservationEmailCsrf :: AppEnv -> Integer -> Text
reservationEmailCsrf env epoch = T.pack (show (hash (TE.encodeUtf8 ("reservation-emails:" <> sessionToken env epoch)) :: Digest SHA256))

reservationEmailGate :: AppEnv -> ActionM () -> ActionM ()
reservationEmailGate env action = adminGate env $ do
  supplied <- formParamMaybe "csrf"
  epoch <- liftIO (readIORef (aeSession env))
  if maybe False (ctEq (reservationEmailCsrf env epoch)) supplied
    then action
    else status status403 >> text "This form expired. Reload Bookings and try again. No email settings changed and no email was sent."

handleReservationEmailsSave :: AppEnv -> ActionM ()
handleReservationEmailsSave env = do
  enabled <- (== Just ("yes" :: Text)) <$> formParamMaybe "enabled"
  rawRecipients <- fromMaybe "" <$> formParamMaybe "recipients"
  case parseRecipients rawRecipients of
    Left why -> status status400 >> handleBookings env (Just why)
    Right [] | enabled -> status status400 >> handleBookings env (Just "Add at least one recipient before enabling reservation notifications.")
    Right recipients -> do
      liftIO (configureReservationEmails (aeBookings env) enabled recipients)
      reservationEmailsRedirect

handleReservationEmailsTest :: AppEnv -> ActionM ()
handleReservationEmailsTest env = do
  recipient <- fromMaybe "" <$> formParamMaybe "testRecipient"
  site <- currentSite env
  case (parseMailbox recipient, siteSlots site) of
    (Left why, _) -> status status400 >> handleBookings env (Just why)
    (_, []) -> status status400 >> handleBookings env (Just "Add a class date before sending a sample reservation notification.")
    (Right address, slot : _) -> do
      _ <- liftIO (queueTestEmail (aeBookings env) slot address)
      liftIO (deliverReservationEmails (aeBookingCtx env))
      reservationEmailsRedirect

handleReservationEmailsRetry :: AppEnv -> ActionM ()
handleReservationEmailsRetry env = do
  ident <- fromMaybe "" <$> formParamMaybe "delivery"
  retried <- liftIO (retryReservationEmail (aeBookings env) ident)
  if retried
    then liftIO (deliverReservationEmails (aeBookingCtx env)) >> reservationEmailsRedirect
    else status status400 >> handleBookings env (Just "That email is not eligible for retry. Reload to see its current status.")

handleReservationEmailsBooking :: AppEnv -> ActionM ()
handleReservationEmailsBooking env = do
  code <- fromMaybe "" <$> formParamMaybe "ticket"
  site <- currentSite env
  result <- liftIO (queueMissingReservationEmails (aeBookings env) (siteSlots site) code)
  case result of
    Left why -> status status400 >> handleBookings env (Just why)
    Right _ -> liftIO (deliverReservationEmails (aeBookingCtx env)) >> reservationEmailsRedirect

handleReservationEmailAccount :: AppEnv -> ActionM ()
handleReservationEmailAccount env = do
  token <- T.strip . fromMaybe "" <$> formParamMaybe "googleMailRefreshToken"
  rawFrom <- T.strip . fromMaybe "" <$> formParamMaybe "googleMailFrom"
  case if T.null rawFrom then Right Nothing else Just <$> parseMailbox rawFrom of
    Left why -> status status400 >> handleBookings env (Just why)
    Right _ | T.null token -> status status400 >> handleBookings env (Just "Enter the authorised sending account's refresh token. The current connection was kept.")
    Right sender -> do
      bundle <- liftIO (loadBundle (aeSecretFile env) (aeMasterKey env))
      result <- liftIO (saveBundle (aeSecretFile env) (aeMasterKey env) bundle {sbGoogleMailRefreshToken = Just (SecretValue token), sbGoogleMailFrom = sender})
      case result of
        Left _ -> status status500 >> handleBookings env (Just "Could not save the sending account. No notification was sent.")
        Right () -> reservationEmailsRedirect

reservationEmailsRedirect :: ActionM ()
reservationEmailsRedirect = status status303 >> setHeader "Location" "/admin/bookings#reservation-emails"

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
