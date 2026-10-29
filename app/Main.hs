{-# LANGUAGE OverloadedStrings #-}

-- | The swwstructor server entry point.
--
-- Reads its instance configuration from the environment, loads the site from a
-- content directory (JSON or YAML), resolves the master key for the
-- encrypted-at-rest secret store, and serves the site. Each running process is
-- ONE single-tenant website; a NixOS box runs several of these (different
-- @PORT@ / @SWW_SITE_DIR@ / @SWW_DATA_DIR@) behind Caddy.
--
-- Environment:
--
--   * @PORT@              — listen port (default 3000)
--   * @SWW_SITE_DIR@      — content directory holding @site.json@/@site.yaml@
--                           (default @sites/nyt@)
--   * @SWW_DATA_DIR@      — writable dir for @secrets.enc@ (default = site dir)
--   * @BASE_URL@          — public base url (default @http://localhost:PORT@)
--   * @SWW_STAGE@         — @prod@ | @test@ (label only; default test)
--   * @SWW_MASTER_KEY@    — 64 hex chars (32 bytes) for the secret store; if
--                           unset, an ephemeral key is generated (dev only)
--   * @SWW_ADMIN_PASSWORD@— admin login password; if unset, a random one is
--                           generated and printed once (dev only)
--
-- Booking (the @scheduleClass@ section; all optional):
--
--   * @SWW_BOOKING_STAFF@     — comma-separated staff addresses invited to
--                               every class event (no mail goes to students:
--                               the site holds no student contact details)
--   * @SWW_BOOKING_NAME@      — short brand name for event and calendar-file
--                               copy (default: site title)
--   * @SWW_BOOKING_TZ@        — IANA zone classes run in (default Europe\/London)
--   * @SWW_BOOKING_TZ_OFFSET@ — that zone's UTC offset for the class dates
--                               (default +01:00; no tz database in-process)
--   * @SWW_BOOKING_VENUE_NOUN@— what this site calls a venue in student-facing
--                               copy (default \"venue\"; e.g. \"kitchen\")
--   * @SWW_GOOGLE_CLIENT_ID@ \/ @SWW_GOOGLE_CLIENT_SECRET@ \/
--     @SWW_GOOGLE_REFRESH_TOKEN@ — seed the Google connection (calendar
--     invites + Gmail booking mail), same store-wins rule as the Stripe keys
module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.IORef (newIORef)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import Web.Scotty (scotty)

import Swwstructor.Content (siteTitle)
import Swwstructor.Server.App (AppEnv (..), Stage (Prod, Test), swwApp)
import Swwstructor.Server.Booking (newBookingStore)
import Swwstructor.Server.PaymentCodes (newPaymentCodeStore)
import Swwstructor.Server.Google (googleCredsOf)
import Swwstructor.Server.Venues (listVenues, newVenueStore)
import Swwstructor.Server.Notify (BookingCtx (..), siteSlots)
import Swwstructor.Server.Reminders (startReminderLoop)
import Swwstructor.Server.SecretStore
  ( MasterKey
  , SecretBundle
      ( sbGoogleClientId
      , sbGoogleClientSecret
      , sbGoogleRefreshToken
      , sbStripePk
      , sbStripeSk
      , sbStripeWebhook
      )
  , SecretValue (SecretValue)
  , genAdminPassword
  , genMasterKey
  , genSessionSeed
  , loadBundle
  , parseMasterKeyHex
  , saveBundle
  )
import Swwstructor.Server.Catalog (newCatalogCache)
import Swwstructor.Site (Analytics (Analytics))
import Swwstructor.Server.Store (newSiteStore, readSite)

main :: IO ()
main = do
  -- Line-buffer stdout/stderr so startup logs reach the systemd journal
  -- immediately (Haskell block-buffers a piped stdout by default).
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  port <- readEnvInt "PORT" 3000
  siteDir <- fromMaybe "sites/nyt" <$> lookupEnv "SWW_SITE_DIR"
  -- Data dir defaults to a dedicated writable dir, NOT the site dir (which may be
  -- a read-only, world-readable Nix store path). The NixOS unit always sets this
  -- to /var/lib/swwstructor/<name> with umask 077.
  dataDir <- fromMaybe ".swwstructor-data" <$> lookupEnv "SWW_DATA_DIR"
  baseUrl <- readBaseUrl port
  stage <- readStage
  analytics <- readAnalytics baseUrl
  let secretFile = dataDir </> "secrets.enc"
  createDirectoryIfMissing True dataDir

  -- The master key for the encrypted secret store.
  mkHex <- lookupEnv "SWW_MASTER_KEY"
  masterKey <- case mkHex of
    Just h -> case parseMasterKeyHex (T.pack h) of
      Right k -> pure k
      Left e -> do
        putStrLn ("[swwstructor] FATAL: SWW_MASTER_KEY invalid: " <> T.unpack e)
        exitFailure
    Nothing -> do
      putStrLn "[swwstructor] WARNING: SWW_MASTER_KEY unset; using an ephemeral key (admin secrets will NOT survive a restart)."
      genMasterKey

  -- The admin password.
  adminPw <- do
    mp <- lookupEnv "SWW_ADMIN_PASSWORD"
    case mp of
      Just p | not (null p) -> pure (T.pack p)
      _ -> do
        p <- genAdminPassword
        putStrLn ("[swwstructor] dev admin password: " <> T.unpack p <> "  (set SWW_ADMIN_PASSWORD to control this)")
        pure p

  -- If Stripe keys are in the environment (e.g. pulled from rageveil by
  -- `just run`) and the store has none yet, seed the encrypted store with them —
  -- so a fresh clone with correct rageveil credentials works without manual
  -- entry. Never clobbers keys already set via the admin WebUI.
  seedStripeFromEnv secretFile masterKey
  seedGoogleFromEnv secretFile masterKey

  -- Open the runtime content store: a writable site.json in the data dir,
  -- seeded once from the deployed template (SWW_SITE_DIR). Admin edits mutate
  -- and persist it; content is therefore editable at runtime.
  let contentFile = dataDir </> "site.json"
  eStore <- newSiteStore contentFile siteDir
  store <- case eStore of
    Right s -> pure s
    Left e -> do
      putStrLn ("[swwstructor] FATAL: no content (tried " <> contentFile <> " then " <> siteDir <> "): " <> e)
      exitFailure
  site0 <- readSite store

  mgr <- newManager tlsManagerSettings
  sessionRef <- newIORef =<< genSessionSeed
  catalogCache <- newCatalogCache
  spentPow <- newIORef M.empty

  -- Bookings: the ticket store (codes + preferred names only), and the
  -- side-effect context. Staff addresses are invited to every class event.
  bookings <- newBookingStore (dataDir </> "bookings.json")
  paymentCodes <- newPaymentCodeStore (dataDir </> "payment-codes.json")
  venues <- newVenueStore (dataDir </> "venues.json")
  staff <- splitCsv . fromMaybe "" <$> lookupEnv "SWW_BOOKING_STAFF"
  bookingName <- fromMaybe (T.unpack (siteTitle site0)) . nonEmptyEnv <$> lookupEnv "SWW_BOOKING_NAME"
  tzName <- fromMaybe "Europe/London" . nonEmptyEnv <$> lookupEnv "SWW_BOOKING_TZ"
  tzOffset <- fromMaybe "+01:00" . nonEmptyEnv <$> lookupEnv "SWW_BOOKING_TZ_OFFSET"
  venueNoun <- fromMaybe "venue" . nonEmptyEnv <$> lookupEnv "SWW_BOOKING_VENUE_NOUN"
  let bookingCtx =
        BookingCtx
          { bcManager = mgr
          , bcStore = bookings
          , bcCreds = googleCredsOf <$> loadBundle secretFile masterKey
          , bcStaff = staff
          , bcSiteName = T.pack bookingName
          , bcBaseUrl = T.pack baseUrl
          , bcTimeZone = T.pack tzName
          , bcTzOffset = T.pack tzOffset
          , bcSlots = siteSlots <$> readSite store
          , bcVenues = listVenues venues
          , bcVenueNoun = T.pack venueNoun
          }

  let env =
        AppEnv
          { aeStore = store
          , aeBaseUrl = T.pack baseUrl
          , aeStage = stage
          , aeSecretFile = secretFile
          , aeMasterKey = masterKey
          , aeSession = sessionRef
          , aeAdminPassword = adminPw
          , aeManager = mgr
          , aeSiteDir = siteDir
          , aeDataDir = dataDir
          , aeCatalog = catalogCache
          , aeSpentPow = spentPow
          , aeBookings = bookings
          , aePaymentCodes = paymentCodes
          , aeBookingCtx = bookingCtx
          , aeVenues = venues
          , aeAnalytics = analytics
          }

  -- The reminder loop: retries unfinished booking side effects and sends the
  -- week-before / day-of nags. In-process, minute tick, ledger-idempotent.
  startReminderLoop bookingCtx

  when (T.null adminPw) (putStrLn "[swwstructor] note: admin is unconfigured (no password).")
  putStrLn ("[swwstructor] serving \"" <> T.unpack (siteTitle site0) <> "\" (editable) from " <> contentFile <> " on port " <> show port <> " (" <> show stage <> ")")
  scotty port (swwApp env)

-- ---------------------------------------------------------------------------
-- Environment parsing
-- ---------------------------------------------------------------------------

readEnvInt :: String -> Int -> IO Int
readEnvInt name dflt = do
  mv <- lookupEnv name
  pure $ case mv >>= readMaybeInt of
    Just n -> n
    Nothing -> dflt

readMaybeInt :: String -> Maybe Int
readMaybeInt s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing

readBaseUrl :: Int -> IO String
readBaseUrl port = do
  mb <- lookupEnv "BASE_URL"
  pure $ case mb of
    Just b | not (null b) -> stripTrailingSlash b
    _ -> "http://localhost:" <> show port
  where
    stripTrailingSlash = reverse . dropWhile (== '/') . reverse

-- | The analytics tag public pages carry, if the deployment asked for one.
-- @SWW_ANALYTICS_SRC@ is the tracker script (a self-hosted Plausible
-- @plausible.js@, say); @SWW_ANALYTICS_DOMAIN@ is the name it reports as and
-- defaults to the host of @BASE_URL@; @SWW_ANALYTICS_INLINE@ optionally points
-- at a pinned copy of that script, which is then shipped inside the page
-- instead of loaded. Unset = no analytics at all.
readAnalytics :: String -> IO (Maybe Analytics)
readAnalytics baseUrl = do
  msrc <- nonEmptyEnv <$> lookupEnv "SWW_ANALYTICS_SRC"
  mdom <- nonEmptyEnv <$> lookupEnv "SWW_ANALYTICS_DOMAIN"
  mfile <- nonEmptyEnv <$> lookupEnv "SWW_ANALYTICS_INLINE"
  inline <- maybe (pure Nothing) readTracker mfile
  pure $ do
    src <- msrc
    pure (Analytics (T.pack src) (T.pack (fromMaybe (hostOf baseUrl) mdom)) inline)
  where
    hostOf u = takeWhile (/= '/') $ case dropWhile (/= '/') u of
      ('/' : '/' : rest) -> rest
      _ -> u

-- | The tracker source to inline. Unreadable, or carrying a closing script tag
-- (which would end the element early and spill JS into the page), means we fall
-- back to loading it from its URL rather than serving broken markup.
readTracker :: FilePath -> IO (Maybe T.Text)
readTracker path = do
  e <- try (TIO.readFile path) :: IO (Either SomeException T.Text)
  case e of
    Left err -> do
      putStrLn ("[swwstructor] WARNING: cannot read SWW_ANALYTICS_INLINE (" <> path <> "): " <> show err <> " — loading the tracker over the network instead.")
      pure Nothing
    Right js
      | "</script" `T.isInfixOf` T.toLower js -> do
          putStrLn "[swwstructor] WARNING: SWW_ANALYTICS_INLINE contains a closing script tag; loading the tracker over the network instead."
          pure Nothing
      | otherwise -> pure (Just (T.strip js))

readStage :: IO Stage
readStage = do
  ms <- lookupEnv "SWW_STAGE"
  pure $ case fmap (map toLowerAscii) ms of
    Just "prod" -> Prod
    Just "production" -> Prod
    _ -> Test
  where
    toLowerAscii c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

-- ---------------------------------------------------------------------------
-- Stripe-from-environment seeding (the rageveil path)
-- ---------------------------------------------------------------------------

-- | Seed the encrypted store from @SWW_STRIPE_{PK,SK,WEBHOOK}@ env vars, but
-- only when a secret key is provided AND the store has none yet (so the admin
-- WebUI remains the source of truth once used).
seedStripeFromEnv :: FilePath -> MasterKey -> IO ()
seedStripeFromEnv file key = do
  sk <- envSecret "SWW_STRIPE_SK"
  case sk of
    Nothing -> pure ()
    Just _ -> do
      cur <- loadBundle file key
      case sbStripeSk cur of
        Just _ -> pure () -- already configured; never clobber
        Nothing -> do
          pk <- envSecret "SWW_STRIPE_PK"
          wh <- envSecret "SWW_STRIPE_WEBHOOK"
          r <-
            saveBundle
              file
              key
              cur
                { sbStripePk = pk `orKeep` sbStripePk cur
                , sbStripeSk = sk
                , sbStripeWebhook = wh `orKeep` sbStripeWebhook cur
                }
          case r of
            Right () -> putStrLn "[swwstructor] seeded Stripe keys from the environment (rageveil)."
            Left e -> putStrLn ("[swwstructor] WARNING: could not seed Stripe keys: " <> T.unpack e)
  where
    orKeep new old = case new of Just _ -> new; Nothing -> old

-- | Seed the Google OAuth credentials from @SWW_GOOGLE_{CLIENT_ID,CLIENT_SECRET,REFRESH_TOKEN}@,
-- but only when the store has no refresh token yet (admin WebUI wins after).
seedGoogleFromEnv :: FilePath -> MasterKey -> IO ()
seedGoogleFromEnv file key = do
  tok <- envSecret "SWW_GOOGLE_REFRESH_TOKEN"
  case tok of
    Nothing -> pure ()
    Just _ -> do
      cur <- loadBundle file key
      case sbGoogleRefreshToken cur of
        Just _ -> pure () -- already configured; never clobber
        Nothing -> do
          gid <- envSecret "SWW_GOOGLE_CLIENT_ID"
          gsec <- envSecret "SWW_GOOGLE_CLIENT_SECRET"
          r <-
            saveBundle
              file
              key
              cur
                { sbGoogleClientId = gid `orKeep` sbGoogleClientId cur
                , sbGoogleClientSecret = gsec `orKeep` sbGoogleClientSecret cur
                , sbGoogleRefreshToken = tok
                }
          case r of
            Right () -> putStrLn "[swwstructor] seeded Google credentials from the environment."
            Left e -> putStrLn ("[swwstructor] WARNING: could not seed Google credentials: " <> T.unpack e)
  where
    orKeep new old = case new of Just _ -> new; Nothing -> old

envSecret :: String -> IO (Maybe SecretValue)
envSecret name = do
  mv <- lookupEnv name
  pure $ case mv of
    Just s | not (null s) -> Just (SecretValue (T.pack s))
    _ -> Nothing

nonEmptyEnv :: Maybe String -> Maybe String
nonEmptyEnv (Just s) | not (null s) = Just s
nonEmptyEnv _ = Nothing

splitCsv :: String -> [T.Text]
splitCsv = filter (not . T.null) . map T.strip . T.splitOn "," . T.pack
