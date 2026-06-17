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

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteArray as BA
import Data.IORef (IORef, modifyIORef', readIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Network.HTTP.Client (Manager)
import Network.HTTP.Types.Status (status303, status400, status404, status500, status503)
import Network.Wai.Middleware.RequestSizeLimit
  ( defaultRequestSizeLimitSettings
  , requestSizeLimitMiddleware
  , setMaxLengthForRequest
  )
import Web.Scotty
  ( ActionM
  , ScottyM
  , body
  , captureParam
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
  ( CheckoutRequest (CheckoutRequest)
  , CheckoutSession (csUrl)
  , Url (Url)
  )
import Swwstructor.Content
  ( PageSpec (PageSpec, pagePath, pageTitle)
  , SiteSpec (siteFooter, siteTheme, sitePages)
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
import Swwstructor.Html (Html, renderHtml)
import Swwstructor.Plugins.Stripe (siteBuyRegistry)
import Swwstructor.Server.Admin
  ( adminLoginPage
  , dashboardPage
  , pageEditorPage
  , parseSection
  , parseTheme
  , sectionFormPage
  , stripePage
  , templatePage
  , themeEditorPage
  )
import Swwstructor.Server.SecretStore
  ( MasterKey
  , SecretBundle (..)
  , SecretValue (SecretValue)
  , hmacTokenHex
  , loadBundle
  , saveBundle
  , secretText
  )
import Swwstructor.Server.Store (SiteStore, modifySite, readSite)
import Swwstructor.Server.Stripe (peekEventType, stripeCreate, verifyStripeSig)
import Swwstructor.Site (renderFullPage, renderStageWith, renderStaticPage, simplePage)
import Swwstructor.Templates (Section (secLayout), expandSection, renderSections, resolvedSection)
import StickyWM
  ( JSON (JObj, JStr)
  , Viewport (Viewport)
  , decode
  , documentToJSON
  , encode
  , jnum
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
  }

currentSite :: AppEnv -> ActionM SiteSpec
currentSite env = liftIO (readSite (aeStore env))

pageByPath :: SiteSpec -> Text -> Maybe PageSpec
pageByPath site p = lookup (norm p) [(norm (pagePath pg), pg) | pg <- sitePages site]
  where
    norm x = if x == "/" then "/" else "/" <> T.dropWhile (== '/') x

-- ---------------------------------------------------------------------------
-- The application
-- ---------------------------------------------------------------------------

swwApp :: AppEnv -> ScottyM ()
swwApp env = do
  -- Bound request bodies before the JSON parser sees them (DoS guard for the
  -- unauthenticated /layout and /stripe/webhook endpoints).
  middleware
    ( requestSizeLimitMiddleware
        (setMaxLengthForRequest (\_ -> pure (Just (1024 * 1024))) defaultRequestSizeLimitSettings)
    )

  get "/health" (text "ok")

  get "/" (servePage env "/")

  get "/layout" $ do
    mw <- queryParamMaybe "w"
    mp <- queryParamMaybe "path"
    serveStage env (fromMaybe "/" mp) (saneW (fromMaybe 1180 mw)) M.empty

  post "/layout" $ do
    raw_ <- body
    let (w, p, overrides) = parseRelayout (TL.unpack (TLE.decodeUtf8 raw_))
    serveStage env p (saneW w) overrides

  post "/buy/:id" (handleBuy env)
  get "/success" $ do
    site <- currentSite env
    htmlOf (simplePage (siteTheme site) "Thank you" "Your payment was received. A receipt is on its way.")
  get "/cancel" $ do
    site <- currentSite env
    htmlOf (simplePage (siteTheme site) "Checkout cancelled" "No charge was made — you can return any time.")

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
  post "/admin/secrets" (adminGate env (handleSaveSecrets env))
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
  site <- currentSite env
  case pageByPath site path of
    -- resolvedSection applies the template engine: header + body (includes
    -- expanded) + footer.
    Just pg -> htmlOf (renderFullPage (siteTheme site) (pageTitle pg) (siteFooter site) (resolvedSection site pg) defaultVp)
    Nothing -> do
      status status404
      htmlOf (simplePage (siteTheme site) "Not found" "That page does not exist.")

serveStage :: AppEnv -> Text -> Double -> Map String Double -> ActionM ()
serveStage env path width overrides = do
  site <- currentSite env
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
  case M.lookup pid (siteBuyRegistry site) of
    Nothing -> do
      status status404
      htmlOf (simplePage (siteTheme site) "Unknown item" "That item is not for sale.")
    Just li -> do
      bundle <- liftIO (loadBundle (aeSecretFile env) (aeMasterKey env))
      case sbStripeSk bundle of
        Nothing ->
          htmlOf (simplePage (siteTheme site) "Checkout unavailable" "The shop owner has not connected Stripe yet.")
        Just sk -> do
          let req =
                CheckoutRequest
                  (li :| [])
                  (Url (aeBaseUrl env <> "/success"))
                  (Url (aeBaseUrl env <> "/cancel"))
          res <- liftIO (stripeCreate (aeManager env) sk req)
          case res of
            Right session -> do
              let Url u = csUrl session
              status status303
              setHeader "Location" (TL.fromStrict u)
            Left err -> do
              liftIO (putStrLn ("[checkout error] " <> show err))
              status status500
              htmlOf (simplePage (siteTheme site) "Checkout error" "Something went wrong creating the checkout. Please try again.")

handleWebhook :: AppEnv -> ActionM ()
handleWebhook env = do
  raw_ <- body
  msig <- header "Stripe-Signature"
  bundle <- liftIO (loadBundle (aeSecretFile env) (aeMasterKey env))
  let rawStrict = TL.toStrict (TLE.decodeUtf8 raw_)
      evType = peekEventType (TE.encodeUtf8 rawStrict)
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
        if verifyStripeSig (secretText whsec) (TE.encodeUtf8 rawStrict) (TL.toStrict sig)
          then do
            liftIO (putStrLn ("[stripe webhook] verified event: " <> T.unpack evType))
            text "ok"
          else do
            liftIO (putStrLn "[stripe webhook] REJECTED: bad signature")
            status status400
            text "bad signature"

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
      let newSec = applyAction action (parseSection old ps)
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
  site <- currentSite env
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
  site <- currentSite env
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
  bundle <- liftIO (loadBundle (aeSecretFile env) (aeMasterKey env))
  htmlOf (stripePage bundle mfl)

handleSaveSecrets :: AppEnv -> ActionM ()
handleSaveSecrets env = do
  bundle <- liftIO (loadBundle (aeSecretFile env) (aeMasterKey env))
  pk <- formParamMaybe "stripePk"
  sk <- formParamMaybe "stripeSk"
  wh <- formParamMaybe "stripeWebhook"
  -- A non-empty field updates that key; a blank field keeps the existing one.
  let pick mval old = case fmap T.strip mval of
        Just v | not (T.null v) -> Just (SecretValue v)
        _ -> old
      updated =
        bundle
          { sbStripePk = pick pk (sbStripePk bundle)
          , sbStripeSk = pick sk (sbStripeSk bundle)
          , sbStripeWebhook = pick wh (sbStripeWebhook bundle)
          }
  res <- liftIO (saveBundle (aeSecretFile env) (aeMasterKey env) updated)
  bundle' <- liftIO (loadBundle (aeSecretFile env) (aeMasterKey env))
  case res of
    Right () -> htmlOf (stripePage bundle' (Just "Saved."))
    Left e -> do
      liftIO (putStrLn ("[admin] save failed: " <> T.unpack e))
      htmlOf (stripePage bundle' (Just "Could not save keys (see server log)."))

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
