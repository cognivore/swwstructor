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
module Main (main) where

import Control.Monad (when)
import Data.IORef (newIORef)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
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
import Swwstructor.Server.SecretStore
  ( genAdminPassword
  , genMasterKey
  , genSessionSeed
  , parseMasterKeyHex
  )
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
          }

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
