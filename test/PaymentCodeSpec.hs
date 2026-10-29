{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (unless)
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (hContentType, hLocation, methodGet, methodPost, statusCode)
import Network.Wai (defaultRequest, requestHeaders, requestMethod)
import Network.Wai.Test (SRequest (..), runSession, setPath, simpleBody, simpleHeaders, simpleStatus, srequest)
import StickyWM (JSON (..), encode)
import Swwstructor.Content (PageSpec (..), SectionSpec (..), SiteSpec (..), SlotSpec (..), slotIdOf)
import Swwstructor.Server.App (AppEnv (..), Stage (Test), swwApp)
import Swwstructor.Server.Booking (listTickets, newBookingStore, takenFor)
import Swwstructor.Server.Catalog (newCatalogCache)
import Swwstructor.Server.Notify (BookingCtx (..))
import Swwstructor.Server.PaymentCodes
import Swwstructor.Server.SecretStore (SecretBundle (..), SecretValue (..), emptyBundle, genMasterKey, saveBundle)
import Swwstructor.Server.Store (newSiteStore, saveSiteFile)
import Swwstructor.Server.Venues (Venue (..), listVenues, newVenueStore, upsertVenue)
import Swwstructor.Theme (nytTheme)
import System.Directory (createDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Web.Scotty (scottyApp)

check :: String -> Bool -> IO ()
check label ok = unless ok (fail label)

main :: IO ()
main = bracket sandbox removeDirectoryRecursive exercise
  where
    sandbox = do
      temp <- getTemporaryDirectory
      (path, handle) <- openTempFile temp "sww-payment-code-test"
      hClose handle
      removeFile path
      createDirectory path
      pure path

exercise :: FilePath -> IO ()
exercise dir = do
  let slot = SlotSpec "2027-01-13" "19:30" "21:30" "Ukiyo, Covent Garden" 8 0 "Mochi class"
      fixture = SiteSpec "Okashi School" "Test fixture" Nothing nytTheme [] Nothing [PageSpec "/classes" "Classes" [ScheduleClass Nothing Nothing 3 [slot]]] []
      secretFile = dir </> "secrets.enc"
      paymentFile = dir </> "payment-codes.json"
      webhookSecret = "fixture-webhook-secret"
  saved <- saveSiteFile (dir </> "site.json") fixture
  either (fail . T.unpack) pure saved
  Right site <- newSiteStore (dir </> "site.json") dir
  master <- genMasterKey
  Right () <- saveBundle secretFile master emptyBundle {sbStripeWebhook = Just (SecretValue webhookSecret)}
  lessons <- newBookingStore (dir </> "bookings.json")
  payments <- newPaymentCodeStore paymentFile
  venues <- newVenueStore (dir </> "venues.json")
  upsertVenue venues (Venue "ukiyo" "UKIYO" "8 Slingsby Place, London WC2E 9AB" "Ukiyo, Covent Garden" "" "" True)
  manager <- newManager defaultManagerSettings
  session <- newIORef 1
  spent <- newIORef M.empty
  catalog <- newCatalogCache
  calendarAttempts <- newIORef (0 :: Int)
  let ctx = BookingCtx manager lessons (modifyIORef' calendarAttempts (+ 1) >> pure Nothing) [] "Okashi School" "https://okashi-school.com" "Europe/London" "+00:00" (pure [slot]) (listVenues venues) "kitchen"
      env = AppEnv site "https://okashi-school.com" Test secretFile master session "fixture-password" manager dir dir catalog spent lessons payments ctx venues Nothing
  app <- scottyApp (swwApp env)
  let requestTo target method path headers body = runSession (srequest (SRequest ((setPath defaultRequest path) {requestMethod = method, requestHeaders = headers}) body)) target
      request = requestTo app
      get path = request methodGet path [] ""
      schedule code = request methodPost "/schedule" [(hContentType, "application/x-www-form-urlencoded")] (BL.fromStrict (TE.encodeUtf8 ("code=" <> code <> "&name=Fixture&slot=" <> slotIdOf slot)))
      paidTo target reference fields state = do
        timestamp <- BC.pack . show . (floor :: Double -> Integer) . realToFrac <$> getPOSIXTime
        let obj = JObj [("id", JStr reference), ("status", JStr state), ("metadata", JObj fields)]
            payload = TE.encodeUtf8 (T.pack (encode (JObj [("type", JStr "payment_intent.succeeded"), ("data", JObj [("object", obj)])])))
            digest = convertToBase Base16 (hmacGetDigest (hmac (TE.encodeUtf8 webhookSecret) (timestamp <> "." <> payload) :: HMAC SHA256)) :: BS.ByteString
        requestTo target methodPost "/stripe/webhook" [(hContentType, "application/json"), ("Stripe-Signature", "t=" <> timestamp <> ",v1=" <> digest)] (BL.fromStrict payload)
      paid = paidTo app
      bodyText = TE.decodeUtf8 . BL.toStrict . simpleBody
  code <- newPaymentCode payments
  check "payment-only codes have their own valid namespace" (isPaymentCode code && not (isPaymentCode "PAY-AB--2345"))
  unknown <- schedule code
  check "an unpaid code is not recorded" (statusCode (simpleStatus unknown) == 400)
  let fields = [("sww_payment_code", JStr (T.unpack code)), ("sww_product", JStr "Feed the payment gremlins")]
  _ <- paid "pi_fixture_not_paid" fields "requires_payment_method"
  before <- lookupPaymentCode payments code
  check "a non-succeeded payment never issues a code" (before == Nothing)
  _ <- paid "pi_fixture_ambiguous" (("sww_code", JStr "ABCD-2345") : fields) "succeeded"
  ambiguous <- lookupPaymentCode payments code
  check "mixed lesson/payment-only metadata is rejected" (ambiguous == Nothing)
  confirmed <- paid "pi_fixture_gremlins" fields "succeeded"
  check "the actual webhook records a paid reference" (statusCode (simpleStatus confirmed) == 200)
  replay <- paid "pi_fixture_gremlins" fields "succeeded"
  check "webhook replay is acknowledged" (statusCode (simpleStatus replay) == 200)
  records <- BS.readFile paymentFile
  check "replay leaves exactly one record" (T.count "pi_fixture_gremlins" (TE.decodeUtf8 records) == 1)
  selected <- schedule (T.toLower (T.filter (/= '-') code))
  check "the booking form recognises the code and redirects to its preview" (statusCode (simpleStatus selected) == 303 && lookup hLocation (simpleHeaders selected) == Just (TE.encodeUtf8 ("/booking/" <> code)))
  page <- get (TE.encodeUtf8 ("/booking/" <> code))
  check "the preview says the payment is recognised but cannot book" (all (`T.isInfixOf` bodyText page) ["We found your code", "Feed the payment gremlins", "eligible for a lesson", "No place has been reserved", "calendar.ics"])
  pending <- get (TE.encodeUtf8 ("/booking/" <> code <> "/calendar.ics"))
  check "the normal calendar endpoint serves the pending ICS" (statusCode (simpleStatus pending) == 200 && "SUMMARY:[PAYMENT PENDING] " `T.isInfixOf` bodyText pending && "8 Slingsby Place" `T.isInfixOf` T.replace "\r\n " "" (bodyText pending))
  unchanged <- listTickets lessons
  attempts <- readIORef calendarAttempts
  check "the gremlins flow creates no lesson, seat claim or staff calendar attempt" (null unchanged && takenFor unchanged slot == 0 && attempts == 0)
  reopened <- newPaymentCodeStore paymentFile
  retained <- lookupPaymentCode reopened code
  check "payment and selected preview survive restart" (maybe False ((== Just (slotIdOf slot)) . pcPreviewSlot) retained)
  let lessonFields = [("sww_code", JStr "ABCD-2345"), ("sww_product", JStr "Group Single"), ("sww_seats", JStr "1")]
  _ <- paid "pi_fixture_class" lessonFields "succeeded"
  booked <- schedule "ABCD-2345"
  check "ordinary paid lesson booking still succeeds" (statusCode (simpleStatus booked) == 303)
  normal <- get "/booking/ABCD-2345/calendar.ics"
  check "ordinary bookings use the same endpoint without the pending title" (statusCode (simpleStatus normal) == 200 && not ("[PAYMENT PENDING]" `T.isInfixOf` bodyText normal))
  tickets <- listTickets lessons
  check "only the real lesson consumes capacity" (length tickets == 1 && takenFor tickets slot == 1)
  _ <- schedule code
  after <- listTickets lessons
  check "repeating a payment preview cannot change lesson reservations" (after == tickets)
  broken <- newPaymentCodeStore (dir </> "missing" </> "codes.json")
  failed <- issuePaymentCode broken "pi_fixture_storage" code "Payment check"
  invisible <- lookupPaymentCode broken code
  check "a failed disk write never publishes a paid code in memory" (either (const True) (const False) failed && invisible == Nothing)
  faultApp <- scottyApp (swwApp env {aePaymentCodes = broken})
  deferred <- paidTo faultApp "pi_fixture_http_storage" fields "succeeded"
  check "the webhook tells Stripe to retry a failed durable write" (statusCode (simpleStatus deferred) == 503 && "payment recording deferred" `T.isInfixOf` bodyText deferred)
  BS.writeFile (dir </> "corrupt.json") "invalid-json"
  corrupt <- try (newPaymentCodeStore (dir </> "corrupt.json")) :: IO (Either IOException PaymentCodeStore)
  check "corrupt paid records cannot silently start empty" (either (const True) (const False) corrupt)
  putStrLn "Payment-code HTTP flow checks passed."
