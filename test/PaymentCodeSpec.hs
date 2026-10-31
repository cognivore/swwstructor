{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (IOException, bracket, try)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (unless)
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Data.ByteArray.Encoding (Base (Base16, Base64), convertToBase, convertFromBase)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
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
import Swwstructor.Server.Booking
import Swwstructor.Server.Catalog (newCatalogCache)
import Swwstructor.Server.Notify (BookingCtx (..), drainReservationEmails)
import Swwstructor.Server.Admin (bookingsPage)
import Swwstructor.Server.Google (GoogleCreds (gcRefreshToken, gcMailFrom), googleCredsOf, googleMailCredsOf, reservationMime, verifiedSendAs)
import Swwstructor.Html (renderHtml)
import Swwstructor.Server.ReservationEmail
import Swwstructor.Server.PaymentCodes
import Swwstructor.Server.SecretStore (SecretBundle (..), SecretValue (..), emptyBundle, genMasterKey, saveBundle, loadBundle, secretText)
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
  let ctx = BookingCtx manager lessons (modifyIORef' calendarAttempts (+ 1) >> pure Nothing) (pure Nothing) [] "Okashi School" "https://okashi-school.com" "Europe/London" "+00:00" (pure [slot]) (listVenues venues) "kitchen"
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
  -- The reservation callback is configured by the authenticated UI and is
  -- committed in the same store write as redemption. No network in this test.
  initialEmails <- readReservationEmails lessons
  check "hook starts disabled and does not backfill old bookings" (initialEmails == emptyReservationEmails)
  denied <- request methodPost "/admin/reservation-emails/save" [(hContentType, "application/x-www-form-urlencoded")] "enabled=yes&recipients=venue%40example.com"
  check "public requests cannot configure recipients" (lookup hLocation (simpleHeaders denied) == Just "/admin/login")
  login <- request methodPost "/admin/login" [(hContentType, "application/x-www-form-urlencoded")] "password=fixture-password"
  let cookie = maybe "" (BC.takeWhile (/= ';')) (lookup "Set-Cookie" (simpleHeaders login))
      adminHeaders = [("Cookie", cookie), (hContentType, "application/x-www-form-urlencoded")]
  admin <- request methodGet "/admin/bookings" [("Cookie", cookie)] ""
  let marker = "name=\"csrf\" value=\""
      csrf = T.takeWhile (/= '"') (T.drop (T.length marker) (snd (T.breakOn marker (bodyText admin))))
      form suffix = BL.fromStrict (TE.encodeUtf8 ("csrf=" <> csrf <> suffix))
  check "admin UI offers recipient configuration and test send" (not (T.null csrf) && "Send test notification" `T.isInfixOf` bodyText admin)
  check "disabled notifications are explicit in the UI" ("OFF — no reservation emails will be sent" `T.isInfixOf` bodyText admin)
  disabledRecovery <- request methodPost "/admin/reservation-emails/booking" adminHeaders (form "&ticket=ABCD-2345")
  check "recovery respects disabled notifications" (statusCode (simpleStatus disabledRecovery) == 400)
  forgedRecovery <- request methodPost "/admin/reservation-emails/booking" adminHeaders "ticket=ABCD-2345"
  check "recovery needs CSRF even for an admin" (statusCode (simpleStatus forgedRecovery) == 403)
  forged <- request methodPost "/admin/reservation-emails/save" adminHeaders "enabled=yes&recipients=venue%40example.com"
  check "authenticated requests still require the email form CSRF token" (statusCode (simpleStatus forged) == 403)
  invalid <- request methodPost "/admin/reservation-emails/save" adminHeaders (form "&enabled=yes&recipients=venue%40example.com%0D%0ABcc%3Aintruder%40example.com")
  check "header injection cannot configure recipients" (statusCode (simpleStatus invalid) == 400)
  savedEmails <- request methodPost "/admin/reservation-emails/save" adminHeaders (form "&enabled=yes&recipients=venue%40example.com%2Cschool%40example.com%2Cvenue%40example.com")
  check "admin can save deduplicated recipients" (statusCode (simpleStatus savedEmails) == 303)
  _ <- paid "pi_fixture_notify" [("sww_code", JStr "PQRS-6789"), ("sww_product", JStr "Group Double"), ("sww_seats", JStr "2")] "succeeded"
  hooked <- schedule "PQRS-6789"
  check "paid ticket redemption invokes the hook" (statusCode (simpleStatus hooked) == 303)
  queued <- readReservationEmails lessons
  check "one private message per recipient, with reservation details" (length (emailsDeliveries queued) == 2 && all (\d -> all (`T.isInfixOf` deliveryBody d) ["PQRS-6789", "2027-01-13", "Places reserved: 2", "Preferred name: Fixture"] && "2 places reserved" `T.isPrefixOf` deliverySubject d) (emailsDeliveries queued))
  currentTickets <- listTickets lessons
  let doubleTickets = filter ((== "PQRS-6789") . tkCode) currentTickets
      doublePage = renderHtml (bookingsPage doubleTickets [slot] emptyBundle queued csrf Nothing)
  check "a double ticket occupies and displays two places" (takenFor doubleTickets slot == 2 && "2 booked / 8 places" `T.isInfixOf` doublePage)
  combinedPage <- request methodGet "/admin/bookings" [("Cookie", cookie)] ""
  check "admin sums places across different ticket sizes" ("3 booked / 8 places" `T.isInfixOf` bodyText combinedPage)
  _ <- schedule "PQRS-6789"
  _ <- schedule code
  unchangedQueue <- readReservationEmails lessons
  check "duplicate redemption and payment previews do not notify" (unchangedQueue == queued)
  reloadedLessons <- newBookingStore (dir </> "bookings.json")
  restored <- readReservationEmails reloadedLessons
  check "reservation, configuration and queued emails survive restart together" (restored == queued)
  sent <- newIORef ([] :: [T.Text])
  let transport d = atomicModifyIORef' sent (\ids -> (deliveryId d : ids, Sent "provider-fixture"))
  doneA <- newEmptyMVar
  doneB <- newEmptyMVar
  _ <- forkIO (drainReservationEmails lessons transport >> putMVar doneA ())
  _ <- forkIO (drainReservationEmails lessons transport >> putMVar doneB ())
  takeMVar doneA
  takeMVar doneB
  drainReservationEmails lessons transport
  deliveredIds <- readIORef sent
  check "concurrent workers and repeated ticks send each job once" (length deliveredIds == 2 && length (M.fromList [(ident, ()) | ident <- deliveredIds]) == 2)
  beforeRecovery <- listTickets lessons
  recovered <- request methodPost "/admin/reservation-emails/booking" adminHeaders (form "&ticket=ABCD-2345")
  recoveryQueue <- readReservationEmails lessons
  afterRecovery <- listTickets lessons
  check "admin recovers an earlier missed reservation without consuming capacity" (statusCode (simpleStatus recovered) == 303 && beforeRecovery == afterRecovery && length (emailsDeliveries recoveryQueue) == 4)
  _ <- request methodPost "/admin/reservation-emails/booking" adminHeaders (form "&ticket=ABCD-2345")
  _ <- request methodPost "/admin/reservation-emails/booking" adminHeaders (form "&ticket=PQRS-6789")
  duplicateRecovery <- readReservationEmails lessons
  check "recovery never duplicates accepted or failed recipient jobs" (duplicateRecovery == recoveryQueue)
  unknownRecovery <- request methodPost "/admin/reservation-emails/booking" adminHeaders (form "&ticket=UNKNOWN")
  check "unknown tickets cannot produce a reservation email" (statusCode (simpleStatus unknownRecovery) == 400)
  let Right recipient = parseMailbox "test@example.com"
      Right sender = parseMailbox "sender@example.com"
      specimen = reservationDelivery True "fixture" "now" "TEST-ONLY" "Double" "2" slot recipient
      mime = TE.decodeUtf8 (reservationMime "Okashi School" sender specimen)
      (headers, encodedBody) = T.breakOn "\r\n\r\n" mime
      decodedBody = convertFromBase Base64 (TE.encodeUtf8 (T.filter (`notElem` ['\r', '\n']) (T.drop 4 encodedBody))) :: Either String BS.ByteString
  check "provider MIME carries the school identity and the actual seat count" ("From: =?UTF-8?B?T2thc2hpIFNjaG9vbA==?= <sender@example.com>\r\n" `T.isPrefixOf` headers && decodedBody == Right (TE.encodeUtf8 (deliveryBody specimen)))
  let hostile = TE.decodeUtf8 (reservationMime "School\r\nBcc: other@example.com" sender specimen)
  check "sender display names cannot inject headers" (not ("\r\nBcc:" `T.isInfixOf` hostile))
  let aliasResponse state address = JObj [("sendAs", JArr [JObj [("sendAsEmail", JStr address), ("verificationStatus", JStr state)]])]
  check "only the requested verified alias can send" (verifiedSendAs sender (aliasResponse "accepted" "sender@example.com") == Right sender)
  check "pending and mismatched aliases cannot silently fall back" (all (either (const True) (const False) . verifiedSendAs sender) [aliasResponse "pending" "sender@example.com", aliasResponse "accepted" "someone@example.com", JObj []])
  let private = reservationDelivery False "private" "now" "PRIVATE" "Private" "all" slot recipient
  check "private-class messages state the full numerical capacity" ("Places reserved: 8 (private class" `T.isInfixOf` deliveryBody private)
  testId <- queueTestEmail lessons slot recipient
  drainReservationEmails lessons (\_ -> pure (Failed "fixture rejected"))
  _ <- retryReservationEmail lessons testId
  drainReservationEmails lessons transport
  delivered <- readReservationEmails lessons
  check "failed test notification is retryable and visibly labelled" (any (\d -> deliveryId d == testId && deliveryState d == Sent "provider-fixture" && "[TEST]" `T.isPrefixOf` deliverySubject d) (emailsDeliveries delivered))
  secondRetry <- retryReservationEmail lessons testId
  check "accepted messages cannot be resent through retry" (not secondRetry)
  _ <- queueTestEmail lessons slot recipient
  _ <- claimReservationEmail lessons
  interruptedStore <- newBookingStore (dir </> "bookings.json")
  interrupted <- readReservationEmails interruptedStore
  check "crash during send is marked uncertain instead of queued again" (any (\d -> case deliveryState d of Uncertain _ -> True; _ -> False) (emailsDeliveries interrupted))
  beforeTest <- listTickets lessons
  testResponse <- request methodPost "/admin/reservation-emails/test" adminHeaders (form "&testRecipient=ep%40memorici.de")
  afterTest <- listTickets lessons
  check "admin test send never consumes a ticket or creates a reservation" (statusCode (simpleStatus testResponse) == 303 && beforeTest == afterTest)
  blankRecipients <- request methodPost "/admin/reservation-emails/save" adminHeaders (form "&enabled=yes&recipients=")
  check "an enabled hook must have recipients" (statusCode (simpleStatus blankRecipients) == 400)
  oldBundle <- loadBundle secretFile master
  let connected = oldBundle {sbGoogleClientId = Just (SecretValue "client"), sbGoogleClientSecret = Just (SecretValue "client-secret"), sbGoogleRefreshToken = Just (SecretValue "calendar-token")}
  Right () <- saveBundle secretFile master connected
  check "existing shared Google connections retain their sender" ((secretText . gcRefreshToken <$> googleMailCredsOf connected) == Just "calendar-token")
  forgedAccount <- request methodPost "/admin/reservation-emails/account" adminHeaders "googleMailRefreshToken=mail-token"
  check "sender changes require CSRF" (statusCode (simpleStatus forgedAccount) == 403)
  savedAccount <- request methodPost "/admin/reservation-emails/account" adminHeaders (form "&googleMailRefreshToken=mail-token")
  storedAccount <- loadBundle secretFile master
  check "school sender is encrypted and independent of the calendar identity" (statusCode (simpleStatus savedAccount) == 303 && (secretText . gcRefreshToken <$> googleMailCredsOf storedAccount) == Just "mail-token" && (secretText . gcRefreshToken <$> googleCredsOf storedAccount) == Just "calendar-token" && (secretText <$> sbStripeWebhook storedAccount) == Just webhookSecret)
  blankAccount <- request methodPost "/admin/reservation-emails/account" adminHeaders (form "&googleMailRefreshToken=")
  accountPage <- request methodGet "/admin/bookings" [("Cookie", cookie)] ""
  check "empty sender updates preserve the account and secrets are never rendered" (statusCode (simpleStatus blankAccount) == 400 && not ("mail-token" `T.isInfixOf` bodyText accountPage))
  savedAlias <- request methodPost "/admin/reservation-emails/account" adminHeaders (form "&googleMailRefreshToken=mail-token&googleMailFrom=hello%40okashi-school.com")
  aliasBundle <- loadBundle secretFile master
  check "sender alias survives encryption independently of calendar credentials" (statusCode (simpleStatus savedAlias) == 303 && (googleMailCredsOf aliasBundle >>= gcMailFrom) == either (const Nothing) Just (parseMailbox "hello@okashi-school.com") && (googleCredsOf aliasBundle >>= gcMailFrom) == Nothing)
  injectedAlias <- request methodPost "/admin/reservation-emails/account" adminHeaders (form "&googleMailRefreshToken=other-token&googleMailFrom=hello%40okashi-school.com%0D%0ABcc%3Aother%40example.com")
  afterInjection <- loadBundle secretFile master
  check "invalid alias updates preserve both sender and token" (statusCode (simpleStatus injectedAlias) == 400 && sbGoogleMailFrom afterInjection == sbGoogleMailFrom aliasBundle && (secretText <$> sbGoogleMailRefreshToken afterInjection) == Just "mail-token")
  brokenBooking <- newBookingStore (dir </> "missing-booking" </> "bookings.json")
  failedBooking <- try (issueTicket brokenBooking "pi_storage" "K7Q2-M4XW" "Class" "1") :: IO (Either IOException (Either T.Text (Ticket, Bool)))
  invisibleBooking <- listTickets brokenBooking
  check "failed booking persistence publishes neither ticket nor outbox" (either (const True) (const False) failedBooking && null invisibleBooking)
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
