{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, throwIO, try)
import Control.Monad (unless, when)
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (hContentType, hLocation, methodGet, methodPost, statusCode)
import Network.Wai (defaultRequest, requestHeaders, requestMethod)
import Network.Wai.Test (SRequest (..), runSession, setPath, simpleBody, simpleHeaders, simpleStatus, srequest)
import StickyWM (JSON (..), decode, encode, (.:?))
import Swwstructor.Checkout (CheckoutError (..), Url (..))
import Swwstructor.Content (Availability (..), PageSpec (..), SectionSpec (..), SiteSpec (..), SlotSpec (..), slotIdOf)
import Swwstructor.Money (Currency (GBP), price)
import Swwstructor.Plugins.StripeCatalog (CatalogItem (..), parsePriceCatalogJSON)
import Swwstructor.Server.App (AppEnv (..), Stage (Test), swwApp)
import Swwstructor.Server.Booking
import Swwstructor.Server.Catalog (newCatalogCache)
import Swwstructor.Server.DirectStripe
import Swwstructor.Server.Notify (BookingCtx (..))
import Swwstructor.Server.PaymentCodes (newPaymentCodeStore)
import Swwstructor.Server.SecretStore (SecretBundle (..), SecretValue (..), emptyBundle, genMasterKey, saveBundle)
import Swwstructor.Server.Store (modifySite, newSiteStore, saveSiteFile)
import Swwstructor.Server.Venues (Venue (..), listVenues, newVenueStore, upsertVenue, venueLocation)
import Swwstructor.Theme (nytTheme)
import System.Directory (createDirectory, doesFileExist, getTemporaryDirectory, removeFile, removePathForcibly)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Web.Scotty (scottyApp)

check :: String -> Bool -> IO ()
check label ok = unless ok (fail label)

main :: IO ()
main = bracket fixture removePathForcibly exercise

exercise :: FilePath -> IO ()
exercise dir = do
  let first = SlotSpec "2030-01-07" "19:00" "21:00" "Ukiyo, Covent Garden" 3 0 "Mochi with tea"
      lastPlace = first {slotDate = "2030-01-08", slotCap = 1}
      later = first {slotDate = "2030-01-09", slotCap = 8}
      past = first {slotDate = "2020-01-01", slotCap = 8}
      slots = [first, lastPlace, later, past]
      single = CatalogItem "One person" "Transferable ticket" Nothing "price_single" (price GBP 80) "1" OnSale
      double = CatalogItem "Two people" "Transferable ticket" Nothing "price_double" (price GBP 150) "2" OnSale
      paused = single {ciPriceId = "price_paused", ciAvailability = NotOffered}
      prices = ["price_single", "price_double", "price_paused"]
      siteData = SiteSpec "Okashi School" "Fixture" Nothing nytTheme [] Nothing
        [ PageSpec "/classes" "Classes" [UpcomingClasses (Just "Book a class") Nothing prices [], ScheduleClass Nothing Nothing 0 slots]
        , PageSpec "/gifts" "Gifts" [ScheduleClass Nothing Nothing 0 slots]
        ] []
      webhookSecret = "fixture-webhook-secret"
      secretPath = dir </> "secrets.enc"
  either (fail . T.unpack) pure =<< saveSiteFile (dir </> "site.json") siteData
  site <- either fail pure =<< newSiteStore (dir </> "site.json") dir
  master <- genMasterKey
  either (fail . T.unpack) pure =<< saveBundle secretPath master emptyBundle {sbStripeWebhook = Just (SecretValue webhookSecret)}
  bookings <- newBookingStore (dir </> "bookings.json")
  payments <- newPaymentCodeStore (dir </> "payment-codes.json")
  venues <- newVenueStore (dir </> "venues.json")
  upsertVenue venues (Venue "ukiyo" "UKIYO" "8 Slingsby Place, London WC2E 9AB" "Ukiyo, Covent Garden" "" "" True)
  manager <- newManager defaultManagerSettings
  adminSession <- newIORef 1
  spent <- newIORef M.empty
  catalog <- newCatalogCache
  items <- newIORef (Right [single, double, paused])
  sessions <- newIORef M.empty
  creates <- newIORef ([] :: [[(T.Text, T.Text)]])
  loseCreate <- newIORef False
  rejectCreate <- newIORef False
  failExpiry <- newIORef False
  let stripe = DirectStripe
        { directCreate = \request -> do
            let code = drCode request
                parameters = directFormParams request
                candidate = DirectSession ("cs_" <> T.filter (/= '-') code) code (drExpiresAt request)
                  (SessionOpen (Url ("https://checkout.stripe.com/test/" <> code)))
            atomicModifyIORef' creates (\seen -> (parameters : seen, ()))
            rejected <- readIORef rejectCreate
            if rejected then pure (Left (CheckoutHttpStatus 400 "fixture definite validation rejection")) else do
              session <- atomicModifyIORef' sessions $ \existing -> case M.lookup code existing of
                Just original -> (existing, original)
                Nothing -> (M.insert code candidate existing, candidate)
              lost <- atomicModifyIORef' loseCreate (\lose -> (False, lose))
              pure (if lost then Left (CheckoutTransport "fixture response lost after creation") else Right session)
        , directRetrieve = \sid -> do
            existing <- readIORef sessions
            pure $ maybe (Left (CheckoutHttpStatus 404 "fixture session missing")) Right
              (lookup sid [(dsId session, session) | session <- M.elems existing])
        , directExpire = \sid -> do
            failed <- readIORef failExpiry
            if failed then pure (Left (CheckoutTransport "fixture expiry unavailable")) else
              atomicModifyIORef' sessions $ \existing -> case [session | session <- M.elems existing, dsId session == sid] of
                [] -> (existing, Left (CheckoutHttpStatus 404 "fixture session missing"))
                session : _ -> case dsState session of
                  SessionPaid _ _ -> (existing, Right session)
                  _ -> let expired = session {dsState = SessionExpired}
                        in (M.insert (dsCode session) expired existing, Right expired)
        }
      ctx = BookingCtx manager bookings False (pure Nothing) (pure Nothing) [] "Okashi School" "https://okashi-school.com" "Europe/London" "+00:00" (pure slots) (listVenues venues) "kitchen"
      env = AppEnv
        { aeStore = site, aeBaseUrl = "https://okashi-school.com", aeStage = Test
        , aeSecretFile = secretPath, aeMasterKey = master, aeSession = adminSession
        , aeAdminPassword = "fixture-password", aeManager = manager, aeSiteDir = dir, aeDataDir = dir
        , aeCatalog = catalog, aeSpentPow = spent, aeBookings = bookings, aePaymentCodes = payments
        , aeBookingCtx = ctx, aeVenues = venues, aeAnalytics = Nothing
        , aeDirectStripe = stripe, aeDatedCatalog = readIORef items
        }
  app <- scottyApp (swwApp env)
  let request method requestPath headers body = runSession (srequest (SRequest ((setPath defaultRequest requestPath) {requestMethod = method, requestHeaders = headers}) body)) app
      get requestPath = request methodGet requestPath [] ""
      form fields = BL.fromStrict (TE.encodeUtf8 (T.intercalate "&" [key <> "=" <> value | (key, value) <- fields]))
      book cookie slot item = request methodPost "/book" [("Cookie", cookie), (hContentType, "application/x-www-form-urlencoded")]
        (form [("slot", slotIdOf slot), ("price", ciPriceId item), ("expectedAmount", if ciPriceId item == "price_double" then "15000" else "8000"), ("expectedCurrency", "gbp")])
      newCookie = do
        response <- get "/classes"
        let cookie = maybe "" (BC.takeWhile (/= ';')) (lookup "Set-Cookie" (simpleHeaders response))
        check "GET class page issues a signed retry cookie" ("sww_booking=" `BS.isPrefixOf` cookie)
        pure cookie
      codeOf cookie = TE.decodeUtf8 (BC.takeWhile (/= ':') (BS.drop 12 cookie))
      status response = statusCode (simpleStatus response)
      textOf = TE.decodeUtf8 . BL.toStrict . simpleBody
      path prefix code suffix = TE.encodeUtf8 (prefix <> code <> suffix)
      rawEvent typ obj = TE.encodeUtf8 (T.pack (encode (JObj [("type", JStr typ), ("data", JObj [("object", obj)])])))
      signed rawBytes = do
        timestamp <- BC.pack . show . (floor :: Double -> Integer) . realToFrac <$> getPOSIXTime
        let digest = convertToBase Base16 (hmacGetDigest (hmac (TE.encodeUtf8 webhookSecret) (timestamp <> "." <> rawBytes) :: HMAC SHA256)) :: BS.ByteString
        request methodPost "/stripe/webhook" [(hContentType, "application/json"), ("Stripe-Signature", "t=" <> timestamp <> ",v1=" <> digest)] (BL.fromStrict rawBytes)
      payment code ident amount = rawEvent "payment_intent.succeeded" (JObj
        [("id", JStr ident), ("status", JStr "succeeded"), ("amount_received", JNum amount), ("currency", JStr "gbp")
        , ("metadata", JObj [("sww_code", JStr (T.unpack code)), ("sww_flow", JStr "dated")])])
      expiryEvent session = rawEvent "checkout.session.expired" (JObj
        [("id", JStr (T.unpack (dsId session))), ("status", JStr "expired"), ("payment_status", JStr "unpaid")
        , ("expires_at", JNum (fromInteger (dsExpiresAt session)))
        , ("metadata", JObj [("sww_code", JStr (T.unpack (dsCode session))), ("sww_flow", JStr "dated")])])
      tokenOf response = let marker = "name=\"csrf\" value=\"" in T.takeWhile (/= '"') (T.drop (T.length marker) (snd (T.breakOn marker (textOf response))))
      cancel code token = request methodPost (path "/checkout/" code "/cancel") [(hContentType, "application/x-www-form-urlencoded")] (form [("csrf", token)])
      occupancy slot = map slotTaken <$> bookingAvailability bookings [slot]
      checkDatedLine code item amount people = do
        requests <- readIORef creates
        parameters <- case filter ((== Just code) . lookup "metadata[sww_code]") requests of
          [one] -> pure one
          _ -> fail "The dated checkout fixture must have exactly one create request."
        check "dated checkout charges the saved total once in the saved currency"
          (lookup "line_items[0][price_data][unit_amount]" parameters == Just amount &&
           lookup "line_items[0][price_data][currency]" parameters == Just "gbp" &&
           lookup "line_items[0][quantity]" parameters == Just "1")
        check "Stripe primary item states the promised date, time and number of people"
          (maybe False (\name -> all (`T.isInfixOf` name) [slotDate first, "19:00–21:00", people])
            (lookup "line_items[0][price_data][product_data][name]" parameters))
        check "dated checkout does not inherit gift product copy"
          (lookup "line_items[0][price]" parameters == Nothing &&
           lookup "line_items[0][price_data][product_data][description]" parameters == Nothing)
        check "original catalogue price remains traceable on checkout and payment metadata"
          (lookup "metadata[sww_price_id]" parameters == Just (ciPriceId item) &&
           lookup "payment_intent_data[metadata][sww_price_id]" parameters == Just (ciPriceId item) &&
           lookup "metadata[sww_product]" parameters == Just (ciName item) &&
           lookup "metadata[sww_seats]" parameters == Just (ciSeats item))
  noCookie <- book "" first single
  check "direct POST without a signed browser attempt is rejected" (status noCookie == 400)
  forged <- book "sww_booking=AAAA-2222:1:forged" first single
  check "forged signed-attempt cookies cannot reserve places" (status forged == 400)
  ordinaryCookie <- newCookie
  let ordinaryCode = codeOf ordinaryCookie
  stalePrice <- request methodPost "/book" [("Cookie", ordinaryCookie), (hContentType, "application/x-www-form-urlencoded")]
    (form [("slot", slotIdOf first), ("price", "price_single"), ("expectedAmount", "1"), ("expectedCurrency", "gbp")])
  check "a stale or forged browser price is refused before Stripe" (status stalePrice == 409)
  wrongCurrency <- request methodPost "/book" [("Cookie", ordinaryCookie), (hContentType, "application/x-www-form-urlencoded")]
    (form [("slot", slotIdOf first), ("price", "price_single"), ("expectedAmount", "8000"), ("expectedCurrency", "usd")])
  check "a forged browser currency is refused before Stripe" (status wrongCurrency == 409)
  forgedDate <- book ordinaryCookie (first {slotDate = "2030-02-01"}) single
  check "an unauthored date cannot start payment" (status forgedDate == 400)
  oldDate <- book ordinaryCookie past single
  check "a past authored date cannot start payment" (status oldDate == 400)
  disabled <- book ordinaryCookie first paused
  check "a paused Stripe product cannot start payment" (status disabled == 409)
  forbiddenPrice <- book ordinaryCookie first single {ciPriceId = "price_not_allowed"}
  check "a price outside the authored allowlist cannot start payment" (status forbiddenPrice == 400)
  foreignOrigin <- request methodPost "/book" [("Cookie", ordinaryCookie), ("Origin", "https://elsewhere.example"), (hContentType, "application/x-www-form-urlencoded")]
    (form [("slot", slotIdOf first), ("price", "price_single"), ("expectedAmount", "8000"), ("expectedCurrency", "gbp")])
  check "cross-origin attempts are rejected" (status foreignOrigin == 403)
  check "rejected input creates no external checkout" . null =<< readIORef creates
  writeIORef items (Left "fixture catalogue unavailable")
  unavailable <- book ordinaryCookie first single
  check "catalogue uncertainty fails closed without holding places" (status unavailable == 503)
  check "catalogue uncertainty creates no holds" . null =<< listPendingCheckoutHolds bookings
  writeIORef items (Right [single, double, paused])
  bought <- book ordinaryCookie first single
  check "one POST reaches Stripe for the selected class" (status bought == 303 && maybe False (BS.isPrefixOf "https://checkout.stripe.com/") (lookup hLocation (simpleHeaders bought)))
  checkDatedLine ordinaryCode single "8000" "1 person"
  repeated <- book ordinaryCookie first single
  check "double clicking returns to the same checkout" (lookup hLocation (simpleHeaders repeated) == lookup hLocation (simpleHeaders bought))
  check "double clicking creates one session" . ((== 1) . M.size) =<< readIORef sessions
  switched <- book ordinaryCookie later single
  check "an open browser attempt cannot silently switch dates" (status switched == 409)
  check "checkout has not issued a paid ticket prematurely" . null =<< listTickets bookings
  badSignature <- request methodPost "/stripe/webhook" [("Stripe-Signature", "t=1,v1=bad")] (BL.fromStrict (payment ordinaryCode "pi_single" 8000))
  check "unsigned payment evidence cannot confirm a reservation" (status badSignature == 400)
  check "rejected webhook leaves payment unconfirmed" . null =<< listTickets bookings
  wrongPrice <- signed (payment ordinaryCode "pi_wrong_price" 1)
  check "signed but mismatched payment value cannot consume a hold" (status wrongPrice == 503)
  paid <- signed (payment ordinaryCode "pi_single" 8000)
  replay <- signed (payment ordinaryCode "pi_single" 8000)
  check "payment webhook automatically redeems once without a browser return" (status paid == 200 && status replay == 200)
  tickets <- listTickets bookings
  check "paid webhook produced one ordinary redeemed ticket" (case tickets of
    [ticket] -> maybe False ((== slotIdOf first) . redSlotId) (tkRedemption ticket)
    _ -> False)
  paidRetry <- book ordinaryCookie first single
  check "retrying after payment returns to the existing booking" (lookup hLocation (simpleHeaders paidRetry) == Just (path "/booking/" ordinaryCode ""))
  bookedPage <- get (path "/booking/" ordinaryCode "")
  check "confirmed page needs no further date selection or ticket redemption" (status bookedPage == 200 && "You're booked!" `T.isInfixOf` textOf bookedPage && "Add to my calendar" `T.isInfixOf` textOf bookedPage)
  nameSaved <- request methodPost (path "/booking/" ordinaryCode "/name") [(hContentType, "application/x-www-form-urlencoded")]
    (form [("csrf", tokenOf bookedPage), ("name", "Momo")])
  check "preferred name is optional after booking and can be saved" (status nameSaved == 303)
  doubleCookie <- newCookie
  let doubleCode = codeOf doubleCookie
  doubleCheckout <- book doubleCookie first double
  check "two-person option reaches checkout with two places held" (status doubleCheckout == 303)
  checkDatedLine doubleCode double "15000" "2 people"
  check "one paid place plus a double hold fills three places" . (== [3]) =<< occupancy first
  _ <- signed (payment doubleCode "pi_double" 15000)
  check "double payment consumes its two places exactly once" . (== [3]) =<< occupancy first

  -- Gift entitlement remains an ordinary unredeemed ticket until /schedule.
  let giftCode = "GGGG-2222"
      giftEvent = rawEvent "payment_intent.succeeded" (JObj [("id", JStr "pi_gift"), ("status", JStr "succeeded"), ("metadata", JObj [("sww_code", JStr (T.unpack giftCode)), ("sww_product", JStr "Gift"), ("sww_seats", JStr "1")])])
  giftPaid <- signed giftEvent
  check "old gift payment still issues a transferable unredeemed ticket" (status giftPaid == 200)
  gifts <- listTickets bookings
  check "gift purchase reserves no dated capacity" (any (\t -> tkCode t == giftCode && tkRedemption t == Nothing) gifts)
  raceCookie <- newCookie
  let redeemGift = request methodPost "/schedule" [(hContentType, "application/x-www-form-urlencoded")]
        (form [("code", giftCode), ("name", "GiftRecipient"), ("slot", slotIdOf lastPlace)])
  (datedRace, giftRace) <- concurrently (book raceCookie lastPlace single) redeemGift
  check "gift redemption and dated checkout share one last-place decision" ((status datedRace == 303) /= (status giftRace == 303))
  check "the real HTTP race cannot oversell the last place" . (== [1]) =<< occupancy lastPlace

  cancelCookie <- newCookie
  let cancelCode = codeOf cancelCookie
  _ <- book cancelCookie later single
  cancelPage <- get (path "/checkout/" cancelCode "")
  deniedCancel <- cancel cancelCode "forged"
  check "cancellation requires the checkout page token" (status deniedCancel == 403)
  writeIORef failExpiry True
  uncertainCancel <- cancel cancelCode (tokenOf cancelPage)
  check "uncertain Stripe expiry cannot release a place" (status uncertainCancel == 503)
  check "uncertain cancellation retains capacity" . (== [1]) =<< occupancy later
  writeIORef failExpiry False
  cancelled <- cancel cancelCode (tokenOf cancelPage)
  check "verified cancellation releases capacity and returns to classes" (status cancelled == 303 && lookup hLocation (simpleHeaders cancelled) == Just "/classes")
  check "cancelled checkout releases its place" . (== [0]) =<< occupancy later

  expiryCookie <- newCookie
  let expiryCode = codeOf expiryCookie
  _ <- book expiryCookie later single
  expirySessions <- readIORef sessions
  expirySession <- maybe (fail "Expiry fixture has no Stripe session") pure (M.lookup expiryCode expirySessions)
  wrongSession <- signed (expiryEvent expirySession {dsId = "cs_unrelated"})
  wrongDeadline <- signed (expiryEvent expirySession {dsExpiresAt = dsExpiresAt expirySession + 1})
  check "expiry evidence must match the saved session and deadline" (status wrongSession == 503 && status wrongDeadline == 503)
  check "mismatched expiry cannot release capacity" . (== [1]) =<< occupancy later
  ended <- signed (expiryEvent expirySession)
  endedAgain <- signed (expiryEvent expirySession)
  check "verified expiry is durable and safe to replay" (status ended == 200 && status endedAgain == 200)
  check "Stripe expiry releases capacity without a browser return" . (== [0]) =<< occupancy later

  uncertainCookie <- newCookie
  let uncertainCode = codeOf uncertainCookie
  writeIORef loseCreate True
  uncertain <- book uncertainCookie later single
  check "a lost create response keeps the attempt recoverable" (status uncertain == 503)
  check "uncertain creation protects the place" . (== [1]) =<< occupancy later
  recovered <- book uncertainCookie later single
  check "retry after lost create response resumes payment" (status recovered == 303)
  requests <- readIORef creates
  let ownRequests = filter ((== Just uncertainCode) . lookup "metadata[sww_code]") requests
  check "uncertain create retries exactly the persisted Stripe request" (case ownRequests of
    [retry, original] -> retry == original
    _ -> False)
  atomicModifyIORef' sessions (\ss -> (M.adjust (\s -> s {dsState = SessionPaid "pi_return_recovery" (price GBP 80)}) uncertainCode ss, ()))
  recoveredBooking <- get (path "/booking/" uncertainCode "")
  check "return page can recover a paid session before webhook delivery" (status recoveredBooking == 200 && "Add to my calendar" `T.isInfixOf` textOf recoveredBooking)

  rejectedCookie <- newCookie
  writeIORef rejectCreate True
  rejectedCreate <- book rejectedCookie later single
  writeIORef rejectCreate False
  check "definitive Stripe validation rejection ends the unpaid attempt" (status rejectedCreate == 503)
  rejectedHold <- lookupCheckoutHold bookings (codeOf rejectedCookie)
  check "a definitely never-created session releases its hold" (maybe False ((== HoldReleased) . chState) rejectedHold)

  completedCookie <- newCookie
  let completedCode = codeOf completedCookie
  _ <- book completedCookie later single
  atomicModifyIORef' sessions (\ss -> (M.adjust (\s -> s {dsState = SessionUnpaidComplete}) completedCode ss, ()))
  unpaidComplete <- get (path "/booking/" completedCode "")
  check "completed but unpaid checkout remains pending" (status unpaidComplete == 200 && not ("Add to my calendar" `T.isInfixOf` textOf unpaidComplete))
  check "unpaid completion does not release its capacity" . (== [2]) =<< occupancy later
  completedHold <- lookupCheckoutHold bookings completedCode
  check "unpaid completion cannot issue a paid ticket" (maybe False (\h -> case chState h of HoldCheckout{} -> True; _ -> False) completedHold)

  paidCancelCookie <- newCookie
  let paidCancelCode = codeOf paidCancelCookie
  _ <- book paidCancelCookie later single
  paidCancelPage <- get (path "/checkout/" paidCancelCode "")
  atomicModifyIORef' sessions (\ss -> (M.adjust (\s -> s {dsState = SessionPaid "pi_cancel_race" (price GBP 80)}) paidCancelCode ss, ()))
  paidDuringCancel <- cancel paidCancelCode (tokenOf paidCancelPage)
  check "payment winning a cancellation race confirms the booking" (status paidDuringCancel == 303 && lookup hLocation (simpleHeaders paidDuringCancel) == Just (path "/booking/" paidCancelCode ""))
  check "paid cancellation race never releases a paid place" . (== [3]) =<< occupancy later

  -- A future content change cannot erase the date/class snapshot of a payment.
  either (fail . T.unpack) pure =<< modifySite site (\s -> s {sitePages = [PageSpec "/classes" "Classes" [UpcomingClasses Nothing Nothing prices [], ScheduleClass Nothing Nothing 0 [later]]]})
  upsertVenue venues (Venue "ukiyo" "UKIYO" "A different future address" "Ukiyo, Covent Garden" "" "" True)
  snapshotPage <- get (path "/booking/" ordinaryCode "")
  calendar <- get (path "/booking/" ordinaryCode "/calendar.ics")
  check "removing the class from sale does not erase a paid booking" (status snapshotPage == 200 && "Mochi with tea" `T.isInfixOf` textOf snapshotPage)
  check "calendar retains paid date/time after authored schedule removal" (status calendar == 200 && "DTSTART:20300107T190000Z" `T.isInfixOf` textOf calendar)
  check "confirmed booking includes the immediate venue address" ("8 Slingsby Place" `T.isInfixOf` T.replace "\r\n " "" (textOf calendar))
  parserChecks
  priceCatalogChecks
  putStrLn "Direct checkout HTTP, webhook, recovery, gift compatibility and parser checks passed."

parserChecks :: IO ()
parserChecks = do
  let prefixedVenue = Venue "ukiyo" "UKIYO" "UKIYO, 8 Slingsby Place, London WC2E 9AB" "Covent Garden" "" "" True
  check "an address already starting with its venue name receives no duplicate prefix"
    (venueLocation [prefixedVenue] "Covent Garden" == "UKIYO, 8 Slingsby Place, London WC2E 9AB")
  -- Reduced schema from actual Stripe sandbox responses, with synthetic ids,
  -- reference and URL. No ephemeral checkout capability enters the repository.
  let session state paymentStatus intent url = JObj
        [("id", JStr "cs_test_fixture"), ("status", JStr state), ("payment_status", JStr paymentStatus)
        , ("payment_intent", intent), ("amount_total", JNum 8000), ("currency", JStr "gbp")
        , ("expires_at", JNum 1900000000), ("url", url), ("livemode", JBool False)
        , ("metadata", JObj [("sww_flow", JStr "dated"), ("sww_code", JStr "AAAA-2222")])]
      wrapped object = TE.encodeUtf8 (T.pack (encode (JObj [("data", JObj [("object", object)])])))
      parsed state = directSessionOf (wrapped state)
  check "Stripe open sessions accept a null PaymentIntent before payment" (maybe False (\s -> case dsState s of SessionOpen _ -> True; _ -> False) (parsed (session "open" "unpaid" JNull (JStr "https://checkout.stripe.com/test/fixture"))))
  check "Stripe expired sessions accept a null URL and PaymentIntent" (maybe False ((== SessionExpired) . dsState) (parsed (session "expired" "unpaid" JNull JNull)))
  check "completed unpaid sessions retain capacity rather than claiming payment" (maybe False ((== SessionUnpaidComplete) . dsState) (parsed (session "complete" "unpaid" JNull JNull)))
  check "paid sessions preserve the exact payment amount and currency" (maybe False ((== SessionPaid "pi_fixture" (price GBP 80)) . dsState) (parsed (session "complete" "paid" (JStr "pi_fixture") JNull)))
  check "a hostile checkout redirect URL is rejected" (parsed (session "open" "unpaid" JNull (JStr "https://attacker.example/pay")) == Nothing)
  let actualPath = "/Users/sweater/.local/state/okashi-direct-booking-20261031/stripe-sandbox-session.json"
  actualExists <- doesFileExist actualPath
  when actualExists $ do
    bytes <- BS.readFile actualPath
    actual <- either fail pure (decode (T.unpack (TE.decodeUtf8 bytes)))
    mapM_ (\key -> case actual .:? key of
      Nothing -> fail "The optional sandbox evidence is missing a response."
      Just response -> check "actual Stripe sandbox response matches the production session parser" (directSessionOf (wrapped response) /= Nothing)) ["created", "retrieved", "expired", "readback"]

priceCatalogChecks :: IO ()
priceCatalogChecks = do
  let productObject active defaultPrice seats = JObj
        [("name", JStr "Gift class ticket"), ("active", JBool active), ("default_price", defaultPrice)
        , ("description", JStr "Choose your date later; tickets can be given to a friend.")
        , ("images", JArr [JStr "https://example.invalid/class.jpg"])
        , ("metadata", JObj [("sww_seats", JStr seats)])]
      priceObject ident amount currency active kind productObject' = JObj
        [("id", JStr ident), ("unit_amount", JNum amount), ("currency", JStr currency)
        , ("active", JBool active), ("type", JStr kind), ("product", productObject')]
      ordinary ident amount = priceObject ident amount "gbp" True "one_time" (productObject True (JStr ident) "1")
      parsePage hasMore entries = parsePriceCatalogJSON (encode (JObj [("has_more", JBool hasMore), ("data", JArr entries)]))
      single = ordinary "price_single" 8000
      double = priceObject "price_double" 15000 "gbp" True "one_time" (productObject True (JStr "price_double") "2")
      ignored =
        [ priceObject "price_old" 7000 "gbp" True "one_time" (productObject True (JStr "price_single") "1")
        , priceObject "price_archived_product" 8000 "gbp" True "one_time" (productObject False (JStr "price_archived_product") "1")
        , priceObject "price_archived_price" 8000 "gbp" False "one_time" (productObject True (JStr "price_archived_price") "1")
        , priceObject "price_recurring" 8000 "gbp" True "recurring" (productObject True (JStr "price_recurring") "1")
        , priceObject "price_inline" 8000 "gbp" True "one_time" (productObject True JNull "1")
        , priceObject "price_unknown_currency" 8000 "xyz" True "one_time" (productObject True (JStr "price_unknown_currency") "1")
        , ordinary "price_fraction" 8000.5
        , ordinary "price_negative" (-1)
        , ordinary "price_out_of_range" 1e30
        ]
  parsed <- either fail pure (parsePage False (double : ignored <> [single]))
  check "price catalogue preserves only active current one-time defaults, sorted by price"
    (map ciPriceId parsed == ["price_single", "price_double"] && map ciPrice parsed == [price GBP 80, price GBP 150])
  check "price catalogue preserves the gift explanation, image and seat entitlement"
    (case parsed of
      [_, item] -> ciSeats item == "2" && ciBlurb item == "Choose your date later; tickets can be given to a friend." && ciImage item == Just "https://example.invalid/class.jpg"
      _ -> False)
  check "partial catalogue is an error, never a misleading incomplete offer list"
    (case parsePage True [single] of Left _ -> True; Right _ -> False)
  check "catalogue requires an explicit completeness marker"
    (case parsePriceCatalogJSON (encode (JObj [("data", JArr [single])])) of Left _ -> True; Right _ -> False)
  check "zero-minor-unit amounts remain representable without rounding"
    (case parsePage False [ordinary "price_zero" 0] of Right [item] -> ciPrice item == price GBP 0; _ -> False)

fixture :: IO FilePath
fixture = do
  temp <- getTemporaryDirectory
  (path, handle) <- openTempFile temp "sww-direct-checkout"
  hClose handle
  removeFile path
  createDirectory path
  pure path

concurrently :: IO a -> IO b -> IO (a, b)
concurrently left right = do
  a <- newEmptyMVar
  b <- newEmptyMVar
  _ <- forkIO (capture left >>= putMVar a)
  _ <- forkIO (capture right >>= putMVar b)
  leftResult <- takeMVar a >>= either throwIO pure
  rightResult <- takeMVar b >>= either throwIO pure
  pure (leftResult, rightResult)
  where
    capture :: IO c -> IO (Either SomeException c)
    capture = try
