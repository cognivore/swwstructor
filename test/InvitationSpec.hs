{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, SomeException, bracket, throwIO, try)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import StickyWM (JSON (..), decode, encode, (.:?))
import Swwstructor.Content (SlotSpec (..), slotIdOf)
import Swwstructor.Money (Currency (GBP, USD), Price (..), Cents (..), price)
import Swwstructor.Server.Booking
import Swwstructor.Server.Invitation
import Swwstructor.Server.ReservationEmail (emailsDeliveries, parseMailbox)
import System.Directory (createDirectory, getTemporaryDirectory, removeFile, removePathForcibly, renameDirectory)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)

check :: String -> Bool -> IO ()
check label ok = unless ok (fail label)
holdRight :: Either HoldError a -> IO a
holdRight = either (fail . T.unpack . holdErrorMessage) pure
inviteRight :: Either InvitationError a -> IO a
inviteRight = either (fail . T.unpack . invitationErrorMessage) pure

main :: IO ()
main = bracket fixture removePathForcibly exercise

exercise :: FilePath -> IO ()
exercise dir = do
  now <- floor <$> getPOSIXTime
  let expiry = now + 1900
      slot = SlotSpec "2030-01-07" "19:00" "21:00" "Original kitchen address" 1 0 "Mochi"
      next = slot {slotDate = "2030-01-08"}
      open name = newBookingStore (dir </> name <> ".json")
      draft name tier scope = inviteRight (mkInvitationDraft name tier scope)
      newInvite store name tier scope = draft name tier scope >>= createInvitation store >>= inviteRight
      begin store slots code invitation = beginInvitationCheckout store slots 3 code (invitationToken invitation) "price_invitation" "One-person class ticket" expiry
      occupied store chosen = map slotTaken <$> bookingAvailability store [chosen]
      counts = length . emailsDeliveries
      totalPaid = sum . map (\purchase -> let Price (Cents amount) _ = ipPaidPrice purchase in amount)
      totalFees = sum . map (\purchase -> let Price (Cents amount) _ = iaReferralFee (ipAttribution purchase) in amount)
  check "referrer labels preserve Unicode and reject empty or oversized inputs"
    (isRight (mkInvitationDraft "Émilie — 和菓子" Friend GeneralTicket) &&
     isLeft (mkInvitationDraft "  " Friend GeneralTicket) &&
     isLeft (mkInvitationDraft (T.replicate 101 "x") Friend GeneralTicket) &&
     isLeft (mkInvitationDraft "Bad\nName" Partner GeneralTicket))
  check "event scope must contain a real date and time"
    (isLeft (mkInvitationDraft "Venue" Partner (EventTicket slot {slotDate = "not-a-date"})))

  store <- open "main"
  recipient <- either (fail . T.unpack) pure (parseMailbox "venue@example.com")
  configureReservationEmails store True [recipient]
  friend <- newInvite store "  Émilie — 和菓子  " Friend GeneralTicket
  sameName <- newInvite store "Émilie — 和菓子" Friend GeneralTicket
  check "the same named referrer can own distinct reusable links"
    (invitationId friend /= invitationId sameName && invitationToken friend /= invitationToken sameName)
  check "invitation token carries 256 random bits and round-trips explicitly"
    (T.length (invitationTokenText (invitationToken friend)) == 67 &&
     parseInvitationToken (invitationTokenText (invitationToken friend)) == Just (invitationToken friend))
  general <- begin store [] "AAAA-2222" friend >>= holdRight
  check "friend admission fixes £65, one seat and frozen referrer attribution"
    (chTarget general == TransferableTicket && checkoutSlot general == Nothing && chPrice general == price GBP 65 && chSeats general == "1" &&
     maybe False (\source -> iaReferrer source == "Émilie — 和菓子" && iaReferralFee source == price GBP 0) (chInvitation general))
  check "general invitation checkout reserves no class capacity" . (== [0]) =<< occupied store slot
  check "unpaid attempts create neither tickets nor referral earnings" . null =<< listInvitationPurchases store
  _ <- begin store [] "BBBB-2222" friend >>= holdRight
  check "one invitation admits different buyers without being consumed" . ((== 2) . length) =<< listPendingCheckoutHolds store
  _ <- setInvitationEnabled store (invitationId friend) False >>= inviteRight
  disabled <- begin store [] "CCCC-2222" friend
  check "disabling blocks new checkout promises" (disabled == Left (HoldInvitationError InvitationDisabled))
  resumed <- begin store [] "AAAA-2222" friend >>= holdRight
  check "disabling preserves the existing purchase promise" (resumed == general)
  wrongLink <- begin store [] "AAAA-2222" sameName
  check "attempt identity cannot be moved to another invitation" (wrongLink == Left HoldConflict)
  wrongAmount <- confirmCheckoutHold store "AAAA-2222" "pi_friend" (price GBP 64)
  wrongCurrency <- confirmCheckoutHold store "AAAA-2222" "pi_friend" (price USD 65)
  check "a succeeded payment must have the exact promised amount and currency"
    (wrongAmount == Left HoldPaymentMismatch && wrongCurrency == Left HoldPaymentMismatch)
  bypass <- issueTicket store "pi_wrong_path" "AAAA-2222" "Gift" "1"
  check "gift issuance cannot bypass an invitation purchase promise" (isLeft bypass)
  (ticket, created) <- confirmCheckoutHold store "AAAA-2222" "pi_friend" (price GBP 65) >>= holdRight
  check "a paid general invitation issues an ordinary unredeemed single-person ticket"
    (created && tkSeats ticket == "1" && tkRedemption ticket == Nothing)
  check "no reservation email is queued until a general ticket is redeemed" . (== 0) . counts =<< readReservationEmails store
  redeemed <- redeemTicket store [slot] 3 (tkCode ticket) "A friend of a friend" (slotIdOf slot)
  check "a friend-of-friend bearer can redeem the general ticket normally" (isRight redeemed)
  check "general-ticket redemption queues the ordinary reservation email once" . (== 1) . counts =<< readReservationEmails store
  replay <- confirmCheckoutHold store "AAAA-2222" "pi_friend" (price GBP 65)
  check "payment replay after later redemption keeps the same redeemed ticket"
    (case replay of Right (existing, False) -> tkRedemption existing /= Nothing; _ -> False)
  reusedPayment <- confirmCheckoutHold store "BBBB-2222" "pi_friend" (price GBP 65)
  check "one paid reference cannot fund a second use of the invitation" (reusedPayment == Left HoldConflict)
  early <- confirmCheckoutHold store "BBBB-2222" "pi_friend_second" (price GBP 65) >>= holdRight
  _ <- attachCheckoutSession store "BBBB-2222" "cs_friend_second" "https://checkout.stripe.com/test/second" >>= holdRight
  check "webhook-before-session-attachment confirms a general purchase once" (snd early)
  reloaded <- open "main"
  restoredInvite <- lookupInvitation reloaded (invitationToken friend)
  check "disabled invitation and full Unicode referrer persist across restart"
    (maybe False (\i -> not (invitationEnabled i) && invitationReferrer i == invitationReferrer friend) restoredInvite)
  reports <- listInvitationPurchases reloaded
  check "reusable friend link reports two paid purchases and no partner fee"
    (length reports == 2 && totalPaid reports == 13000 && totalFees reports == 0 && all (not . T.null . ipConfirmedAt) reports)
  check "reported confirmation time is stable across replay and restart"
    (any (\purchase -> tkCode (ipTicket purchase) == tkCode ticket && ipConfirmedAt purchase == tkCreatedAt ticket) reports)

  partner <- newInvite reloaded "Partner venue" Partner (EventTicket next)
  held <- begin reloaded [next] "DDDD-2222" partner >>= holdRight
  check "event invitation holds one place at £75 with a £5 fee snapshot"
    (checkoutSlot held == Just next && chPrice held == price GBP 75 &&
     maybe False ((== price GBP 5) . iaReferralFee) (chInvitation held))
  blocked <- beginCheckoutHold reloaded [next] 3 "EEEE-2222" (slotIdOf next) "price_ordinary" "Ordinary" "1" (price GBP 80) expiry
  check "invitation holds share the last-place decision with ordinary buyers" (blocked == Left HoldSlotFull)
  _ <- setInvitationEnabled reloaded (invitationId partner) False >>= inviteRight
  retried <- begin reloaded [] "DDDD-2222" partner >>= holdRight
  check "an admitted event checkout survives removal from authored content" (retried == held)
  (leftPaid, rightPaid) <- concurrently
    (confirmCheckoutHold reloaded "DDDD-2222" "pi_partner" (price GBP 75))
    (confirmCheckoutHold reloaded "DDDD-2222" "pi_partner" (price GBP 75))
  check "concurrent duplicate paid events create one ticket and one attribution"
    (length [() | Right (_, True) <- [leftPaid, rightPaid]] == 1 && length [() | Right _ <- [leftPaid, rightPaid]] == 2)
  check "event payment automatically redeems and queues one reservation message" . (== 2) . counts =<< readReservationEmails reloaded
  reportsWithPartner <- listInvitationPurchases reloaded
  check "partner reporting accrues £5 only once after confirmed payment"
    (length reportsWithPartner == 3 && totalPaid reportsWithPartner == 20500 && totalFees reportsWithPartner == 500)
  restored <- open "main"
  storedPartner <- lookupInvitation restored (invitationToken partner)
  check "event artwork retains its original event snapshot after disable/reload"
    (maybe False ((== EventTicket next) . invitationScope) storedPartner)

  cancelledStore <- open "cancelled"
  cancelledInvite <- newInvite cancelledStore "Venue" Partner GeneralTicket
  _ <- begin cancelledStore [] "FFFF-2222" cancelledInvite >>= holdRight
  _ <- releaseCheckoutHold cancelledStore "FFFF-2222" >>= holdRight
  check "released unpaid checkout accrues no gross revenue or referral fee" . null =<< listInvitationPurchases cancelledStore
  late <- confirmCheckoutHold cancelledStore "FFFF-2222" "pi_after_expiry" (price GBP 75)
  check "terminal release cannot be resurrected by a later payment claim" (late == Left HoldAlreadyReleased)

  raceStore <- open "race"
  raceInvite <- newInvite raceStore "Venue" Partner (EventTicket slot)
  _ <- issueTicket raceStore "pi_gift" "GGGG-2222" "Existing gift" "1"
  (invitationRace, giftRace) <- concurrently
    (begin raceStore [slot] "HHHH-2222" raceInvite)
    (redeemTicket raceStore [slot] 3 "GGGG-2222" "Gift bearer" (slotIdOf slot))
  check "gift redemption and invitation booking cannot both take the last seat" (isRight invitationRace /= isRight giftRace)
  check "concurrent gift/invitation booking never oversells" . (== [1]) =<< occupied raceStore slot
  revokeStore <- open "revocation"
  revokeInvite <- newInvite revokeStore "Friend" Friend GeneralTicket
  (admission, revoked) <- concurrently (begin revokeStore [] "JJJJ-2222" revokeInvite)
    (setInvitationEnabled revokeStore (invitationId revokeInvite) False)
  check "disable racing admission yields a complete authorised promise or a disabled rejection"
    (isRight revoked && (isRight admission || admission == Left (HoldInvitationError InvitationDisabled)))
  afterRevoke <- begin revokeStore [] "KKKK-2222" revokeInvite
  check "after disable completes every new attempt is refused" (afterRevoke == Left (HoldInvitationError InvitationDisabled))

  -- Existing ticket and dated-hold objects must survive registry additions.
  legacyStore <- open "legacy"
  _ <- issueTicket legacyStore "pi_old_gift" "MMMM-2222" "Purchased gift" "2"
  _ <- beginCheckoutHold legacyStore [slot] 3 "NNNN-2222" (slotIdOf slot) "price_old" "Purchased class" "1" (price GBP 80) expiry >>= holdRight
  _ <- confirmCheckoutHold legacyStore "NNNN-2222" "pi_old_dated" (price GBP 80) >>= holdRight
  before <- readJSON (dir </> "legacy.json")
  _ <- newInvite legacyStore "New friend" Friend GeneralTicket
  after <- readJSON (dir </> "legacy.json")
  check "adding invitation data preserves existing paid ticket objects exactly" (before .:? "tickets" == after .:? "tickets")
  check "adding invitations preserves the previous dated-hold wire format exactly" (before .:? "checkoutHolds" == after .:? "checkoutHolds")
  _ <- open "legacy"

  -- Permanent deletion is confined to disabled links with no checkout history.
  unused <- newInvite legacyStore "Unused draft" Friend GeneralTicket
  beforeActiveDelete <- BS.readFile (dir </> "legacy.json")
  activeDelete <- deleteInvitation legacyStore (invitationId unused)
  check "an enabled invitation cannot be permanently deleted" (activeDelete == Left InvitationActive)
  check "refused active deletion leaves storage unchanged" . (== beforeActiveDelete) =<< BS.readFile (dir </> "legacy.json")
  _ <- setInvitationEnabled legacyStore (invitationId unused) False >>= inviteRight
  beforeDelete <- readJSON (dir </> "legacy.json")
  removed <- deleteInvitation legacyStore (invitationId unused)
  check "a disabled invitation with no history can be permanently deleted" (removed == Right ())
  afterDelete <- readJSON (dir </> "legacy.json")
  let removeUnused (JArr entries) = JArr [entry | entry <- entries, entry .:? "id" /= Just (JStr (T.unpack (invitationIdText (invitationId unused))))]
      removeUnused value = value
  check "deletion preserves every other invitation, ticket, hold and email record exactly"
    (afterDelete == changeField "invitations" removeUnused beforeDelete)
  deletedReload <- open "legacy"
  check "deleted secret link stays absent after reload" . (== Nothing) =<< lookupInvitation deletedReload (invitationToken unused)
  repeatedDelete <- deleteInvitation deletedReload (invitationId unused)
  check "a repeated delete reports that the invitation is absent" (repeatedDelete == Left InvitationNotFound)

  historyStore <- open "deletion-history"
  mapM_ (\(code, wantedState) -> do
    invitation <- newInvite historyStore "History owner" Partner GeneralTicket
    _ <- begin historyStore [] code invitation >>= holdRight
    case wantedState of
      HoldCreating -> pure ()
      HoldCheckout sid url -> do _ <- attachCheckoutSession historyStore code sid url >>= holdRight; pure ()
      HoldConfirmed ref -> do _ <- confirmCheckoutHold historyStore code ref (price GBP 75) >>= holdRight; pure ()
      HoldReleased -> do _ <- releaseCheckoutHold historyStore code >>= holdRight; pure ()
    _ <- setInvitationEnabled historyStore (invitationId invitation) False >>= inviteRight
    expectedHold <- lookupCheckoutHold historyStore code
    saved <- BS.readFile (dir </> "deletion-history.json")
    refused <- deleteInvitation historyStore (invitationId invitation)
    check "every checkout state protects its invitation from deletion" (refused == Left InvitationHasHistory)
    check "refused history deletion writes nothing" . (== saved) =<< BS.readFile (dir </> "deletion-history.json")
    reloadedHistory <- open "deletion-history"
    check "refused deletion preserves the exact hold after reload" . (== expectedHold) =<< lookupCheckoutHold reloadedHistory code)
    [("QQQQ-2222", HoldCreating), ("RRRR-2222", HoldCheckout "cs_delete_open" "https://checkout.stripe.com/test/delete"),
     ("SSSS-2222", HoldReleased), ("TTTT-2222", HoldConfirmed "pi_delete_paid")]

  deletionRaceStore <- open "deletion-race"
  deletionRaceInvite <- newInvite deletionRaceStore "Unused link" Friend GeneralTicket
  _ <- setInvitationEnabled deletionRaceStore (invitationId deletionRaceInvite) False >>= inviteRight
  (racedDelete, racedEnable) <- concurrently
    (deleteInvitation deletionRaceStore (invitationId deletionRaceInvite))
    (setInvitationEnabled deletionRaceStore (invitationId deletionRaceInvite) True)
  check "delete and re-enable are one atomic choice"
    ((racedDelete == Right () && racedEnable == Left InvitationNotFound) ||
     (racedDelete == Left InvitationActive && isRight racedEnable))

  -- A failed durable write never publishes a new invitation or a paid record.
  broken <- newBookingStore (dir </> "missing" </> "store.json")
  chosen <- draft "Friend" Friend GeneralTicket
  failedCreate <- try (createInvitation broken chosen) :: IO (Either IOException (Either InvitationError Invitation))
  check "failed invitation persistence does not publish a usable link" (isLeft failedCreate)
  check "failed invitation write leaves registry empty" . null =<< listInvitations broken
  let durableDir = dir </> "durable"
      movedDir = dir </> "moved"
  createDirectory durableDir
  durable <- newBookingStore (durableDir </> "bookings.json")
  durableInvite <- newInvite durable "Venue" Partner GeneralTicket
  _ <- begin durable [] "PPPP-2222" durableInvite >>= holdRight
  renameDirectory durableDir movedDir
  failedDisable <- try (setInvitationEnabled durable (invitationId durableInvite) False) :: IO (Either IOException (Either InvitationError Invitation))
  stillEnabled <- lookupInvitation durable (invitationToken durableInvite)
  check "failed disable persistence cannot publish an unrecorded revocation"
    (isLeft failedDisable && maybe False invitationEnabled stillEnabled)
  failedPaid <- try (confirmCheckoutHold durable "PPPP-2222" "pi_disk_retry" (price GBP 75)) :: IO (Either IOException (Either HoldError (Ticket, Bool)))
  check "failed paid commit cannot accrue referral fees" (isLeft failedPaid)
  check "failed paid commit leaves purchase reporting empty" . null =<< listInvitationPurchases durable
  renameDirectory movedDir durableDir
  _ <- confirmCheckoutHold durable "PPPP-2222" "pi_disk_retry" (price GBP 75) >>= holdRight
  check "repaired retry records one successful purchase" . ((== 1) . length) =<< listInvitationPurchases durable
  durableUnused <- newInvite durable "Unused link" Friend GeneralTicket
  disabledUnused <- setInvitationEnabled durable (invitationId durableUnused) False >>= inviteRight
  renameDirectory durableDir movedDir
  failedDelete <- try (deleteInvitation durable (invitationId durableUnused)) :: IO (Either IOException (Either InvitationError ()))
  retainedUnused <- lookupInvitation durable (invitationToken durableUnused)
  check "failed deletion persistence retains the disabled invitation in memory"
    (isLeft failedDelete && retainedUnused == Just disabledUnused)
  renameDirectory movedDir durableDir
  restoredDurable <- newBookingStore (durableDir </> "bookings.json")
  check "failed deletion also preserves the invitation on disk" . (== Just disabledUnused) =<< lookupInvitation restoredDurable (invitationToken durableUnused)
  retriedDelete <- deleteInvitation durable (invitationId durableUnused)
  check "repaired deletion can be retried successfully" (retriedDelete == Right ())

  -- Persisted financial or ownership corruption must fail startup.
  mainJSON <- readJSON (dir </> "main.json")
  let corruptFee = changeField "invitations" (\v -> case v of JArr (entry : rest) -> JArr (changeField "referralFee" (const (JNum 999)) entry : rest); _ -> v) mainJSON
      missingRegistry = changeField "invitations" (const (JArr [])) mainJSON
  mapM_ (\(name, value) -> do
    BS.writeFile (dir </> name <> ".json") (TE.encodeUtf8 (T.pack (encode value)))
    loaded <- try (open name) :: IO (Either IOException BookingStore)
    check "corrupt invitation terms or missing attribution owner fail closed at startup" (isLeft loaded))
    [("bad-fee", corruptFee), ("missing-owner", missingRegistry)]
  putStrLn "Invitation reuse, deletion, pricing, attribution, concurrency, persistence and legacy compatibility checks passed."

readJSON :: FilePath -> IO JSON
readJSON path = do
  bytes <- BS.readFile path
  either fail pure (decode (T.unpack (TE.decodeUtf8 bytes)))

changeField :: String -> (JSON -> JSON) -> JSON -> JSON
changeField key change (JObj fields) = JObj [(name, if name == key then change value else value) | (name, value) <- fields]
changeField _ _ value = value

fixture :: IO FilePath
fixture = do
  temp <- getTemporaryDirectory
  (path, handle) <- openTempFile temp "sww-invitations"
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
  first <- takeMVar a >>= either throwIO pure
  second <- takeMVar b >>= either throwIO pure
  pure (first, second)
  where
    capture :: IO c -> IO (Either SomeException c)
    capture = try
