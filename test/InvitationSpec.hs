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
      begin store slots code invitation = beginInvitationCheckout store slots 3 code (invitationToken invitation)
        (case invitationScope invitation of GeneralTicket -> Nothing; EventTicket chosen -> Just (slotIdOf chosen)) "price_invitation" "One-person class ticket" expiry
      choose store slots weekly code invitation selected = beginInvitationCheckout store slots weekly code (invitationToken invitation)
        (slotIdOf <$> selected) "price_invitation" "One-person class ticket" expiry
      offers store slots weekly invitation = fmap (fmap (map slotIdOf)) (availableInvitationSlots store slots weekly (invitationToken invitation))
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

  -- Only a full original event may offer explicit alternatives. The condition
  -- and chosen date are rechecked atomically; stale choices never move silently.
  alternativeStore <- open "alternatives"
  let earlier = slot {slotDate = "2030-01-06"}
      second = slot {slotDate = "2030-01-08"}
      third = slot {slotDate = "2030-01-09"}
      fourth = slot {slotDate = "2030-01-10"}
      fifth = slot {slotDate = "2030-01-11"}
      offered = [fifth, third, earlier, slot, fourth, second]
  alternativeInvite <- newInvite alternativeStore "Original venue" Partner (EventTicket slot)
  check "an event with space offers only its original date" .
    (== Right [slotIdOf slot]) =<< offers alternativeStore offered 0 alternativeInvite
  check "repeated schedule sections cannot duplicate offered dates" .
    (== Right [slotIdOf slot]) =<< offers alternativeStore (second : slot : second : offered) 0 alternativeInvite
  prematureAlternative <- choose alternativeStore offered 0 "AAAA-2222" alternativeInvite (Just second)
  arbitrary <- choose alternativeStore offered 0 "AAAA-2222" alternativeInvite (Just fifth)
  backwards <- choose alternativeStore offered 0 "AAAA-2222" alternativeInvite (Just earlier)
  missingChoice <- choose alternativeStore offered 0 "AAAA-2222" alternativeInvite Nothing
  check "alternatives cannot be forced while the original has room"
    (prematureAlternative == Left HoldConflict && arbitrary == Left HoldConflict && backwards == Left HoldConflict && missingChoice == Left HoldConflict)
  _ <- beginCheckoutHold alternativeStore offered 0 "BBBB-2222" (slotIdOf slot) "price_full" "Original pending buyer" "1" (price GBP 80) expiry >>= holdRight
  let stillHasRoom = [if slotIdOf candidate == slotIdOf slot then candidate {slotCap = 3} else candidate | candidate <- offered]
  check "fullness uses current capacity, not the invitation's saved capacity" .
    (== Right [slotIdOf slot]) =<< offers alternativeStore stillHasRoom 0 alternativeInvite
  notActuallyFull <- choose alternativeStore stillHasRoom 0 "AAAA-2222" alternativeInvite (Just second)
  check "partly occupied originals do not permit an alternative-date POST" (notActuallyFull == Left HoldConflict)
  check "a full original including pending holds unlocks exactly three later dates" .
    (== Right (map slotIdOf [second, third, fourth])) =<< offers alternativeStore offered 0 alternativeInvite
  check "full-event alternatives are three distinct dates despite repeated sections" .
    (== Right (map slotIdOf [second, third, fourth])) =<< offers alternativeStore (second : slot : second : offered) 0 alternativeInvite
  check "even a full original offers no alternatives after it is removed or past cutoff" .
    (== Right []) =<< offers alternativeStore [second, third, fourth, fifth] 0 alternativeInvite
  tooLate <- choose alternativeStore offered 0 "AAAA-2222" alternativeInvite (Just fifth)
  check "a forged date outside the three full-event alternatives is refused" (tooLate == Left HoldConflict)
  staleOriginal <- choose alternativeStore offered 0 "AAAA-2222" alternativeInvite (Just slot)
  check "a stale original-date button is refused without silently booking an alternative" (staleOriginal == Left HoldConflict)
  (choiceA, choiceB) <- concurrently
    (choose alternativeStore offered 0 "CCCC-2222" alternativeInvite (Just second))
    (choose alternativeStore offered 0 "DDDD-2222" alternativeInvite (Just second))
  check "two simultaneous alternative-date buyers cannot both obtain its last seat"
    (length [h | Right h <- [choiceA, choiceB]] == 1 && length [() | Left HoldConflict <- [choiceA, choiceB]] == 1)
  winner <- case [h | Right h <- [choiceA, choiceB]] of [h] -> pure h; _ -> fail "Expected one alternative-date winner."
  check "alternative checkout keeps the original tier, fee and attribution"
    (checkoutSlot winner == Just second && chPrice winner == price GBP 75 && chInvitation winner == Just (attributionFor alternativeInvite))
  check "alternative last-seat race holds exactly one place" . (== [1]) =<< occupied alternativeStore second
  check "filled alternatives are replaced by the next chronological choice" .
    (== Right (map slotIdOf [third, fourth, fifth])) =<< offers alternativeStore offered 0 alternativeInvite
  _ <- releaseCheckoutHold alternativeStore "BBBB-2222" >>= holdRight
  check "reopening the original hides all alternative dates" .
    (== Right [slotIdOf slot]) =<< offers alternativeStore offered 0 alternativeInvite
  reopenedStale <- choose alternativeStore offered 0 "AAAA-2222" alternativeInvite (Just third)
  check "a stale alternative is refused once the original reopens" (reopenedStale == Left HoldConflict)
  resumedChoice <- choose alternativeStore [] 0 (chCode winner) alternativeInvite (Just second) >>= holdRight
  check "existing alternative checkout resumes its promised date after original reopens or content disappears" (resumedChoice == winner)
  changedChoice <- choose alternativeStore offered 0 (chCode winner) alternativeInvite (Just third)
  check "an existing checkout cannot change its selected date" (changedChoice == Left HoldConflict)
  _ <- setInvitationEnabled alternativeStore (invitationId alternativeInvite) False >>= inviteRight
  afterDisable <- choose alternativeStore [] 0 (chCode winner) alternativeInvite (Just second) >>= holdRight
  check "disable preserves the exact admitted alternative-date promise" (afterDisable == winner)
  (alternativeTicket, _) <- confirmCheckoutHold alternativeStore (chCode winner) "pi_alternative" (price GBP 75) >>= holdRight
  check "paid alternative redeems the chosen date rather than the invitation's original date"
    (maybe False ((== slotIdOf second) . redSlotId) (tkRedemption alternativeTicket))
  alternativeReload <- open "alternatives"
  restoredAlternative <- lookupCheckoutHold alternativeReload (chCode winner)
  check "alternative target and original attribution survive a real store reload"
    (maybe False (\h -> checkoutSlot h == Just second && chInvitation h == Just (attributionFor alternativeInvite)) restoredAlternative)
  alternativeReports <- listInvitationPurchases alternativeReload
  check "alternative purchase reports the same partner price and one £5 fee against original invitation"
    (case alternativeReports of [purchase] -> ipPaidPrice purchase == price GBP 75 && iaReferralFee (ipAttribution purchase) == price GBP 5 && ipAttribution purchase == attributionFor alternativeInvite; _ -> False)

  generalDates <- open "general-dates"
  generalInvite <- newInvite generalDates "Friend" Friend GeneralTicket
  check "general invitations never offer dated choices" .
    (== Right []) =<< offers generalDates offered 0 generalInvite
  forbiddenGeneralDate <- choose generalDates offered 0 "EEEE-2222" generalInvite (Just second)
  check "a forged general-invitation date cannot start checkout" (forbiddenGeneralDate == Left HoldConflict)
  generalTransferable <- choose generalDates [] 0 "FFFF-2222" generalInvite Nothing >>= holdRight
  check "general invitations still admit a transferable ticket at their original price"
    (chTarget generalTransferable == TransferableTicket && chPrice generalTransferable == price GBP 65)
  -- Historical V2 general-origin dated holds were admitted by the previous
  -- release. Load that persisted shape directly; new admission now forbids it.
  generalJSON <- readJSON (dir </> "general-dates.json")
  let oldGeneralDate (JArr [JObj fields]) = JArr
        [JObj fields, JObj (("slot", JObj
          [("date", JStr (T.unpack (slotDate second))), ("start", JStr (T.unpack (slotStart second))),
           ("end", JStr (T.unpack (slotEnd second))), ("area", JStr (T.unpack (slotArea second))),
           ("capacity", JNum (fromIntegral (slotCap second))), ("description", JStr (T.unpack (slotDescription second)))]) :
          [(key, if key == "code" then JStr "EEEE-2222" else value) | (key, value) <- fields, key /= "target"])]
      oldGeneralDate value = value
  BS.writeFile (dir </> "general-dates.json") (TE.encodeUtf8 (T.pack (encode (changeField "checkoutHolds" oldGeneralDate generalJSON))))
  generalReload <- open "general-dates"
  historicalHold <- lookupCheckoutHold generalReload "EEEE-2222" >>= maybe (fail "Historical general dated hold missing") pure
  resumedHistorical <- choose generalReload [] 0 "EEEE-2222" generalInvite (Just second) >>= holdRight
  check "historical general-origin dated holds remain readable and resumable"
    (resumedHistorical == historicalHold && chRequestVersion historicalHold == CheckoutRequestV2 &&
     checkoutSlot historicalHold == Just second && chInvitation historicalHold == Just (attributionFor generalInvite))
  (historicalTicket, _) <- confirmCheckoutHold generalReload "EEEE-2222" "pi_general_dated" (price GBP 65) >>= holdRight
  check "the historical general dated promise is still fulfilled on its saved date"
    (maybe False ((== slotIdOf second) . redSlotId) (tkRedemption historicalTicket))
  historicalPaidReload <- open "general-dates"
  check "historical general dated purchases remain valid after payment and reload" . ((== 1) . length) =<< listInvitationPurchases historicalPaidReload
  check "no available dates is a valid general invitation with transferable checkout intact" . (== Right []) =<< offers generalDates [] 0 generalInvite

  absentDates <- open "absent-dates"
  absentInvite <- newInvite absentDates "Friend" Friend (EventTicket slot)
  check "past, cutoff or removed originals do not unlock alternatives" .
    (== Right []) =<< offers absentDates [earlier, fifth, third, fourth, second] 0 absentInvite
  absentAttempt <- choose absentDates [second, third] 0 "AAAA-2222" absentInvite (Just second)
  check "a missing original cannot be bypassed by posting a later date" (absentAttempt == Left HoldConflict)
  check "a zero-capacity original is unavailable rather than full" .
    (== Right []) =<< offers absentDates [slot {slotCap = 0}, second, third] 0 absentInvite
  check "an event with no eligible original or alternatives offers no date" . (== Right []) =<< offers absentDates [] 0 absentInvite
  weeklyDates <- open "weekly-dates"
  weeklyInvite <- newInvite weeklyDates "Friend" Friend (EventTicket second)
  let nextWeek = slot {slotDate = "2030-01-14"}
      weeklySlots = [slot, second, third, nextWeek]
  _ <- beginCheckoutHold weeklyDates weeklySlots 1 "GGGG-2222" (slotIdOf slot) "price_weekly" "Weekly commitment" "1" (price GBP 80) expiry >>= holdRight
  check "a weekly-blocked original does not unlock alternatives" .
    (== Right []) =<< offers weeklyDates weeklySlots 1 weeklyInvite
  fullWeeklyInvite <- newInvite weeklyDates "Full event" Friend (EventTicket slot)
  check "full-event alternatives still obey the weekly class limit" .
    (== Right [slotIdOf nextWeek]) =<< offers weeklyDates weeklySlots 1 fullWeeklyInvite
  earlierGiftDates <- open "earlier-gift-dates"
  earlierGiftInvite <- newInvite earlierGiftDates "Friend" Friend (EventTicket second)
  _ <- issueTicket earlierGiftDates "pi_previous_gift" "AAAA-2222" "Old gift" "1"
  _ <- redeemTicket earlierGiftDates [slot] 1 "AAAA-2222" "Gift guest" (slotIdOf slot)
  check "past gift commitments still block the original without unlocking alternatives" .
    (== Right []) =<< offers earlierGiftDates [second, third, nextWeek] 1 earlierGiftInvite

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
