{-# LANGUAGE OverloadedStrings #-}

-- | Booking notifications: the ticket email, the reservation confirmation +
-- Google Calendar invitation, and the two reminder nags (a week before, the
-- day of). One module owns the copy and the side-effect discipline; the HTTP
-- handlers and the reminder loop both call it.
--
-- Side effects are retried, not fire-and-forget: each is attempted, and only
-- a SUCCESS is recorded on the ticket (the event id / the notes ledger). The
-- reminder loop re-runs anything still unrecorded every minute, so a Google
-- outage at booking time heals by itself. The crash window between a send
-- completing and the mark persisting can at worst repeat one email — chosen
-- over the opposite failure (marking first and losing the send forever).
module Swwstructor.Server.Notify
  ( BookingCtx (..)
  , siteSlots
  , notifyTicketIssued
  , ensureReservationSideEffects
  , sendDueReminders
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager)
import Swwstructor.Block (fmtSlotDay)
import Swwstructor.Content
  ( SectionSpec (ScheduleClass)
  , SiteSpec
  , SlotSpec (..)
  , allSectionsOf
  , slotIdOf
  )
import Swwstructor.Server.Booking
  ( BookingStore
  , Redemption (..)
  , Ticket (..)
  , claimNote
  , listTickets
  , setEventId
  )
import Swwstructor.Server.Google
  ( CalendarEvent (..)
  , GoogleCreds
  , Mail (Mail)
  , createCalendarEvent
  , patchEventLocation
  , sendMail
  )
import Swwstructor.Server.Venues (Venue (vnAddress, vnMapUrl, vnName), venueForArea)

-- | Everything the notification side needs — deliberately NOT 'AppEnv' (this
-- module is shared with the reminder loop and must not import the app).
data BookingCtx = BookingCtx
  { bcManager :: !Manager
  , bcStore :: !BookingStore
  , bcCreds :: !(IO (Maybe GoogleCreds))
  -- ^ loaded per use — the owner can paste keys into the admin at runtime
  , bcStaff :: ![Text]
  -- ^ school-side addresses invited to every event and copied on every mail
  , bcReplyTo :: !(Maybe Text)
  -- ^ Reply-To for outgoing mail (e.g. the school's public address)
  , bcSiteName :: !Text
  -- ^ short brand name for mail copy (\"Okashi School\")
  , bcBaseUrl :: !Text
  , bcTimeZone :: !Text
  -- ^ IANA zone the classes run in, e.g. @Europe\/London@
  , bcTzOffset :: !Text
  -- ^ that zone's UTC offset for the class dates, e.g. @+01:00@ (Haskell has
  -- no tz database; one static offset is exact for a same-DST booking window)
  , bcSlots :: !(IO [SlotSpec])
  -- ^ the live slot definitions (site content may be edited at runtime)
  , bcVenues :: !(IO [Venue])
  -- ^ the venue address book (admin-managed, server-side only)
  , bcVenueNoun :: !Text
  -- ^ what this site calls a venue in student-facing copy — @\"kitchen\"@
  -- for a pastry school, @\"studio\"@, @\"hall\"@… (default @\"venue\"@)
  }

-- | Every slot defined by any @scheduleClass@ section of the site.
siteSlots :: SiteSpec -> [SlotSpec]
siteSlots site = concat [ss | ScheduleClass _ _ ss <- allSectionsOf site]

-- ---------------------------------------------------------------------------
-- The notifications
-- ---------------------------------------------------------------------------

-- | The \"here is your ticket\" mail, sent once per ticket (idempotent via
-- the @ticket@ ledger note). Runs from the success page, the webhook, or the
-- retry loop — whoever gets there first.
notifyTicketIssued :: BookingCtx -> Ticket -> IO ()
notifyTicketIssued ctx t
  | "ticket" `elem` tkNotesSent t = pure ()
  | otherwise = do
      sent <-
        trySend ctx (tkEmail t) $
          ( "Your " <> bcSiteName ctx <> " class ticket - " <> tkCode t
          , T.intercalate
              "\n"
              [ "Hello,"
              , ""
              , "Thank you for booking \"" <> tkProduct t <> "\" with " <> bcSiteName ctx <> "!"
              , ""
              , "Your ticket code is:"
              , ""
              , "    " <> tkCode t
              , ""
              , "To pick the date of your class, go to"
              , ""
              , "    " <> bcBaseUrl ctx <> "/classes"
              , ""
              , "choose a time that suits you, and enter this code together with the"
              , "email address you used at checkout (this one)."
              , ""
              , "Classes run in small groups. The exact " <> bcVenueNoun ctx <> " address is emailed"
              , "about a week before each class - the booking page shows the area."
              , ""
              , "See you soon,"
              , bcSiteName ctx
              ]
          )
      markIf ctx sent (tkCode t) "ticket"

-- | Make a redeemed ticket's side effects true: the calendar event exists and
-- the confirmation mail went out. Fully idempotent — safe to call from the
-- redeem handler and again from every reminder tick until both are recorded.
ensureReservationSideEffects :: BookingCtx -> Ticket -> IO ()
ensureReservationSideEffects ctx t = case tkRedemption t of
  Nothing -> pure ()
  Just r -> do
    slots <- bcSlots ctx
    case [s | s <- slots, slotIdOf s == redSlotId r] of
      [] -> pure () -- slot definition edited away; nothing sensible to do
      (s : _) -> do
        ensureEvent ctx t r s
        ensureConfirmMail ctx t r s

ensureEvent :: BookingCtx -> Ticket -> Redemption -> SlotSpec -> IO ()
ensureEvent ctx t r s = case redEventId r of
  Just _ -> pure ()
  Nothing -> do
    mcreds <- bcCreds ctx
    case mcreds of
      Nothing -> pure ()
      Just creds -> do
        let ev =
              CalendarEvent
                { ceSummary = tkProduct t <> " - " <> bcSiteName ctx
                , ceDescription =
                    T.intercalate
                      "\n"
                      [ "Student: " <> redName r <> " <" <> tkEmail t <> ">"
                      , "Ticket:  " <> tkCode t
                      , ""
                      , "The exact " <> bcVenueNoun ctx <> " address is emailed about a week before class."
                      ]
                , ceLocation = slotArea s <> " (exact address announced a week ahead)"
                , ceStart = slotDate s <> "T" <> slotStart s <> ":00" <> bcTzOffset ctx
                , ceEnd = slotDate s <> "T" <> slotEnd s <> ":00" <> bcTzOffset ctx
                , ceTimeZone = bcTimeZone ctx
                , ceAttendees = tkEmail t : bcStaff ctx
                }
        res <- createCalendarEvent (bcManager ctx) creds ev
        case res of
          Right evId -> setEventId (bcStore ctx) (tkCode t) evId
          Left err -> putStrLn ("[booking] calendar event failed (will retry): " <> T.unpack err)

ensureConfirmMail :: BookingCtx -> Ticket -> Redemption -> SlotSpec -> IO ()
ensureConfirmMail ctx t r s
  | "confirm" `elem` tkNotesSent t = pure ()
  | otherwise = do
      sent <-
        trySend ctx (tkEmail t) $
          ( "You're booked: " <> tkProduct t <> ", " <> fmtSlotDay (slotDate s) <> " " <> slotStart s
          , T.intercalate
              "\n"
              [ "Hello " <> redName r <> ","
              , ""
              , "Your class is confirmed:"
              , ""
              , "    What:   " <> tkProduct t
              , "    When:   " <> fmtSlotDay (slotDate s) <> ", " <> slotStart s <> "-" <> slotEnd s
              , "    Where:  " <> slotArea s
              , "            (the exact " <> bcVenueNoun ctx <> " address is emailed about a week before class)"
              , "    Ticket: " <> tkCode t
              , ""
              , "A Google Calendar invitation is on its way to this address too."
              , ""
              , "Everything is provided - ingredients, tools and printed recipes - and"
              , "you'll take the sweets you make home, so a small box or bag helps."
              , ""
              , "See you soon,"
              , bcSiteName ctx
              ]
          )
      markIf ctx sent (tkCode t) "confirm"

-- | The dated nags. @today@ is the class-timezone civil date (@YYYY-MM-DD@)
-- and @hourOfDay@ its wall-clock hour; the ledger makes re-ticking free.
sendDueReminders :: BookingCtx -> Text -> Int -> IO ()
sendDueReminders ctx today hourOfDay = do
  tickets <- listTickets (bcStore ctx)
  slots <- bcSlots ctx
  venues <- bcVenues ctx
  mapM_ (remindOne ctx slots venues today hourOfDay) tickets

remindOne :: BookingCtx -> [SlotSpec] -> [Venue] -> Text -> Int -> Ticket -> IO ()
remindOne ctx slots venues today hourOfDay t = case tkRedemption t of
  Nothing -> pure ()
  Just r -> case [s | s <- slots, slotIdOf s == redSlotId r] of
    [] -> pure ()
    (s : _) -> do
      let due note = note `notElem` tkNotesSent t
          noun = bcVenueNoun ctx
          mvenue = venueForArea venues (slotArea s)
          whereLines lead = case mvenue of
            Just v ->
              [ lead <> slotArea s
              , pad lead <> vnName v <> ", " <> vnAddress v
              ]
                <> [pad lead <> vnMapUrl v | not (T.null (vnMapUrl v))]
            Nothing -> [lead <> slotArea s]
          pad = (`T.replicate` " ") . T.length
      -- A week out (sent from 09:00 local). With a venue on file for the
      -- area this IS the announcement — the mail carries the address and the
      -- calendar event is updated; without one it stays the manual cue.
      case () of
        _
          | slotDate s == addDaysIso 7 today && hourOfDay >= 9 && due "t7" -> do
              sent <-
                trySend ctx (tkEmail t) $
                  ( "One week to go: " <> tkProduct t <> " on " <> fmtSlotDay (slotDate s)
                  , T.intercalate
                      "\n"
                      ( [ "Hello " <> redName r <> ","
                        , ""
                        , "A quick reminder that your class is one week away:"
                        , ""
                        , "    What:  " <> tkProduct t
                        , "    When:  " <> fmtSlotDay (slotDate s) <> ", " <> slotStart s <> "-" <> slotEnd s
                        ]
                          <> whereLines "    Where: "
                          <> [""]
                          <> ( case mvenue of
                                 Just _ ->
                                   [ "That's the exact " <> noun <> " address - your calendar invitation has"
                                   , "been updated with it too."
                                   ]
                                 Nothing ->
                                   [ "The exact " <> noun <> " address is announced by email around now - if it"
                                   , "hasn't reached you within a day or two, just reply to this email."
                                   ]
                             )
                          <> [ ""
                             , "See you very soon,"
                             , bcSiteName ctx
                             ]
                      )
                  )
              markIf ctx sent (tkCode t) "t7"
              -- The calendar catches up with the announcement (quiet patch;
              -- best-effort — the mail above already carries the address).
              case (sent, mvenue, redEventId r) of
                (True, Just v, Just evId) -> do
                  mcreds <- bcCreds ctx
                  case mcreds of
                    Nothing -> pure ()
                    Just creds -> do
                      res <-
                        patchEventLocation
                          (bcManager ctx)
                          creds
                          evId
                          (vnName v <> ", " <> vnAddress v)
                      case res of
                        Right () -> pure ()
                        Left err -> putStrLn ("[booking] event location patch failed: " <> T.unpack err)
                _ -> pure ()
          | slotDate s == today && hourOfDay >= 7 && due "day0" -> do
              sent <-
                trySend ctx (tkEmail t) $
                  ( "Today's class: " <> tkProduct t <> ", " <> slotStart s <> "-" <> slotEnd s
                  , T.intercalate
                      "\n"
                      ( [ "Hello " <> redName r <> ","
                        , ""
                        , "It's today!"
                        , ""
                        , "    What:  " <> tkProduct t
                        , "    When:  today, " <> slotStart s <> "-" <> slotEnd s
                        ]
                          <> ( case mvenue of
                                 Just v ->
                                   [ "    Where: " <> vnName v <> ", " <> vnAddress v ]
                                     <> ["           " <> vnMapUrl v | not (T.null (vnMapUrl v))]
                                 Nothing ->
                                   [ "    Where: " <> slotArea s <> " - the exact address was emailed to you this week" ]
                             )
                          <> [ ""
                             , "Everything is provided; bring a small box or bag for the sweets you"
                             , "make. If anything comes up, reply to this email."
                             , ""
                             , "See you in a few hours,"
                             , bcSiteName ctx
                             ]
                      )
                  )
              markIf ctx sent (tkCode t) "day0"
          | otherwise -> pure ()

-- ---------------------------------------------------------------------------
-- Plumbing
-- ---------------------------------------------------------------------------

-- | Send one mail to the student + the staff circle. 'False' (creds missing /
-- API error) leaves the ledger untouched so the loop retries.
trySend :: BookingCtx -> Text -> (Text, Text) -> IO Bool
trySend ctx student (subject, body) = do
  mcreds <- bcCreds ctx
  case mcreds of
    Nothing -> pure False
    Just creds -> do
      res <-
        sendMail
          (bcManager ctx)
          creds
          (Mail (student : bcStaff ctx) (bcReplyTo ctx) subject body)
      case res of
        Right () -> pure True
        Left err -> do
          putStrLn ("[booking] mail failed (will retry): " <> T.unpack err)
          pure False

markIf :: BookingCtx -> Bool -> Text -> Text -> IO ()
markIf ctx sent code note
  | sent = () <$ claimNote (bcStore ctx) code note
  | otherwise = pure ()

-- | @YYYY-MM-DD@ plus N days, in pure string space (Gregorian arithmetic).
addDaysIso :: Int -> Text -> Text
addDaysIso n iso = case mapM readIntT (T.splitOn "-" iso) of
  Just [y, m, d] -> fmt (go n y m d)
  _ -> iso
  where
    readIntT x = case reads (T.unpack x) of [(v :: Int, "")] -> Just v; _ -> Nothing
    go 0 y m d = (y, m, d)
    go k y m d
      | d < monthLen y m = go (k - 1) y m (d + 1)
      | m < 12 = go (k - 1) y (m + 1) 1
      | otherwise = go (k - 1) (y + 1) 1 1
    monthLen y m
      | m == 2 = if leap y then 29 else 28
      | m `elem` [4, 6, 9, 11] = 30
      | otherwise = 31
    leap y = (y `mod` 4 == 0 && y `mod` 100 /= 0) || y `mod` 400 == 0
    fmt (y, m, d) = T.pack (pad 4 y <> "-" <> pad 2 m <> "-" <> pad 2 d)
    pad w v = let s = show v in replicate (w - length s) '0' <> s
