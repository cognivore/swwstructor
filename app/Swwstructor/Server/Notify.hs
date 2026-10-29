{-# LANGUAGE OverloadedStrings #-}

-- | Booking side effects, all of them staff-facing or student-pulled: the
-- staff calendar event for a reservation, its location update, and
-- the calendar file a student downloads from their booking page.
--
-- The site holds no way to reach a student — no email, nothing personal; a
-- ticket is a reference code and a preferred name. So nothing is ever SENT
-- to a student: Stripe's receipt carries their code, and their booking page
-- (@\/booking\/<code>@) is where they find the details, the address and the
-- calendar file immediately.
--
-- Side effects are retried, not fire-and-forget: each is attempted, and only
-- a SUCCESS is recorded on the ticket (the event id / the ledger). The loop
-- re-runs anything still unrecorded every minute, so a Google outage at
-- booking time heals by itself.
module Swwstructor.Server.Notify
  ( BookingCtx (..)
  , siteSlots
  , siteMaxWeekly
  , ensureReservationSideEffects
  , announceVenues
  , bookingIcs
  , CalendarTicket (..)
  , CalendarStatus (..)
  , offsetSeconds
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.LocalTime (LocalTime, localTimeToUTC, minutesToTimeZone)
import Network.HTTP.Client (Manager)
import Swwstructor.Block (markdownPlain)
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
  , createCalendarEvent
  , patchEventLocation
  )
import Swwstructor.Server.Venues (Venue, venueForArea, venueLocation)

-- | Everything the side-effect code needs — deliberately NOT 'AppEnv' (this
-- module is shared with the loop and must not import the app).
data BookingCtx = BookingCtx
  { bcManager :: !Manager
  , bcStore :: !BookingStore
  , bcCreds :: !(IO (Maybe GoogleCreds))
  -- ^ loaded per use — the owner can paste keys into the admin at runtime
  , bcStaff :: ![Text]
  -- ^ school-side addresses invited to every class event
  , bcSiteName :: !Text
  -- ^ short brand name for event and calendar-file copy (\"Okashi School\")
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
siteSlots site = concat [ss | ScheduleClass _ _ _ ss <- allSectionsOf site]

-- | The site's per-week session cap: the strictest (largest) cap any
-- @scheduleClass@ section declares — it models the teacher's capacity, so
-- one value governs the whole site. 0 = no cap.
siteMaxWeekly :: SiteSpec -> Int
siteMaxWeekly site = maximum (0 : [mw | ScheduleClass _ _ mw _ <- allSectionsOf site])

-- ---------------------------------------------------------------------------
-- The staff calendar event
-- ---------------------------------------------------------------------------

-- | Make a redeemed ticket's calendar event exist on the staff calendar.
-- Idempotent — safe from the redeem handler and from every tick until the
-- event id is recorded. The event names the student only by the name they
-- chose and the ticket code; staff are the only attendees.
ensureReservationSideEffects :: BookingCtx -> Ticket -> IO ()
ensureReservationSideEffects ctx t = case tkRedemption t of
  Nothing -> pure ()
  Just r -> do
    slots <- bcSlots ctx
    case [s | s <- slots, slotIdOf s == redSlotId r] of
      [] -> pure () -- slot definition edited away; nothing sensible to do
      (s : _) -> ensureEvent ctx t r s

ensureEvent :: BookingCtx -> Ticket -> Redemption -> SlotSpec -> IO ()
ensureEvent ctx t r s = case redEventId r of
  Just _ -> pure ()
  Nothing -> do
    mcreds <- bcCreds ctx
    case mcreds of
      Nothing -> pure ()
      Just creds -> do
        venues <- bcVenues ctx
        let ev =
              CalendarEvent
                { ceSummary = redName r <> " - " <> tkProduct t <> " (" <> tkCode t <> ")"
                , ceDescription =
                    T.intercalate
                      "\n"
                      ( classLine "" s
                          <> [ "Booked as: " <> redName r
                             , "Ticket:    " <> tkCode t <> " (" <> tkProduct t <> ")"
                             , "Booking:   " <> bookingUrl ctx t
                             ]
                      )
                , ceLocation = venueLocation venues (slotArea s)
                , ceStart = slotDate s <> "T" <> slotStart s <> ":00" <> bcTzOffset ctx
                , ceEnd = slotDate s <> "T" <> slotEnd s <> ":00" <> bcTzOffset ctx
                , ceTimeZone = bcTimeZone ctx
                , ceAttendees = bcStaff ctx
                }
        res <- createCalendarEvent (bcManager ctx) creds ev
        case res of
          Right evId -> setEventId (bcStore ctx) (tkCode t) evId
          Left err -> putStrLn ("[booking] calendar event failed (will retry): " <> T.unpack err)

-- ---------------------------------------------------------------------------
-- Fill locations on existing staff events
-- ---------------------------------------------------------------------------

-- | Existing future events receive their matching venue address immediately.
-- @today@ only excludes archived classes; there is no disclosure delay.
announceVenues :: BookingCtx -> Text -> IO ()
announceVenues ctx today = do
  tickets <- listTickets (bcStore ctx)
  slots <- bcSlots ctx
  venues <- bcVenues ctx
  mapM_ (announceOne slots venues) tickets
  where
    announceOne slots venues t = case tkRedemption t of
      Just r
        | "venue" `notElem` tkNotesSent t
        , Just evId <- redEventId r
        , (s : _) <- [s | s <- slots, slotIdOf s == redSlotId r]
        , slotDate s >= today
        , Just _ <- venueForArea venues (slotArea s) -> do
            mcreds <- bcCreds ctx
            case mcreds of
              Nothing -> pure ()
              Just creds -> do
                res <- patchEventLocation (bcManager ctx) creds evId (venueLocation venues (slotArea s))
                case res of
                  Right () -> () <$ claimNote (bcStore ctx) (tkCode t) "venue"
                  Left err -> putStrLn ("[booking] event location patch failed (will retry): " <> T.unpack err)
      _ -> pure ()

-- ---------------------------------------------------------------------------
-- The student's calendar file
-- ---------------------------------------------------------------------------

-- | An iCalendar file for one reservation, for the student to save from
-- their booking page. It carries the class, its time (in UTC, from the
-- configured offset), where it is (the full matching venue address, else the area)
-- and a link back to the booking page — and no name.
data CalendarStatus = ConfirmedClass | PaymentPending deriving (Eq, Show)

-- | Calendar presentation data shared by reservations and payment previews.
-- Constructing this value grants no lesson entitlement.
data CalendarTicket = CalendarTicket
  { calendarCode :: !Text
  , calendarProduct :: !Text
  , calendarStatus :: !CalendarStatus
  }

bookingIcs :: BookingCtx -> [Venue] -> UTCTime -> CalendarTicket -> SlotSpec -> Text
bookingIcs ctx venues now t s =
  T.concat
    ( map
        ((<> "\r\n") . fold75)
        [ "BEGIN:VCALENDAR"
        , "VERSION:2.0"
        , "PRODID:-//swwstructor//booking//EN"
        , "CALSCALE:GREGORIAN"
        , "METHOD:PUBLISH"
        , "BEGIN:VEVENT"
        , "UID:" <> calendarCode t <> "@" <> host
        , "DTSTAMP:" <> utcStamp now
        , "DTSTART:" <> maybe "" utcStamp (slotUtc (slotStart s))
        , "DTEND:" <> maybe "" utcStamp (slotUtc (slotEnd s))
        , "SUMMARY:" <> icsText (titlePrefix <> calendarProduct t <> " - " <> bcSiteName ctx)
        , "LOCATION:" <> icsText location
        , "DESCRIPTION:" <> icsText (T.intercalate "\n" (classLine "" s <> ["Your booking: " <> calendarUrl]))
        , "URL:" <> calendarUrl
        , "END:VEVENT"
        , "END:VCALENDAR"
        ]
    )
  where
    host = T.takeWhile (/= '/') (T.drop 2 (snd (T.breakOn "//" (bcBaseUrl ctx))))
    location = venueLocation venues (slotArea s)
    calendarUrl = bcBaseUrl ctx <> "/booking/" <> calendarCode t
    titlePrefix = case calendarStatus t of
      ConfirmedClass -> ""
      PaymentPending -> "[PAYMENT PENDING] "
    slotUtc hhmm = do
      lt <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" (T.unpack (slotDate s <> " " <> hhmm)) :: Maybe LocalTime
      pure (localTimeToUTC (minutesToTimeZone (offsetSeconds (bcTzOffset ctx) `div` 60)) lt)
    utcStamp = T.pack . formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ"
    icsText = T.replace "\n" "\\n" . T.replace ";" "\\;" . T.replace "," "\\," . T.replace "\\" "\\\\"
    -- RFC 5545 folds lines over 75 octets; 60 characters keeps any UTF-8
    -- line safely inside that.
    fold75 l
      | T.length l <= 60 = l
      | otherwise = T.intercalate "\r\n " (T.chunksOf 60 l)

-- ---------------------------------------------------------------------------
-- Plumbing
-- ---------------------------------------------------------------------------

bookingUrl :: BookingCtx -> Ticket -> Text
bookingUrl ctx t = bcBaseUrl ctx <> "/booking/" <> tkCode t

-- | The slot's own description as an event / calendar-file line (none when
-- the class has no description), its markdown spelled out as plain text.
classLine :: Text -> SlotSpec -> [Text]
classLine lead s =
  [ lead <> markdownPlain (T.unwords (T.words (slotDescription s)))
  | not (T.null (T.strip (slotDescription s)))
  ]

-- | @\"+01:00\"@ → 3600. Malformed offsets read as UTC.
offsetSeconds :: Text -> Int
offsetSeconds t = case T.unpack t of
  (sign : rest)
    | sign `elem` ("+-" :: String)
    , (hh, ':' : mm) <- span (/= ':') rest
    , [(h, "")] <- reads hh
    , [(m, "")] <- reads mm ->
        (if sign == '-' then negate else id) (h * 3600 + m * 60)
  _ -> 0
