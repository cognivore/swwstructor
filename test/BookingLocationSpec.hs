{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Swwstructor.Content (SlotSpec (..))
import Swwstructor.Server.Booking (Ticket (..), newBookingStore)
import Swwstructor.Server.Notify (BookingCtx (..), CalendarTicket (..), CalendarStatus (..), bookingIcs)
import Swwstructor.Server.Venues (Venue (..), venueLocation)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  temp <- getTemporaryDirectory
  (path, handle) <- openTempFile temp "sww-location-test"
  hClose handle
  removeFile path
  store <- newBookingStore path
  let area = "Ukiyo, Covent Garden"
      venue = Venue "ukiyo" "UKIYO" "8 Slingsby Place, London WC2E 9AB" area "" "" True
      slot = SlotSpec "2027-01-13" "19:30" "21:30" area 8 0 "Mochi class"
      ticket = Ticket "ABCD-2345" "Group Single" "pi_fixture" "2026-10-29T12:00:00Z" "1" Nothing []
      ctx = BookingCtx manager store (pure Nothing) [] "Okashi School" "https://okashi-school.com" "Europe/London" "+00:00" (pure [slot]) (pure [venue]) "kitchen"
      -- A class months away must already contain its address. This is the
      -- actual calendar renderer, with no network, paid ticket or booking write.
      now = read "2026-10-29 12:00:00 UTC" :: UTCTime
      ics venues = T.replace "\r\n " "" (bookingIcs ctx venues now (CalendarTicket (tkCode ticket) (tkProduct ticket) ConfirmedClass) slot)
      pending = T.replace "\r\n " "" (bookingIcs ctx [venue] now (CalendarTicket (tkCode ticket) (tkProduct ticket) PaymentPending) slot)
      location = "UKIYO, 8 Slingsby Place, London WC2E 9AB"
      check label ok = unless ok (fail label)
  check "a uniquely matched venue resolves its full address" (venueLocation [venue] area == location)
  check "the calendar includes the address months before class" ("LOCATION:UKIYO\\, 8 Slingsby Place\\, London WC2E 9AB\r\n" `T.isInfixOf` ics [venue])
  check "calendar timing and booking reference remain intact" (all (`T.isInfixOf` ics [venue]) ["DTSTART:20270113T193000Z", "UID:ABCD-2345@okashi-school.com", "URL:https://okashi-school.com/booking/ABCD-2345"])
  check "missing venues retain the authored area" (venueLocation [] area == area && "LOCATION:Ukiyo\\, Covent Garden\r\n" `T.isInfixOf` ics [])
  check "inactive venues are not selected" (venueLocation [venue {vnActive = False}] area == area)
  check "ambiguous venues never select an arbitrary address" (venueLocation [venue, venue {vnId = "other", vnAddress = "Elsewhere"}] area == area)
  check "pending and confirmed ICS differ only by the title prefix" ("SUMMARY:[PAYMENT PENDING] " `T.isInfixOf` pending && T.replace "[PAYMENT PENDING] " "" pending == ics [venue])
  putStrLn "Booking location checks passed."
