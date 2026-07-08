{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The booking reminder loop: a one-minute in-process tick (the NLCC
-- pattern — no cron, no external queue; systemd restarts the process, the
-- ticket ledger makes every tick idempotent). Each tick it
--
--   1. retries unfinished side effects: tickets whose \"here is your code\"
--      mail hasn't gone out, reservations missing their calendar event or
--      confirmation mail (e.g. Google was down at booking time), and
--   2. sends the dated nags — one week before the class (from 09:00 local)
--      and on the day itself (from 07:00 local).
module Swwstructor.Server.Reminders
  ( startReminderLoop
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forever, void)
import qualified Data.Text as T
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Swwstructor.Server.Booking (listTickets)
import Swwstructor.Server.Notify
  ( BookingCtx (bcStore, bcTzOffset)
  , ensureReservationSideEffects
  , notifyTicketIssued
  , sendDueReminders
  )

startReminderLoop :: BookingCtx -> IO ()
startReminderLoop ctx = void . forkIO . forever $ do
  r <- try (tick ctx) :: IO (Either SomeException ())
  case r of
    Left e -> putStrLn ("[reminders] tick failed: " <> show e)
    Right () -> pure ()
  threadDelay 60_000_000

tick :: BookingCtx -> IO ()
tick ctx = do
  now <- getCurrentTime
  -- Class-timezone civil time from the configured offset (see 'bcTzOffset').
  let local = addUTCTime (fromIntegral (offsetSeconds (bcTzOffset ctx))) now
      today = T.pack (formatTime defaultTimeLocale "%Y-%m-%d" local)
      hour = read (formatTime defaultTimeLocale "%H" local) :: Int
  tickets <- listTickets (bcStore ctx)
  -- Heal anything a handler could not finish at request time.
  mapM_ (notifyTicketIssued ctx) tickets
  mapM_ (ensureReservationSideEffects ctx) tickets
  -- The dated nags.
  sendDueReminders ctx today hour

-- | @\"+01:00\"@ → 3600. Malformed offsets read as UTC.
offsetSeconds :: T.Text -> Int
offsetSeconds t = case T.unpack t of
  (sign : rest)
    | sign `elem` ("+-" :: String)
    , (hh, ':' : mm) <- span (/= ':') rest
    , [(h, "")] <- reads hh
    , [(m, "")] <- reads mm ->
        (if sign == '-' then negate else id) (h * 3600 + m * 60)
  _ -> 0
