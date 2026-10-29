{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The booking upkeep loop: a one-minute in-process tick (the NLCC
-- pattern — no cron, no external queue; systemd restarts the process, the
-- ticket ledger makes every tick idempotent). Each tick it
--
--   1. retries unfinished side effects: reservations whose staff calendar
--      event could not be created at booking time (e.g. Google was down),
--   2. fills the venue address on existing future events immediately.
--
-- Nothing here contacts a student: the site holds no way to.
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
  , announceVenues
  , ensureReservationSideEffects
  , offsetSeconds
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
  tickets <- listTickets (bcStore ctx)
  -- Heal anything a handler could not finish at request time.
  mapM_ (ensureReservationSideEffects ctx) tickets
  announceVenues ctx today
