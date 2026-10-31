{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Google Calendar + Gmail, over a stored OAuth refresh token — the
-- \"install an app on the backend\" pattern: a Google OAuth client
-- (client id + secret) plus a refresh token minted once by a human consent,
-- kept in the encrypted secret store. Every call mints a short-lived access
-- token from the refresh token (Google's tokens last ~1h; we simply mint per
-- operation — bookings are rare events, not a hot path).
--
-- Calendar events are created on the token owner's primary calendar with
-- @sendUpdates=all@, which makes Google email a real invitation to every
-- attendee — the students and the school staff alike. Mail is sent via the
-- Gmail REST API (@users\/me\/messages\/send@) as the token owner.
module Swwstructor.Server.Google
  ( GoogleCreds (..)
  , googleCredsOf
  , googleMailCredsOf
  , CalendarEvent (..)
  , createCalendarEvent
  , patchEventLocation
  , Mail (..)
  , sendMail
  , sendReservationEmail
  , reservationMime
  , verifiedSendAs
  ) where

import Control.Exception (SomeException, try)
import Data.ByteArray.Encoding (Base (Base64, Base64URLUnpadded), convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
  ( Manager
  , HttpException
  , Request
  , RequestBody (RequestBodyBS)
  , Response
  , httpLbs
  , method
  , parseRequest
  , requestBody
  , requestHeaders
  , responseBody
  , responseStatus
  , responseTimeout
  , responseTimeoutMicro
  )
import Network.HTTP.Types.Status (statusCode)
import Swwstructor.Checkout (encodeForm)
import Swwstructor.Server.ReservationEmail
import Swwstructor.Server.SecretStore
  ( SecretBundle (sbGoogleClientId, sbGoogleClientSecret, sbGoogleRefreshToken, sbGoogleMailRefreshToken, sbGoogleMailFrom)
  , SecretValue
  , secretText
  )
import StickyWM (JSON (JArr, JBool, JObj, JStr), decode, encode, jstr, (.:))

-- | The three values that make the backend able to act as a Google account.
data GoogleCreds = GoogleCreds
  { gcClientId :: !SecretValue
  , gcClientSecret :: !SecretValue
  , gcRefreshToken :: !SecretValue
  , gcMailFrom :: !(Maybe Mailbox)
  }

-- | The bundle's Google credentials, when all three parts are present.
googleCredsOf :: SecretBundle -> Maybe GoogleCreds
googleCredsOf b =
  GoogleCreds
    <$> sbGoogleClientId b
    <*> sbGoogleClientSecret b
    <*> sbGoogleRefreshToken b
    <*> pure Nothing

-- An email-specific account does not move existing calendar events or change
-- the calendar creator. Older configurations keep their shared connection.
googleMailCredsOf :: SecretBundle -> Maybe GoogleCreds
googleMailCredsOf b =
  GoogleCreds <$> sbGoogleClientId b <*> sbGoogleClientSecret b
    <*> (case sbGoogleMailRefreshToken b of Just token -> Just token; Nothing -> sbGoogleRefreshToken b)
    <*> pure (sbGoogleMailFrom b)

-- | A calendar event to create. Times are RFC-3339 with offset
-- (e.g. @2026-09-01T11:00:00+01:00@); 'ceTimeZone' is the IANA name Google
-- files the event under (e.g. @Europe\/London@).
data CalendarEvent = CalendarEvent
  { ceSummary :: !Text
  , ceDescription :: !Text
  , ceLocation :: !Text
  , ceStart :: !Text
  , ceEnd :: !Text
  , ceTimeZone :: !Text
  , ceAttendees :: ![Text]
  }
  deriving (Show)

-- | A plain-text email. One message, all recipients on To (a class booking is
-- a small, mutually-known circle).
data Mail = Mail
  { mTo :: ![Text]
  , mReplyTo :: !(Maybe Text)
  , mSubject :: !Text
  , mBody :: !Text
  }
  deriving (Show)

-- ---------------------------------------------------------------------------
-- OAuth: refresh token -> access token
-- ---------------------------------------------------------------------------

mintAccessToken :: Manager -> GoogleCreds -> IO (Either Text Text)
mintAccessToken mgr creds = do
  let form =
        encodeForm
          [ ("client_id", secretText (gcClientId creds))
          , ("client_secret", secretText (gcClientSecret creds))
          , ("refresh_token", secretText (gcRefreshToken creds))
          , ("grant_type", "refresh_token")
          ]
  resp <-
    httpJson
      mgr
      "POST"
      "https://oauth2.googleapis.com/token"
      Nothing
      "application/x-www-form-urlencoded"
      (Just (TE.encodeUtf8 form))
  pure $ do
    v <- resp
    either (\e -> Left ("google token response: " <> T.pack e)) Right (T.pack <$> (jstr =<< v .: "access_token"))

-- ---------------------------------------------------------------------------
-- Calendar
-- ---------------------------------------------------------------------------

-- | Create the event on the token owner's primary calendar, inviting every
-- attendee (@sendUpdates=all@ → Google emails the invitations). Returns the
-- event id.
createCalendarEvent :: Manager -> GoogleCreds -> CalendarEvent -> IO (Either Text Text)
createCalendarEvent mgr creds ev = do
  etok <- mintAccessToken mgr creds
  case etok of
    Left e -> pure (Left e)
    Right tok -> do
      let body =
            JObj
              [ ("summary", JStr (T.unpack (ceSummary ev)))
              , ("description", JStr (T.unpack (ceDescription ev)))
              , ("location", JStr (T.unpack (ceLocation ev)))
              , ("start", JObj [("dateTime", JStr (T.unpack (ceStart ev))), ("timeZone", JStr (T.unpack (ceTimeZone ev)))])
              , ("end", JObj [("dateTime", JStr (T.unpack (ceEnd ev))), ("timeZone", JStr (T.unpack (ceTimeZone ev)))])
              , ("attendees", JArr [JObj [("email", JStr (T.unpack a))] | a <- ceAttendees ev])
              , ("reminders", JObj [("useDefault", JBool True)])
              ]
      resp <-
        httpJson
          mgr
          "POST"
          "https://www.googleapis.com/calendar/v3/calendars/primary/events?sendUpdates=all"
          (Just tok)
          "application/json; charset=utf-8"
          (Just (TE.encodeUtf8 (T.pack (encode body))))
      pure $ do
        v <- resp
        either (\e -> Left ("calendar response: " <> T.pack e)) Right (T.pack <$> (jstr =<< v .: "id"))

-- | Update an event's location (e.g. when the exact kitchen is announced a
-- week ahead). Quiet — attendees' calendars sync the change; the reminder
-- mail is the announcement, so no extra Google notification is sent.
patchEventLocation :: Manager -> GoogleCreds -> Text -> Text -> IO (Either Text ())
patchEventLocation mgr creds eventId location = do
  etok <- mintAccessToken mgr creds
  case etok of
    Left e -> pure (Left e)
    Right tok -> do
      let body = JObj [("location", JStr (T.unpack location))]
      resp <-
        httpJson
          mgr
          "PATCH"
          ("https://www.googleapis.com/calendar/v3/calendars/primary/events/" <> T.unpack (sanitizeId eventId) <> "?sendUpdates=none")
          (Just tok)
          "application/json; charset=utf-8"
          (Just (TE.encodeUtf8 (T.pack (encode body))))
      pure (() <$ resp)
  where
    -- Event ids are base32hex-ish; strip anything else so a stored id can
    -- never smuggle path segments into the URL.
    sanitizeId = T.filter (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_' || c == '-')

-- ---------------------------------------------------------------------------
-- Gmail
-- ---------------------------------------------------------------------------

-- | Dedicated reservation delivery: distinguish an explicit rejection from an
-- unknown send result; only a provider receipt is reported as accepted. Never
-- retain an HTTP exception's request headers (which include the access token).
sendReservationEmail :: Manager -> GoogleCreds -> Text -> Delivery -> IO EmailState
sendReservationEmail mgr creds senderName delivery = do
  etok <- mintAccessToken mgr creds
  case etok of
    Left err -> pure (Failed ("Google authorisation failed before sending. Check the Google connection. " <> if "HTTP " `T.isPrefixOf` err then T.take 8 err else ""))
    Right tok -> do
      sender <- case gcMailFrom creds of
        Just requested -> do
          aliases <- httpJson mgr "GET" "https://gmail.googleapis.com/gmail/v1/users/me/settings/sendAs" (Just tok) "application/json" Nothing
          pure (aliases >>= verifiedSendAs requested)
        Nothing -> do
          profile <- httpJson mgr "GET" "https://gmail.googleapis.com/gmail/v1/users/me/profile" (Just tok) "application/json" Nothing
          pure $ do
            value <- profile
            address <- either (Left . T.pack) Right (jstr =<< value .: "emailAddress")
            parseMailbox (T.pack address)
      case sender of
        Left _ -> pure (Failed "Google could not verify the configured sender. Check Gmail read access and that the From address is an accepted send-as alias. No email was sent.")
        Right address -> sendFrom tok address
  where
    sendFrom tok sender = do
      base <- parseRequest "https://gmail.googleapis.com/gmail/v1/users/me/messages/send"
      let mime = reservationMime senderName sender delivery
          payload = JObj [("raw", JStr (T.unpack (TE.decodeUtf8 (convertToBase Base64URLUnpadded mime :: ByteString))))]
          req = base {method = "POST", requestHeaders = [("Authorization", TE.encodeUtf8 ("Bearer " <> tok)), ("Content-Type", "application/json")], requestBody = RequestBodyBS (TE.encodeUtf8 (T.pack (encode payload))), responseTimeout = responseTimeoutMicro 20_000_000}
      result <- try (httpLbs req mgr) :: IO (Either HttpException (Response BL.ByteString))
      pure $ case result of
        Left _ -> Uncertain "Gmail did not return a response. Check the sender's Sent folder before retrying."
        Right resp ->
          let status = statusCode (responseStatus resp)
           in if status >= 200 && status < 300
                then case TE.decodeUtf8' (BL.toStrict (responseBody resp)) of
                  Right bytes -> case decode (T.unpack bytes) >>= (\v -> jstr =<< v .: "id") of
                    Right ident -> Sent (T.pack ident)
                    Left _ -> Uncertain "Gmail accepted the request but returned no message identifier. Check Sent mail."
                  Left _ -> Uncertain "Gmail accepted the request but its response was unreadable. Check Sent mail."
                else if status >= 400 && status < 500
                  then Failed ("Gmail rejected the send (HTTP " <> T.pack (show status) <> "). Check Gmail send permission and account limits before retrying.")
                  else Uncertain ("Gmail returned HTTP " <> T.pack (show status) <> ". Check Sent mail before retrying.")

-- A configured alias must be accepted by Gmail for this exact account. Never
-- silently fall back to a personal address when the school alias is unavailable.
verifiedSendAs :: Mailbox -> JSON -> Either Text Mailbox
verifiedSendAs requested value = case value .: "sendAs" of
  Right (JArr aliases)
    | any accepted aliases -> Right requested
  _ -> Left "The configured From address is not verified by Gmail."
  where
    accepted alias = case (alias .: "sendAsEmail", alias .: "verificationStatus") of
      (Right (JStr address), Right (JStr "accepted")) -> parseMailbox (T.pack address) == Right requested
      _ -> False

-- Gmail supplies the authenticated mailbox or verified alias; the site's name belongs only to
-- this message, so other mail from the account keeps its own identity.
reservationMime :: Text -> Mailbox -> Delivery -> ByteString
reservationMime senderName sender delivery = TE.encodeUtf8 $ T.intercalate "\r\n"
  [ "From: " <> encodedHeader senderName <> " <" <> mailboxText sender <> ">"
  , "To: " <> mailboxText (deliveryTo delivery)
  , "Subject: " <> encodedHeader (deliverySubject delivery)
  , "MIME-Version: 1.0"
  , "Content-Type: text/plain; charset=utf-8"
  , "Content-Transfer-Encoding: base64"
  , ""
  , T.intercalate "\r\n" (chunks76 (TE.decodeUtf8 (convertToBase Base64 (TE.encodeUtf8 (deliveryBody delivery)) :: ByteString)))
  ]
  where
    encodedHeader value = "=?UTF-8?B?" <> TE.decodeUtf8 (convertToBase Base64 (TE.encodeUtf8 (T.unwords (T.words value))) :: ByteString) <> "?="
    chunks76 value | T.null value = []
                   | otherwise = T.take 76 value : chunks76 (T.drop 76 value)

-- | Send a plain-text mail as the token owner. The Gmail API takes the whole
-- RFC-822 message base64url-encoded in a JSON envelope.
sendMail :: Manager -> GoogleCreds -> Mail -> IO (Either Text ())
sendMail mgr creds m = do
  etok <- mintAccessToken mgr creds
  case etok of
    Left e -> pure (Left e)
    Right tok -> do
      let mime =
            T.intercalate
              "\r\n"
              ( [ "To: " <> T.intercalate ", " (mTo m) ]
                  <> maybe [] (\r -> ["Reply-To: " <> r]) (mReplyTo m)
                  <> [ "Subject: " <> mSubject m
                     , "MIME-Version: 1.0"
                     , "Content-Type: text/plain; charset=utf-8"
                     , "Content-Transfer-Encoding: 8bit"
                     , ""
                     , T.replace "\n" "\r\n" (mBody m)
                     ]
              )
          raw = TE.decodeUtf8 (convertToBase Base64URLUnpadded (TE.encodeUtf8 mime) :: ByteString)
          body = JObj [("raw", JStr (T.unpack raw))]
      resp <-
        httpJson
          mgr
          "POST"
          "https://gmail.googleapis.com/gmail/v1/users/me/messages/send"
          (Just tok)
          "application/json; charset=utf-8"
          (Just (TE.encodeUtf8 (T.pack (encode body))))
      pure (() <$ resp)

-- ---------------------------------------------------------------------------
-- Shared HTTP plumbing
-- ---------------------------------------------------------------------------

-- | POST a JSON-speaking API: bearer auth, catch transport exceptions, insist
-- on 2xx, decode the body. Errors are terse one-liners fit for the journal.
httpJson :: Manager -> ByteString -> String -> Maybe Text -> ByteString -> Maybe ByteString -> IO (Either Text JSON)
httpJson mgr verb url mtok ctype mbody = do
  ereq <- try (parseRequest url) :: IO (Either SomeException Request)
  case ereq of
    Left e -> pure (Left (T.pack (show e)))
    Right base -> do
      let auth = maybe [] (\t -> [("Authorization", TE.encodeUtf8 ("Bearer " <> t))]) mtok
          req =
            base
              { method = verb
              , requestHeaders = auth <> [("Content-Type", ctype)]
              , requestBody = maybe (requestBody base) RequestBodyBS mbody
              }
      res <- try (httpLbs req mgr) :: IO (Either SomeException (Response BL.ByteString))
      pure $ case res of
        Left (e :: SomeException) -> Left (T.pack (show e))
        Right resp ->
          let code = statusCode (responseStatus resp)
              bodyStr = T.unpack (TE.decodeUtf8 (BL.toStrict (responseBody resp)))
           in if code >= 200 && code < 300
                then either (Left . T.pack) Right (decode bodyStr)
                else Left ("HTTP " <> T.pack (show code) <> ": " <> T.pack (take 300 bodyStr))
