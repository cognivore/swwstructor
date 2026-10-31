{-# LANGUAGE OverloadedStrings #-}

-- | The admin-owned recipients and immutable reservation email outbox.
-- Customer addresses are never requested. Each recipient receives a separate
-- message, so venue contacts do not learn one another's addresses.
module Swwstructor.Server.ReservationEmail
  ( Mailbox, mailboxText, parseMailbox, parseRecipients
  , EmailState (..), Delivery (..), ReservationEmails (..), emptyReservationEmails
  , reservationDelivery, emailStatus, emailsToJSON, emailsFromJSON
  ) where

import Data.Char (isAscii, isAlphaNum)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Swwstructor.Content (SlotSpec (..))
import Swwstructor.Block (markdownPlain)
import StickyWM (JSON (..), jstr, (.:))

newtype Mailbox = Mailbox { mailboxText :: Text } deriving (Eq, Show)

-- Bare ASCII mailboxes only, deliberately excluding display names and header
-- syntax. The UI states this supported form; it is not an RFC-wide validator.
parseMailbox :: Text -> Either Text Mailbox
parseMailbox raw = case T.splitOn "@" (T.strip raw) of
  [local, domain]
    | T.length raw <= 254, not (T.null local), T.length local <= 64
    , T.all (\c -> isAscii c && (isAlphaNum c || c `elem` (".!#$%&'*+-/=?^_`{|}~" :: String))) local
    , not (T.isPrefixOf "." local || T.isSuffixOf "." local || ".." `T.isInfixOf` local)
    , let labels = T.splitOn "." domain
    , length labels >= 2, all validLabel labels -> Right (Mailbox (local <> "@" <> T.toLower domain))
  _ -> Left "Enter a bare email address such as bookings@example.com (no display name or extra headers)."
  where
    validLabel label = not (T.null label) && T.length label <= 63
      && not (T.isPrefixOf "-" label || T.isSuffixOf "-" label)
      && T.all (\c -> isAscii c && (isAlphaNum c || c == '-')) label

parseRecipients :: Text -> Either Text [Mailbox]
parseRecipients raw
  | T.length raw > 6000 = Left "The recipient list is too long. Use at most 20 addresses."
  | otherwise = do
      addresses <- nub <$> traverse parseMailbox (filter (not . T.null) (map T.strip (T.split (\c -> c == ',' || c == '\n') raw)))
      if length addresses <= 20 then Right addresses else Left "Use at most 20 recipient addresses."

data EmailState = Queued | Sending | Sent Text | Failed Text | Uncertain Text
  deriving (Eq, Show)

data Delivery = Delivery
  { deliveryId :: !Text
  , deliveryTo :: !Mailbox
  , deliverySubject :: !Text
  , deliveryBody :: !Text
  , deliveryCreated :: !Text
  , deliveryState :: !EmailState
  } deriving (Eq, Show)

data ReservationEmails = ReservationEmails
  { emailsEnabled :: !Bool
  , emailsRecipients :: ![Mailbox]
  , emailsDeliveries :: ![Delivery]
  } deriving (Eq, Show)

emptyReservationEmails :: ReservationEmails
emptyReservationEmails = ReservationEmails False [] []

reservationDelivery :: Bool -> Text -> Text -> Text -> Text -> Text -> SlotSpec -> Mailbox -> Delivery
reservationDelivery isTest ident now code productName seats slot recipient = Delivery
  ident recipient
  ((if isTest then "[TEST] " else "") <> places <> (if places == "1" then " place reserved — " else " places reserved — ") <> slotDate slot <> " " <> slotStart slot)
  (T.unlines
    [ if isTest then "TEST NOTIFICATION — no ticket was redeemed and no place was reserved." else "A ticket has been redeemed and a class reservation is confirmed."
    , ""
    , "Class: " <> markdownPlain (slotDescription slot)
    , "Date: " <> slotDate slot
    , "Time: " <> slotStart slot <> "–" <> slotEnd slot
    , "Venue: " <> slotArea slot
    , "Ticket: " <> code
    , "Ticket type: " <> productName
    , "Places reserved: " <> places <> (if seats == "all" then " (private class — all places)" else "")
    ]) now Queued
  where
    places = if seats == "all" then T.pack (show (slotCap slot)) else seats

emailStatus :: EmailState -> Text
emailStatus Queued = "Queued"
emailStatus Sending = "Sending — delivery not yet confirmed"
emailStatus (Sent _) = "Accepted by Gmail"
emailStatus (Failed msg) = "Not sent: " <> msg
emailStatus (Uncertain msg) = "Delivery uncertain: " <> msg

emailsToJSON :: ReservationEmails -> JSON
emailsToJSON settings = JObj
  [ ("enabled", JBool (emailsEnabled settings))
  , ("recipients", JArr (map (JStr . T.unpack . mailboxText) (emailsRecipients settings)))
  , ("deliveries", JArr (map encodeDelivery (emailsDeliveries settings)))
  ]
  where
    encodeDelivery d = JObj
      [ pair "id" (deliveryId d), pair "to" (mailboxText (deliveryTo d))
      , pair "subject" (deliverySubject d), pair "body" (deliveryBody d), pair "created" (deliveryCreated d)
      , ("state", let (tag, detail) = case deliveryState d of
                       Queued -> ("queued", "")
                       Sending -> ("sending", "")
                       Sent receipt -> ("sent", receipt)
                       Failed why -> ("failed", why)
                       Uncertain why -> ("uncertain", why)
                   in JObj [pair "tag" tag, pair "detail" detail])
      ]
    pair key value = (key, JStr (T.unpack value))

emailsFromJSON :: JSON -> Either String ReservationEmails
emailsFromJSON v = do
  enabled <- case v .: "enabled" of Right (JBool b) -> Right b; _ -> Left "reservation emails: enabled must be boolean"
  recipients <- array v "recipients" >>= traverse address
  if length recipients > 20 then Left "reservation emails: too many recipients" else pure ()
  deliveries <- array v "deliveries" >>= traverse decodeDelivery
  pure (ReservationEmails enabled recipients deliveries)
  where
    txt o key = T.pack <$> (jstr =<< o .: key)
    array o key = case o .: key of Right (JArr xs) -> Right xs; _ -> Left ("reservation emails: expected array " <> key)
    address j = jstr j >>= either (Left . T.unpack) Right . parseMailbox . T.pack
    decodeDelivery d = do
      ident <- txt d "id"
      recipient <- d .: "to" >>= address
      subject <- txt d "subject"
      body <- txt d "body"
      created <- txt d "created"
      st <- d .: "state"
      tag <- txt st "tag"
      detail <- txt st "detail"
      state <- case tag of
        "queued" -> Right Queued
        -- A restart after a send began has an unknown external outcome. Never
        -- blindly resend: the admin can check the sender's Sent folder first.
        "sending" -> Right (Uncertain "The server restarted during sending. Check Sent mail before retrying.")
        "sent" -> Right (Sent detail)
        "failed" -> Right (Failed detail)
        "uncertain" -> Right (Uncertain detail)
        _ -> Left "reservation emails: unknown delivery state"
      pure (Delivery ident recipient subject body created state)
