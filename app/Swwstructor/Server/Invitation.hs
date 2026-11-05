{-# LANGUAGE OverloadedStrings #-}

-- | Reusable invitation terms and their immutable purchase attribution.
-- Booking owns persistence and serialises disabling with checkout admission.
-- Tokens are bearer capabilities: they deliberately have no Show instance.
module Swwstructor.Server.Invitation
  ( InvitationId, InvitationToken, parseInvitationId, parseInvitationToken
  , invitationIdText, invitationTokenText
  , InvitationTier (..), InvitationScope (..), tierPrice, tierReferralFee
  , InvitationDraft, mkInvitationDraft
  , Invitation, invitationId, invitationToken, invitationReferrer
  , invitationTier, invitationScope, invitationEnabled, invitationCreatedAt
  , invitationPrice, invitationReferralFee, invitationFromDraft
  , InvitationAttribution, attributionFor, iaInvitationId, iaReferrer
  , iaTier, iaScope, iaPrice, iaReferralFee
  , InvitationError (..), invitationErrorMessage
  , InvitationRegistry, emptyInvitationRegistry, registryInvitations
  , registryLookupToken, registryLookupId, registryInsert, registrySetEnabled, registryDelete
  , invitationRegistryToJSON, invitationRegistryFromJSON
  , attributionToJSON, attributionFromJSON
  ) where

import Data.Char (isControl)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Swwstructor.Content (SlotSpec (..))
import Swwstructor.Money (Currency (GBP), Price (..), Cents (..), price)
import StickyWM (JSON (..), jstr, (.:), (.:?))

newtype InvitationId = InvitationId Text deriving (Eq, Ord, Show)
newtype InvitationToken = InvitationToken Text deriving (Eq, Ord)

invitationIdText :: InvitationId -> Text
invitationIdText (InvitationId value) = value
invitationTokenText :: InvitationToken -> Text
invitationTokenText (InvitationToken value) = value

parseInvitationId :: Text -> Maybe InvitationId
parseInvitationId value = if validHex "iv_" 16 value then Just (InvitationId value) else Nothing
parseInvitationToken :: Text -> Maybe InvitationToken
parseInvitationToken value = if validHex "in_" 64 value then Just (InvitationToken value) else Nothing

validHex :: Text -> Int -> Text -> Bool
validHex prefix digits value = prefix `T.isPrefixOf` value && T.length value == T.length prefix + digits &&
  T.all (`elem` ("0123456789abcdef" :: String)) (T.drop (T.length prefix) value)

data InvitationTier = Friend | Partner deriving (Eq, Ord, Show)
data InvitationScope = GeneralTicket | EventTicket !SlotSpec deriving (Eq, Show)

-- Version-one terms are fixed. A future pricing policy needs a new explicit
-- version; changing these constants must not reprice an existing invitation.
tierPrice :: InvitationTier -> Price
tierPrice Friend = price GBP 65
tierPrice Partner = price GBP 75
tierReferralFee :: InvitationTier -> Price
tierReferralFee Friend = price GBP 0
tierReferralFee Partner = price GBP 5

data InvitationDraft = InvitationDraft !Text !InvitationTier !InvitationScope deriving (Eq, Show)
data Invitation = Invitation
  { invitationId :: !InvitationId
  , invitationToken :: !InvitationToken
  , invitationReferrer :: !Text
  , invitationTier :: !InvitationTier
  , invitationScope :: !InvitationScope
  , invitationEnabled :: !Bool
  , invitationCreatedAt :: !Text
  } deriving (Eq)

invitationPrice :: Invitation -> Price
invitationPrice = tierPrice . invitationTier
invitationReferralFee :: Invitation -> Price
invitationReferralFee = tierReferralFee . invitationTier

data InvitationError
  = InvitationNotFound
  | InvitationDisabled
  | InvitationActive
  | InvitationHasHistory
  | InvitationConflict
  | InvitationLimitReached
  | InvitationInvalid !Text
  deriving (Eq, Show)

invitationErrorMessage :: InvitationError -> Text
invitationErrorMessage failure = case failure of
  InvitationNotFound -> "This invitation could not be found. No purchase was started."
  InvitationDisabled -> "This invitation is no longer available for new purchases. Existing tickets and checkouts still work."
  InvitationActive -> "Disable this invitation before permanently deleting it. Nothing was deleted."
  InvitationHasHistory -> "This invitation has checkout history and cannot be permanently deleted. Keep it disabled to prevent new purchases."
  InvitationConflict -> "The invitation reference already exists. No invitation was changed."
  InvitationLimitReached -> "The invitation registry has reached its limit of 10,000 links. No invitation was created."
  InvitationInvalid reason -> "The invitation is invalid: " <> reason <> "."

mkInvitationDraft :: Text -> InvitationTier -> InvitationScope -> Either InvitationError InvitationDraft
mkInvitationDraft referrer tier scope
  | T.null chosen || T.length chosen > 100 || T.any isControl chosen =
      Left (InvitationInvalid "the referrer's name must be between 1 and 100 characters without control characters")
  | not (validScope scope) = Left (InvitationInvalid "the event reference must contain a valid date and start time")
  | otherwise = Right (InvitationDraft chosen tier (case scope of GeneralTicket -> GeneralTicket; EventTicket slot -> EventTicket slot {slotTaken = 0}))
  where chosen = T.strip referrer

validScope :: InvitationScope -> Bool
validScope GeneralTicket = True
validScope (EventTicket slot) = slotCap slot >= 1 && slotCap slot <= 1000 &&
  (parseTimeM True defaultTimeLocale "%F" (T.unpack (slotDate slot)) :: Maybe Day) /= Nothing &&
  (parseTimeM True defaultTimeLocale "%R" (T.unpack (slotStart slot)) :: Maybe TimeOfDay) /= Nothing &&
  (parseTimeM True defaultTimeLocale "%R" (T.unpack (slotEnd slot)) :: Maybe TimeOfDay) /= Nothing &&
  T.length (slotArea slot) <= 2000 && T.length (slotDescription slot) <= 10000

invitationFromDraft :: InvitationId -> InvitationToken -> Text -> InvitationDraft -> Invitation
invitationFromDraft ident token created (InvitationDraft referrer tier scope) =
  Invitation ident token referrer tier scope True created

data InvitationAttribution = InvitationAttribution
  { iaInvitationId :: !InvitationId
  , iaReferrer :: !Text
  , iaTier :: !InvitationTier
  , iaScope :: !InvitationScope
  , iaPrice :: !Price
  , iaReferralFee :: !Price
  } deriving (Eq, Show)

attributionFor :: Invitation -> InvitationAttribution
attributionFor invitation = InvitationAttribution (invitationId invitation) (invitationReferrer invitation)
  (invitationTier invitation) (invitationScope invitation) (invitationPrice invitation) (invitationReferralFee invitation)

newtype InvitationRegistry = InvitationRegistry [Invitation] deriving (Eq)
emptyInvitationRegistry :: InvitationRegistry
emptyInvitationRegistry = InvitationRegistry []
registryInvitations :: InvitationRegistry -> [Invitation]
registryInvitations (InvitationRegistry invitations) = invitations
registryLookupToken :: InvitationRegistry -> InvitationToken -> Maybe Invitation
registryLookupToken registry token = lookup token [(invitationToken i, i) | i <- registryInvitations registry]
registryLookupId :: InvitationRegistry -> InvitationId -> Maybe Invitation
registryLookupId registry ident = lookup ident [(invitationId i, i) | i <- registryInvitations registry]

registryInsert :: Invitation -> InvitationRegistry -> Either InvitationError InvitationRegistry
registryInsert invitation registry
  | registryLookupId registry (invitationId invitation) /= Nothing || registryLookupToken registry (invitationToken invitation) /= Nothing = Left InvitationConflict
  | length (registryInvitations registry) >= 10000 = Left InvitationLimitReached
  | otherwise = Right (InvitationRegistry (registryInvitations registry <> [invitation]))

registrySetEnabled :: InvitationId -> Bool -> InvitationRegistry -> Either InvitationError (InvitationRegistry, Invitation)
registrySetEnabled ident enabled registry = case registryLookupId registry ident of
  Nothing -> Left InvitationNotFound
  Just invitation ->
    let changed = invitation {invitationEnabled = enabled}
     in Right (InvitationRegistry [if invitationId i == ident then changed else i | i <- registryInvitations registry], changed)

-- Booking must also reject every checkout reference in the same transaction.
registryDelete :: InvitationId -> InvitationRegistry -> Either InvitationError InvitationRegistry
registryDelete ident registry = case registryLookupId registry ident of
  Nothing -> Left InvitationNotFound
  Just invitation
    | invitationEnabled invitation -> Left InvitationActive
    | otherwise -> Right (InvitationRegistry (filter ((/= ident) . invitationId) (registryInvitations registry)))

invitationRegistryToJSON :: InvitationRegistry -> JSON
invitationRegistryToJSON registry = JArr
  [ JObj (termsFields (attributionFor invitation) <>
      [("token", string (invitationTokenText (invitationToken invitation)))
      , ("enabled", JBool (invitationEnabled invitation)), ("createdAt", string (invitationCreatedAt invitation))])
  | invitation <- registryInvitations registry ]

invitationRegistryFromJSON :: JSON -> Either String InvitationRegistry
invitationRegistryFromJSON (JArr entries) = do
  if length entries > 10000 then Left "invitations: registry exceeds the 10,000-link limit" else pure ()
  invitations <- mapM invitationFromJSON entries
  if S.size (S.fromList (map invitationId invitations)) /= length invitations ||
       S.size (S.fromList (map invitationToken invitations)) /= length invitations
    then Left "invitations: duplicate identity or token; refusing ambiguous ownership"
    else Right (InvitationRegistry invitations)
invitationRegistryFromJSON _ = Left "invitations: registry must be an array"

invitationFromJSON :: JSON -> Either String Invitation
invitationFromJSON value = do
  attribution <- attributionFromJSON value
  tokenText <- txt value "token"
  token <- maybe (Left "invitation has an invalid token; value withheld") Right (parseInvitationToken tokenText)
  enabled <- case value .:? "enabled" of Just (JBool flag) -> Right flag; _ -> Left "invitation has no boolean enabled state"
  created <- txt value "createdAt"
  if T.null created || T.length created > 40 || T.any isControl created then Left "invitation has an invalid creation timestamp" else pure ()
  pure (Invitation (iaInvitationId attribution) token (iaReferrer attribution) (iaTier attribution) (iaScope attribution) enabled created)

attributionToJSON :: InvitationAttribution -> JSON
attributionToJSON = JObj . termsFields

termsFields :: InvitationAttribution -> [(String, JSON)]
termsFields attribution =
  let Price (Cents amount) _ = iaPrice attribution
      Price (Cents fee) _ = iaReferralFee attribution
   in [("termsVersion", JNum 1), ("id", string (invitationIdText (iaInvitationId attribution)))
      , ("referrer", string (iaReferrer attribution))
      , ("tier", string (case iaTier attribution of Friend -> "friend"; Partner -> "partner"))
      , ("scope", case iaScope attribution of
          GeneralTicket -> JObj [("kind", string "general")]
          EventTicket slot -> JObj [("kind", string "event"), ("slot", JObj
            [("date", string (slotDate slot)), ("start", string (slotStart slot)), ("end", string (slotEnd slot))
            , ("area", string (slotArea slot)), ("capacity", JNum (fromIntegral (slotCap slot)))
            , ("description", string (slotDescription slot))])])
      , ("amount", JNum (fromIntegral amount)), ("referralFee", JNum (fromIntegral fee)), ("currency", string "gbp")]

attributionFromJSON :: JSON -> Either String InvitationAttribution
attributionFromJSON value = do
  if value .:? "termsVersion" /= Just (JNum 1) then Left "invitation terms version is unsupported" else pure ()
  identText <- txt value "id"
  ident <- maybe (Left "invitation has an invalid public identity") Right (parseInvitationId identText)
  referrer <- txt value "referrer"
  tierText <- txt value "tier"
  tier <- case tierText of "friend" -> Right Friend; "partner" -> Right Partner; _ -> Left "invitation has an unsupported tier"
  scopeValue <- value .: "scope"
  kind <- txt scopeValue "kind"
  scope <- case kind of
    "general" | scopeValue .:? "slot" == Nothing -> Right GeneralTicket
    "event" -> do
      slot <- scopeValue .: "slot"
      capacity <- case slot .:? "capacity" of
        Just (JNum n) | n >= 1, n <= 1000, fromInteger (round n) == n -> Right (round n)
        _ -> Left "invitation event capacity is invalid"
      EventTicket <$> (SlotSpec <$> txt slot "date" <*> txt slot "start" <*> txt slot "end" <*> txt slot "area" <*> pure capacity <*> pure 0 <*> txt slot "description")
    _ -> Left "invitation has an invalid scope"
  _ <- either (const (Left "invitation has invalid referrer or event terms")) Right (mkInvitationDraft referrer tier scope)
  if referrer /= T.strip referrer then Left "invitation referrer is not canonical" else pure ()
  let expected = InvitationAttribution ident referrer tier scope (tierPrice tier) (tierReferralFee tier)
      Price (Cents amount) _ = iaPrice expected
      Price (Cents fee) _ = iaReferralFee expected
  if value .:? "currency" /= Just (string "gbp") || value .:? "amount" /= Just (JNum (fromIntegral amount)) || value .:? "referralFee" /= Just (JNum (fromIntegral fee))
    then Left "invitation financial terms do not match the declared version and tier"
    else Right expected

txt :: JSON -> String -> Either String Text
txt value key = T.pack <$> (jstr =<< value .: key)
string :: Text -> JSON
string = JStr . T.unpack
