{-# LANGUAGE OverloadedStrings #-}

-- | Safe presentation projections. Routes retain authority over invitation
-- lookup, price, expiry, CSRF, purchases and the canonical bearer URL.
module Swwstructor.Server.InvitationView
  ( InvitationLanding (..)
  , InvitationEvent (..)
  , InvitationRow (..)
  , InvitationDetail (..)
  , InvitationSaleRow (..)
  , invitationLandingPage
  , invitationsPage
  , invitationDetailPage
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (isNothing)
import Swwstructor.Html
import Swwstructor.Money (Price, formatPrice)
import Swwstructor.Server.Admin (adminDoc, topbar)
import Swwstructor.Theme (Theme (..), themeCss)

data InvitationLanding = InvitationLanding
  { ilInviter :: !Text
  , ilPrice :: !Price
  , ilEvent :: !(Maybe InvitationEvent)
  , ilOriginalSlot :: !(Maybe Text)
  , ilDates :: ![(Text, InvitationEvent)]
  , ilCheckoutAction :: !Text
  , ilMessage :: !(Maybe Text)
  , ilAvailable :: !Bool
  , ilCsrf :: !Text
  }

data InvitationEvent = InvitationEvent
  { ieWhen :: !Text
  , ieVenue :: !Text
  , ieWhere :: !Text
  , ieDescription :: !Text
  }

data InvitationRow = InvitationRow
  { irName :: !Text
  , irTier :: !Text
  , irScope :: !Text
  , irEnabled :: !Bool
  , irPurchases :: !Int
  , irGross :: !Price
  , irFee :: !Price
  , irDetailHref :: !Text
  }

data InvitationSaleRow = InvitationSaleRow
  { isrPurchasedAt :: !Text
  , isrCode :: !Text
  , isrPaymentReference :: !Text
  , isrStatus :: !Text
  , isrPrice :: !Price
  , isrFee :: !Price
  }

data InvitationDetail = InvitationDetail
  { idName :: !Text
  , idTier :: !Text
  , idScope :: !Text
  , idEnabled :: !Bool
  , idShareUrl :: !Text
  , idPngUrl :: !Text
  , idPdfUrl :: !Text
  , idPersonalPngUrl :: !Text
  , idPersonalPdfUrl :: !Text
  , idDisableAction :: !Text
  , idDeleteAction :: !Text
  , idPurchases :: !Int
  , idGross :: !Price
  , idFee :: !Price
  , idCsrf :: !Text
  , idSales :: ![InvitationSaleRow]
  }

-- Deliberately independent of the normal public page shell: no analytics or
-- remote fonts/assets are requested while a bearer URL is in the address bar.
invitationLandingPage :: Theme -> InvitationLanding -> Html
invitationLandingPage theme value = privateDoc ("An invitation from " <> ilInviter value <> " — Okashi School") (themeCss theme <> landingCss) $
  elAttr "main" [("class","invitation")] $
    elAttr "article" [("class","invitation-card")] $
      htmlConcat
        [ elAttr "a" [("class","invitation-brand"),("href","/"),("rel","noreferrer")] (esc "Okashi School")
        , el "h1" (esc ("and " <> ilInviter value <> " invite you to a mochi-making class"))
        , elAttr "p" [("class","invitation-intro")] (esc (maybe "Buy a ticket now and choose your class date later." (\event -> "Learn mochi making at " <> ieVenue event <> ".") (ilEvent value)))
        , case ilEvent value of
            Nothing -> mempty
            Just event -> htmlConcat
              [ elAttr "p" [("class","invitation-event")] (esc (ieWhen event))
              , elAttr "p" [("class","invitation-location")] (esc (ieWhere event))
              , if T.null (ieDescription event) then mempty else elAttr "p" [("class","invitation-description")] (esc (ieDescription event))
              ]
        , elAttr "p" [("class","invitation-price")] (esc (formatPrice (ilPrice value) <> " per person"))
        , maybe mempty (elAttr "p" [("class","invitation-message"),("role","status")] . esc) (ilMessage value)
        , if ilAvailable value
            then htmlConcat
              [ case primaryDate of
                  Just (sid, _) -> checkoutButton (Just sid) ("Book this class for " <> amount)
                  Nothing -> mempty
              , if null otherDates then mempty else elAttr "section" [("class","invitation-dates"),("aria-label","Available class dates")] $
                  el "h2" (esc "Next available dates, same invitation price")
                  <> htmlConcat (map dateCard otherDates)
              , if isNothing (ilOriginalSlot value)
                  then elAttr "section" [("class","invitation-gift")] $
                    el "h2" (esc "Choose the date later")
                    <> el "p" (esc "Keep your ticket or give it to a friend. All tickets are transferable.")
                    <> checkoutButton Nothing ("Buy a transferable ticket for " <> amount)
                  else mempty
              ]
            else el "p" (esc "This invitation is not available for new purchases. Please contact Okashi School if you need help.")
        , elAttr "p" [("class","invitation-note")] (esc (if isNothing (ilOriginalSlot value)
            then "Go straight to secure payment. Your transferable ticket lets you choose a class later. No account needed."
            else "Go straight to secure payment. Payment confirms your place for the selected class. Your ticket is transferable. No account needed."))
        , elAttr "a" [("href","/contact"),("rel","noreferrer")] (esc "Ask us a question")
        ]
  where
    amount = formatPrice (ilPrice value)
    primaryDate = case [choice | choice@(sid, _) <- ilDates value, Just sid == ilOriginalSlot value] of
      choice:_ -> Just choice
      [] -> Nothing
    otherDates = case (ilOriginalSlot value, primaryDate) of
      (Just original, Nothing) -> [choice | choice@(sid, _) <- ilDates value, sid /= original]
      _ -> []
    checkoutButton selected label = elAttr "form" [("method","post"),("action",ilCheckoutAction value)] $
      csrfField (ilCsrf value)
      <> elAttr "button" ([("type","submit"),("class","sww-buy")] <> maybe [] (\sid -> [("name","slot"),("value",sid),("aria-label",label <> maybe "" ((" — " <>) . ieWhen) (lookup sid (ilDates value)))]) selected) (esc label)
    dateCard (sid, event) = elAttr "article" [("class","invitation-date")] $
      el "h3" (esc (ieWhen event))
      <> elAttr "p" [("class","invitation-location")] (esc (ieWhere event))
      <> (if T.null (ieDescription event) then mempty else elAttr "p" [("class","invitation-description")] (esc (ieDescription event)))
      <> checkoutButton (Just sid) ("Book for " <> amount)

invitationsPage :: Text -> [(Text,Text)] -> [InvitationRow] -> Maybe Text -> Html
invitationsPage csrf events rows message = adminDoc "Invitations" $ topbar <> elAttr "main" [("class","wrap")] (htmlConcat
  [ el "h1" (esc "Invitations")
  , el "p" (esc "Create a reusable invitation for a friend, contact, venue or partner. Share its private link, or download a card to send or print.")
  , flash message
  , elAttr "section" [("class","card")] $
      el "h2" (esc "Create an invitation")
      <> elAttr "form" [("method","post"),("action","/admin/invitations")] (htmlConcat
        [ csrfField csrf
        , fieldLabel "invitation-name" "Inviter’s name"
        , voidElAttr "input" [("type","text"),("id","invitation-name"),("name","name"),("maxlength","100"),("required","required"),("autocomplete","off")]
        , elAttr "p" [("class","hint")] (esc "Download a card to share with their friends, or a personal card addressed to them. Both use the same reusable invitation link.")
        , fieldLabel "invitation-tier" "Ticket price"
        , elAttr "select" [("id","invitation-tier"),("name","tier")] (option "friend" "Friends and friends of friends — £65" <> option "partner" "Partner — £75, including a £5 referral fee")
        , fieldLabel "invitation-scope" "What can guests buy?"
        , elAttr "select" [("id","invitation-scope"),("name","scope")] (option "general" "A transferable ticket — choose the date later" <> option "event" "A specific class — alternatives only when full")
        , fieldLabel "invitation-slot" "Class (for a specific-class invitation)"
        , elAttr "select" [("id","invitation-slot"),("name","slot")] (option "" "Choose a class" <> htmlConcat [option value label | (value,label) <- events])
        , elAttr "div" [("class","actions")] (elAttr "button" [("class","primary"),("type","submit")] (esc "Create invitation"))
        ])
  , elAttr "section" [("class","card")] $
      el "h2" (esc "Your invitations")
      <> if null rows then el "p" (esc "No invitations yet.") else table
          ["Inviter", "Ticket", "Status", "Paid purchases", "Gross sales", "Referral fees accrued"]
          [ [ elAttr "a" [("href",irDetailHref row)] (esc (irName row)) <> elAttr "p" [("class","hint")] (esc (irScope row))
            , esc (irTier row), esc (status (irEnabled row)), esc (T.pack (show (irPurchases row)))
            , esc (formatPrice (irGross row)), esc (formatPrice (irFee row))
            ] | row <- rows ]
  , feeNote
  ])

invitationDetailPage :: InvitationDetail -> Maybe Text -> Html
invitationDetailPage value message = adminDoc "Invitation details" $ topbar <> elAttr "main" [("class","wrap")] (htmlConcat
  [ elAttr "a" [("href","/admin/invitations")] (esc "← Invitations")
  , el "h1" (esc (idName value))
  , flash message
  , elAttr "section" [("class","card")] $ htmlConcat
      [ el "p" (esc (idTier value <> "; " <> idScope value <> "; " <> status (idEnabled value)))
      , fieldLabel "invitation-link" "Private invitation link"
      , voidElAttr "input" [("type","text"),("id","invitation-link"),("value",idShareUrl value),("readonly","readonly"),("autocomplete","off")]
      , elAttr "p" [("class","hint")] (esc "Anyone with this link can use the invitation. Share it with your guests; it can be used for more than one purchase.")
      , elAttr "div" [("class","actions")] $ htmlConcat
          [ elAttr "a" [("href",idShareUrl value),("rel","noreferrer"),("target","_blank")] (esc "Open invitation")
          ]
      , el "h2" (esc "Download invitation cards")
      , el "h3" (esc "For sharing with friends")
      , elAttr "p" [("class","hint")] (esc ("“Okashi School and " <> idName value <> " invite you to a mochi-making class.”"))
      , elAttr "div" [("class","actions")] $ htmlConcat
          [ elAttr "a" [("href",idPdfUrl value),("download","")] (esc "Shared card PDF")
          , elAttr "a" [("href",idPngUrl value),("download","")] (esc "Shared card PNG")
          ]
      , el "h3" (esc ("Addressed to " <> idName value))
      , elAttr "p" [("class","hint")] (esc ("“Dear " <> idName value <> ", you are invited to a mochi-making class.”"))
      , elAttr "div" [("class","actions")] $ htmlConcat
          [ elAttr "a" [("href",idPersonalPdfUrl value),("download","")] (esc "Personal card PDF")
          , elAttr "a" [("href",idPersonalPngUrl value),("download","")] (esc "Personal card PNG")
          ]
      , elAttr "form" [("method","post"),("action",idDisableAction value)] $
          csrfField (idCsrf value)
          <> voidElAttr "input" [("type","hidden"),("name","enabled"),("value",if idEnabled value then "no" else "yes")]
          <> elAttr "div" [("class","actions")] (elAttr "button" [("type","submit"),("class","danger")] (esc (if idEnabled value then "Disable this invitation" else "Enable this invitation")))
          <> elAttr "p" [("class","hint")] (esc "Disabling stops new purchases through this link. Previously purchased tickets stay valid.")
      , if idEnabled value then mempty else
          elAttr "form" [("method","post"),("action",idDeleteAction value)] $
            csrfField (idCsrf value)
            <> elAttr "p" [("class","hint")] (esc "Unused invitations can be deleted permanently. Invitations with checkout or purchase history are kept for your records.")
            <> elAttr "div" [("class","actions")] (elAttr "button" [("type","submit"),("class","danger")] (esc "Delete unused invitation permanently"))
      ]
  , elAttr "section" [("class","card")] $
      el "h2" (esc "Sales from this invitation")
      <> el "p" (esc (T.pack (show (idPurchases value)) <> " paid purchases, " <> formatPrice (idGross value) <> " gross sales, " <> formatPrice (idFee value) <> " referral fees accrued"))
      <> if null (idSales value) then el "p" (esc "No paid purchases yet.") else table
          ["Purchased", "Ticket code", "Payment reference", "Status", "Price", "Referral fee"]
          [ map esc [isrPurchasedAt row,isrCode row,isrPaymentReference row,isrStatus row,formatPrice (isrPrice row),formatPrice (isrFee row)] | row <- idSales value ]
  , feeNote
  ])

privateDoc :: Text -> Text -> Html -> Html
privateDoc title css body = rawHtml "<!DOCTYPE html>" <> elAttr "html" [("lang","en")] (
  el "head" (htmlConcat
    [ voidElAttr "meta" [("charset","utf-8")]
    , voidElAttr "meta" [("name","viewport"),("content","width=device-width, initial-scale=1")]
    , voidElAttr "meta" [("name","robots"),("content","noindex, nofollow, noarchive")]
    , voidElAttr "meta" [("name","referrer"),("content","no-referrer")]
    , el "title" (esc title)
    , el "style" (rawHtml css)
    ]) <> el "body" body)

landingCss :: Text
landingCss = T.unlines
  [ "@font-face{font-family:'Cormorant Garamond';src:url('/fonts/okashi-cormorant-garamond-v1.ttf') format('truetype');font-weight:300 700;font-style:normal;font-display:swap}"
  , "@font-face{font-family:Inter;src:url('/fonts/okashi-inter-v1.ttf') format('truetype');font-weight:100 900;font-style:normal;font-display:swap}"
  , "@font-face{font-family:'Noto Serif JP';src:url('/fonts/okashi-noto-serif-jp-v1.ttf') format('truetype');font-weight:200 900;font-style:normal;font-display:swap}"
  , ".invitation{max-width:680px;margin:0 auto;padding:36px 20px;min-height:100vh;display:grid;align-items:center}"
  , ".invitation a{color:var(--sww-accent)}"
  , ".invitation-card{text-align:center;border:1px solid var(--sww-accent);padding:44px 32px;background:var(--sww-bg);overflow-wrap:anywhere}"
  , ".invitation-brand{font:600 clamp(34px,6vw,48px)/1.1 var(--sww-mast);color:var(--sww-fg);text-decoration:none}"
  , ".invitation h1{font:600 clamp(30px,5vw,42px)/1.15 var(--sww-display);margin:8px 0 24px}"
  , ".invitation-intro{font-size:18px;line-height:1.5;margin:0 0 20px}"
  , ".invitation-event{font-weight:600}.invitation-price{font-size:22px;font-weight:600;margin:22px 0}"
  , ".invitation-location{line-height:1.5;margin:10px 0}.invitation-description{font-size:15px;line-height:1.6;margin:18px 0}"
  , ".invitation .sww-buy{width:100%;font-size:16px;min-height:48px;padding:13px 16px}"
  , ".invitation .sww-buy:focus-visible{outline:3px solid var(--sww-fg);outline-offset:3px}.invitation .sww-buy:hover{filter:brightness(.93)}"
  , ".invitation-dates,.invitation-gift{margin-top:30px}.invitation h2{font:600 25px/1.25 var(--sww-display);margin:0 0 18px}"
  , ".invitation-date{border-top:1px solid var(--sww-border);padding:20px 0 4px;margin-top:16px;text-align:left}.invitation-date h3{font:600 18px/1.5 var(--sww-body);margin:0}.invitation-date .invitation-location{font-size:15px;margin:6px 0 14px}.invitation-date .invitation-description{margin:8px 0 16px}.invitation-gift p{line-height:1.5}"
  , ".invitation-note{font-size:14px;line-height:1.6;margin:24px 0}.invitation-message{border:1px solid var(--sww-border);padding:12px}"
  , "@media(max-width:440px){.invitation{padding:18px 14px}.invitation-card{padding:30px 20px}}"
  ]

csrfField :: Text -> Html
csrfField value = voidElAttr "input" [("type","hidden"),("name","csrf"),("value",value)]

fieldLabel :: Text -> Text -> Html
fieldLabel ident = elAttr "label" [("for",ident)] . esc

option :: Text -> Text -> Html
option value = elAttr "option" [("value",value)] . esc

flash :: Maybe Text -> Html
flash = maybe mempty (elAttr "p" [("class","flash"),("role","status")] . esc)

status :: Bool -> Text
status True = "Enabled"
status False = "Disabled"

feeNote :: Html
feeNote = elAttr "p" [("class","muted")] (esc "Referral fees are accrued before refunds or settlements. This page records amounts owed; it does not make payouts.")

table :: [Text] -> [[Html]] -> Html
table headings rows = elAttr "div" [("style","overflow-x:auto")] $ elAttr "table" [("class","plist")] $
  el "thead" (el "tr" (htmlConcat (map (el "th" . esc) headings)))
  <> el "tbody" (htmlConcat [el "tr" (htmlConcat (map (el "td") row)) | row <- rows])
