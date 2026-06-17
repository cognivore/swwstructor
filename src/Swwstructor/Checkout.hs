{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The checkout capability — the pure, testable core of Stripe integration,
-- ported from @Okashi.Checkout@. The /effect/ is a typeclass 'MonadCheckout'
-- with two interpreters: 'MockCheckoutT' (here; no network, for tests) and the
-- real @StripeCheckoutT@ (in the server executable, where @http-client@ lives).
--
-- The model is Stripe-hosted Checkout: we build a typed 'CheckoutRequest' with
-- inline @price_data@ (currency, name, @unit_amount@ in /minor units/, quantity)
-- and create a session; Stripe returns a hosted URL we 303-redirect to. The
-- amount is server-resolved, never client-supplied. 'stripeFormParams' — the
-- exact wire encoding — is pure, so a test asserts @mode=payment@, the right
-- @unit_amount@ and currency without touching the network.
module Swwstructor.Checkout
  ( Url (..)
  , LineItem (..)
  , lineItem
  , CheckoutRequest (..)
  , CheckoutSession (..)
  , CheckoutError (..)
  , MonadCheckout (..)
  , stripeFormParams
  , encodeForm
  , MockCheckoutT (..)
  , runMockCheckoutT
  ) where

import Control.Monad.Reader (ReaderT (..))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, ord)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex)
import Swwstructor.Money
  ( Cents (Cents)
  , Price (Price)
  , Quantity (Quantity)
  , currencyCode
  )

-- | A URL (Stripe's hosted session URL, our success/cancel URLs). A strong
-- newtype, never a bare 'Text'.
newtype Url = Url Text
  deriving (Eq, Show)

-- | One purchasable line: a display name, a unit price, and a quantity.
data LineItem = LineItem
  { liName :: !Text
  , liUnitPrice :: !Price
  , liQty :: !Quantity
  }
  deriving (Eq, Show)

-- | A line item of quantity one.
lineItem :: Text -> Price -> LineItem
lineItem name p = LineItem name p (Quantity 1)

-- | A request to create a Checkout session.
data CheckoutRequest = CheckoutRequest
  { crItems :: !(NonEmpty LineItem)
  , crSuccessUrl :: !Url
  , crCancelUrl :: !Url
  }
  deriving (Eq, Show)

-- | The created session — just the URL we redirect the browser to.
newtype CheckoutSession = CheckoutSession {csUrl :: Url}
  deriving (Eq, Show)

-- | Why a checkout could not be created (exhaustive).
data CheckoutError
  = CheckoutTransport !Text
  | CheckoutHttpStatus !Int !Text
  | CheckoutBadResponse !Text
  deriving (Eq, Show)

-- | The checkout effect. One method; many interpreters.
class (Monad m) => MonadCheckout m where
  createCheckout :: CheckoutRequest -> m (Either CheckoutError CheckoutSession)

-- ---------------------------------------------------------------------------
-- The Stripe wire encoding (pure)
-- ---------------------------------------------------------------------------

-- | The @application/x-www-form-urlencoded@ parameters for
-- @POST \/v1\/checkout\/sessions@. Pure, so it is unit-tested directly: line
-- items become @line_items[i][price_data]...@, amounts are integer minor units.
stripeFormParams :: CheckoutRequest -> [(Text, Text)]
stripeFormParams req =
  [ ("mode", "payment")
  , ("success_url", urlText (crSuccessUrl req))
  , ("cancel_url", urlText (crCancelUrl req))
  ]
    <> concat (zipWith lineItemParams [0 :: Int ..] (toList (crItems req)))
  where
    urlText (Url u) = u
    lineItemParams i li =
      let ix t = "line_items[" <> T.pack (show i) <> "]" <> t
          Price (Cents amount) cur = liUnitPrice li
          Quantity q = liQty li
       in [ (ix "[price_data][currency]", currencyCode cur)
          , (ix "[price_data][product_data][name]", liName li)
          , (ix "[price_data][unit_amount]", T.pack (show amount))
          , (ix "[quantity]", T.pack (show q))
          ]

-- | Percent-encode a list of params into a form body.
encodeForm :: [(Text, Text)] -> Text
encodeForm = T.intercalate "&" . map one
  where
    one (k, v) = urlEncode k <> "=" <> urlEncode v

-- | RFC-3986 application/x-www-form-urlencoded percent-encoding (spaces as
-- @+@), pure and total.
urlEncode :: Text -> Text
urlEncode = T.concatMap enc
  where
    enc ' ' = "+"
    enc c
      | unreserved c = T.singleton c
      | otherwise = T.concat [pct b | b <- utf8Bytes c]
    unreserved c =
      isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("-_.~" :: String)
    pct b = "%" <> pad2 (T.toUpper (T.pack (showHex b "")))
    pad2 t = if T.length t == 1 then "0" <> t else t

-- | UTF-8 byte expansion of a character (so non-ASCII names encode correctly).
utf8Bytes :: Char -> [Int]
utf8Bytes c =
  let n = ord c
   in if
        | n < 0x80 -> [n]
        | n < 0x800 -> [0xC0 + (n `div` 0x40), 0x80 + (n `mod` 0x40)]
        | n < 0x10000 ->
            [ 0xE0 + (n `div` 0x1000)
            , 0x80 + ((n `div` 0x40) `mod` 0x40)
            , 0x80 + (n `mod` 0x40)
            ]
        | otherwise ->
            [ 0xF0 + (n `div` 0x40000)
            , 0x80 + ((n `div` 0x1000) `mod` 0x40)
            , 0x80 + ((n `div` 0x40) `mod` 0x40)
            , 0x80 + (n `mod` 0x40)
            ]

-- ---------------------------------------------------------------------------
-- Mock interpreter (no network)
-- ---------------------------------------------------------------------------

-- | A checkout interpreter that never touches the network — it returns a
-- deterministic fake session URL. Lets the whole flow be tested without keys.
newtype MockCheckoutT m a = MockCheckoutT {unMockCheckoutT :: ReaderT () m a}
  deriving (Functor, Applicative, Monad)

runMockCheckoutT :: MockCheckoutT m a -> m a
runMockCheckoutT m = runReaderT (unMockCheckoutT m) ()

instance (Monad m) => MonadCheckout (MockCheckoutT m) where
  createCheckout req =
    pure . Right . CheckoutSession . Url $
      "https://checkout.stripe.test/session?items="
        <> T.pack (show (length (toList (crItems req))))
