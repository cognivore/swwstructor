{-# LANGUAGE OverloadedStrings #-}

-- | Money — strong types for currency and price, ported and lightly generalised
-- from @Okashi.Money@. No bare 'Int' for money: amounts are 'Cents', a price is
-- a 'Cents' tagged with a 'Currency'. This is the type the Stripe checkout
-- layer consumes (Stripe wants integer minor units), and the type the
-- @priceTag@ block renders.
module Swwstructor.Money
  ( Cents (..)
  , centsInt
  , Quantity (..)
  , quantityInt
  , Currency (..)
  , currencyCode
  , currencyOfCode
  , currencySymbol
  , Price (..)
  , price
  , cents
  , formatPrice
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | An integer count of minor currency units (e.g. euro cents). Stripe's
-- @unit_amount@ is exactly this.
newtype Cents = Cents Int
  deriving (Eq, Ord, Show)

centsInt :: Cents -> Int
centsInt (Cents c) = c

-- | A line-item quantity.
newtype Quantity = Quantity Int
  deriving (Eq, Ord, Show)

quantityInt :: Quantity -> Int
quantityInt (Quantity q) = q

-- | The supported settlement currencies. Closed sum so 'currencyCode' /
-- 'currencySymbol' are total.
data Currency = EUR | GBP | USD | JPY
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The lower-case ISO code Stripe expects (@"eur"@, @"gbp"@, …).
currencyCode :: Currency -> Text
currencyCode EUR = "eur"
currencyCode GBP = "gbp"
currencyCode USD = "usd"
currencyCode JPY = "jpy"

-- | Inverse of 'currencyCode' (case-insensitive); 'Nothing' for unknown codes.
currencyOfCode :: Text -> Maybe Currency
currencyOfCode t = case T.toLower t of
  "eur" -> Just EUR
  "gbp" -> Just GBP
  "usd" -> Just USD
  "jpy" -> Just JPY
  _ -> Nothing

-- | The human-facing currency symbol.
currencySymbol :: Currency -> Text
currencySymbol EUR = "\x20AC" -- €
currencySymbol GBP = "\xA3" -- £
currencySymbol USD = "$"
currencySymbol JPY = "\xA5" -- ¥

-- | A price: a 'Cents' amount in a 'Currency'.
data Price = Price
  { priceAmount :: !Cents
  , priceCurrency :: !Currency
  }
  deriving (Eq, Show)

-- | Build a price from a /major/ unit (euros, pounds, dollars) — multiplies by
-- 100. For zero-decimal currencies (JPY) the caller should use 'cents'.
price :: Currency -> Int -> Price
price cur major = Price (Cents (major * 100)) cur

-- | Build a price from raw minor units.
cents :: Currency -> Int -> Price
cents cur n = Price (Cents n) cur

-- | Render a price for display: @€85@, @£12.50@, @¥600@. Zero-decimal
-- currencies never show a fractional part.
formatPrice :: Price -> Text
formatPrice (Price (Cents c) cur)
  | isZeroDecimal cur = currencySymbol cur <> T.pack (show c)
  | otherwise =
      let major = c `div` 100
          minor = c `mod` 100
          body
            | minor == 0 = T.pack (show major)
            | otherwise = T.pack (show major) <> "." <> pad2 minor
       in currencySymbol cur <> body
  where
    isZeroDecimal JPY = True
    isZeroDecimal _ = False
    pad2 n
      | n < 10 = "0" <> T.pack (show n)
      | otherwise = T.pack (show n)
