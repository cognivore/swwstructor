{-# LANGUAGE OverloadedStrings #-}

-- | Shopify as a /section provider/ (the plugin pattern from CONSTRUCTOR §3.6):
-- a plugin is @cfg -> [Section]@; here, injected Shopify products become a
-- responsive 'ProductGrid'. This module is pure — it maps the Storefront API's
-- product shape onto the constructor's 'Product'. The /live/ Storefront fetch
-- (and the Admin token, which must never reach the browser) lives in the server;
-- offline/CI uses injected products, so the grid always solves deterministically.
module Swwstructor.Plugins.Shopify
  ( ShopifyMoney (..)
  , ShopifyImage (..)
  , ShopifyProduct (..)
  , shopifyToProduct
  , shopifyProducts
  , shopifySection
  , parseMinorUnits
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Swwstructor.Block (BuyTarget (BuyTarget))
import Swwstructor.Content (ImageRef (ImageRef), Product (..), SectionSpec (ProductGrid))
import Swwstructor.Money (Cents (Cents), Currency (USD), Price (Price), currencyOfCode)

-- | Storefront @minVariantPrice@: a decimal amount string + an ISO code.
data ShopifyMoney = ShopifyMoney
  { smAmount :: !Text
  , smCurrency :: !Text
  }
  deriving (Eq, Show)

-- | Storefront @featuredImage@.
data ShopifyImage = ShopifyImage
  { siUrl :: !Text
  , siWidth :: !Int
  , siHeight :: !Int
  }
  deriving (Eq, Show)

-- | A Storefront product node (the fields the catalogue query returns).
data ShopifyProduct = ShopifyProduct
  { spHandle :: !Text
  , spTitle :: !Text
  , spImage :: !(Maybe ShopifyImage)
  , spPrice :: !ShopifyMoney
  }
  deriving (Eq, Show)

-- | Parse a decimal money string (e.g. @"24.00"@, @"1499.5"@, @"600"@) into
-- integer minor units (cents). Total: anything unparseable yields 0.
parseMinorUnits :: Text -> Int
parseMinorUnits t =
  case T.splitOn "." (T.strip t) of
    [whole] -> 100 * readIntDef whole
    (whole : frac : _) ->
      let cents2 = T.take 2 (frac <> "00")
       in 100 * readIntDef whole + readIntDef cents2
    [] -> 0
  where
    readIntDef s = fromMaybe 0 (readMaybeInt (T.unpack (T.filter (/= ',') s)))
    readMaybeInt s = case reads s of [(n, "")] -> Just n; _ -> Nothing

-- | Map one Shopify product to a constructor 'Product'. The buy target is the
-- product handle; the figure aspect comes from the image dimensions (capped at
-- 360px so it never dominates its column).
shopifyToProduct :: ShopifyProduct -> Product
shopifyToProduct sp =
  Product
    { prodName = spTitle sp
    , prodBlurb = ""
    , prodPrice = Price (Cents (parseMinorUnits (smAmount (spPrice sp)))) cur
    , prodImage = imgOf <$> spImage sp
    , prodBuy = BuyTarget (spHandle sp)
    }
  where
    cur :: Currency
    cur = fromMaybe USD (currencyOfCode (smCurrency (spPrice sp)))
    imgOf (ShopifyImage url w h) =
      ImageRef (Just url) (spTitle sp) (aspectOf w h) 360
    aspectOf w h
      | h <= 0 = 1.0
      | otherwise = fromIntegral w / fromIntegral h

shopifyProducts :: [ShopifyProduct] -> [Product]
shopifyProducts = map shopifyToProduct

-- | The plugin output: a titled product grid section (re-homes to one column on
-- a phone for free, because it is a @splitN@).
shopifySection :: Maybe Text -> [ShopifyProduct] -> SectionSpec
shopifySection title = ProductGrid title Nothing . shopifyProducts
