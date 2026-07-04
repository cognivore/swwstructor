{-# LANGUAGE OverloadedStrings #-}

-- | Stripe as the product SOURCE (not just the till): the owner maintains
-- their catalogue in the Stripe dashboard — names, prices, photos — and a
-- @stripeCatalog@ section renders it with zero site-side maintenance. This
-- module is the pure half: parse @GET \/v1\/products?expand[]=data.default_price@
-- and turn the items into the engine's 'ProductGrid' shape (buy targets are
-- Stripe PRICE ids, so checkout charges exactly the catalogue price). The
-- fetch + cache live in the server ("Swwstructor.Server.Catalog").
module Swwstructor.Plugins.StripeCatalog
  ( CatalogItem (..)
  , parseCatalogJSON
  , catalogSection
  , isPriceId
  ) where

import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Swwstructor.Block (BuyTarget (BuyTarget))
import Swwstructor.Content (ImageRef (ImageRef), Product (Product), SectionSpec (ProductGrid))
import Swwstructor.Money (Cents (Cents), Price (Price), currencyOfCode)
import StickyWM (JSON (JArr, JNum, JObj, JStr), decode, (.:), (.:?))

-- | One sellable item as Stripe's catalogue describes it.
data CatalogItem = CatalogItem
  { ciName :: !Text
  , ciBlurb :: !Text
  -- ^ the product description ("" when absent)
  , ciImage :: !(Maybe Text)
  -- ^ the first product image URL, if any
  , ciPriceId :: !Text
  -- ^ the default price id (@price_…@) — the buy target
  , ciPrice :: !Price
  }
  deriving (Eq, Show)

-- | Parse the body of @GET \/v1\/products?active=true&expand[]=data.default_price@.
-- Items without a usable one-time default price are skipped (a product with
-- no price cannot be bought). Sorted cheapest-first for a tidy grid.
parseCatalogJSON :: String -> Either String [CatalogItem]
parseCatalogJSON body = do
  v <- decode body
  items <- case v .: "data" of
    Right (JArr xs) -> Right xs
    Right _ -> Left "catalog: 'data' is not an array"
    Left e -> Left e
  pure (sortOn (\i -> let Price (Cents n) _ = ciPrice i in n) (mapMaybe itemOf items))

itemOf :: JSON -> Maybe CatalogItem
itemOf v = do
  JStr name <- v .:? "name"
  priceObj <- v .:? "default_price"
  -- Only one-time prices belong in a payment-mode checkout; a recurring
  -- (subscription) price would be refused by Stripe at session creation.
  JStr "one_time" <- priceObj .:? "type"
  JStr pid <- priceObj .:? "id"
  JNum amount <- priceObj .:? "unit_amount"
  JStr code <- priceObj .:? "currency"
  cur <- currencyOfCode (T.pack code)
  let blurb = case v .:? "description" of
        Just (JStr d) -> T.pack d
        _ -> ""
      img = case v .:? "images" of
        Just (JArr (JStr u : _)) -> Just (T.pack u)
        _ -> Nothing
  pure
    CatalogItem
      { ciName = T.pack name
      , ciBlurb = blurb
      , ciImage = img
      , ciPriceId = T.pack pid
      , ciPrice = Price (Cents (max 0 (round amount))) cur
      }

-- | The catalogue as an engine section: a 'ProductGrid' whose buy targets are
-- the Stripe price ids and whose photos are the Stripe product images — the
-- single source of truth, rendered N-wide by the engine (3 items = 3 columns).
catalogSection :: Maybe Text -> Maybe Text -> [CatalogItem] -> SectionSpec
catalogSection title intro items =
  ProductGrid title intro (map productOf items)
  where
    productOf i =
      Product
        (ciName i)
        (ciBlurb i)
        (ciPrice i)
        (Just (ImageRef (ciImage i) (ciName i) 1.25 300))
        (BuyTarget (ciPriceId i))

-- | Does a buy target name a Stripe price (a catalogue purchase) rather than
-- a content-defined product?
isPriceId :: Text -> Bool
isPriceId = ("price_" `T.isPrefixOf`)
