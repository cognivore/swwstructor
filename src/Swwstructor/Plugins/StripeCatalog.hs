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
  , parsePriceCatalogJSON
  , catalogSection
  , isPriceId
  ) where

import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Swwstructor.Block (BuyTarget (BuyTarget))
import Swwstructor.Content (Availability (NotOffered, OnSale), ImageRef (ImageRef), Product (Product), SectionSpec (ProductGrid))
import Swwstructor.Money (Cents (Cents), Price (Price), currencyOfCode)
import StickyWM (JSON (JArr, JBool, JNum, JObj, JStr), decode, (.:), (.:?))

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
  , ciSeats :: !Text
  -- ^ the product's @sww_seats@ metadata: how many class places one purchase
  -- occupies at scheduling time — @\"1\"@, @\"2\"@…, or @\"all\"@ (a private
  -- whole-class ticket). Defaults to @\"1\"@ when unset.
  , ciAvailability :: !Availability
  -- ^ the product's @sww_available@ metadata: @\"false\"@ keeps the product
  -- on the page, greyed out and not buyable. Anything else (or unset) is on
  -- sale; retiring a product for good is archiving it in Stripe.
  }
  deriving (Eq, Show)

-- | Parse the body of @GET \/v1\/products?active=true&expand[]=data.default_price@.
-- Items without a usable one-time default price are skipped (a product with
-- no price cannot be bought). Sorted cheapest-first for a tidy grid.
parseCatalogJSON :: String -> Either String [CatalogItem]
parseCatalogJSON body = do
  v <- decode body
  items <- catalogData v
  pure (sortItems (mapMaybe itemOf items))

-- | Stripe's price list excludes inline checkout prices. Project only each
-- active product's current default, preserving the owner-managed catalogue
-- without allowing per-booking products to displace it. Refuse a partial
-- response rather than silently treating an omitted ticket as unavailable.
parsePriceCatalogJSON :: String -> Either String [CatalogItem]
parsePriceCatalogJSON body = do
  v <- decode body
  case v .:? "has_more" of
    Just (JBool False) -> pure ()
    Just (JBool True) -> Left "catalog: price list is incomplete"
    _ -> Left "catalog: price list has no completeness marker"
  items <- catalogData v
  pure (sortItems (mapMaybe priceItemOf items))

catalogData :: JSON -> Either String [JSON]
catalogData v = case v .: "data" of
    Right (JArr xs) -> Right xs
    Right _ -> Left "catalog: 'data' is not an array"
    Left e -> Left e

sortItems :: [CatalogItem] -> [CatalogItem]
sortItems = sortOn (\i -> let Price (Cents n) _ = ciPrice i in n)

priceItemOf :: JSON -> Maybe CatalogItem
priceItemOf priceObj = do
  JBool True <- priceObj .:? "active"
  productObj <- priceObj .:? "product"
  JBool True <- productObj .:? "active"
  JStr pid <- priceObj .:? "id"
  defaultPrice <- productObj .:? "default_price"
  defaultId <- case defaultPrice of
    JStr ident -> Just ident
    obj@JObj{} -> case obj .:? "id" of Just (JStr ident) -> Just ident; _ -> Nothing
    _ -> Nothing
  if defaultId == pid then pricedItemOf productObj priceObj else Nothing

itemOf :: JSON -> Maybe CatalogItem
itemOf v = do
  priceObj <- v .:? "default_price"
  pricedItemOf v priceObj

pricedItemOf :: JSON -> JSON -> Maybe CatalogItem
pricedItemOf v priceObj = do
  JStr name <- v .:? "name"
  -- Only one-time prices belong in a payment-mode checkout; a recurring
  -- (subscription) price would be refused by Stripe at session creation.
  JStr "one_time" <- priceObj .:? "type"
  JStr pid <- priceObj .:? "id"
  JNum amount <- priceObj .:? "unit_amount"
  minor <- checkedMinorUnits amount
  JStr code <- priceObj .:? "currency"
  cur <- currencyOfCode (T.pack code)
  let blurb = case v .:? "description" of
        Just (JStr d) -> T.pack d
        _ -> ""
      img = case v .:? "images" of
        Just (JArr (JStr u : _)) -> Just (T.pack u)
        _ -> Nothing
      meta k = case v .:? "metadata" of
        Just md@JObj{} -> case md .:? k of
          Just (JStr s) | not (null s) -> Just (T.pack s)
          _ -> Nothing
        _ -> Nothing
      seats = fromMaybe "1" (meta "sww_seats")
      availability = case T.toLower . T.strip <$> meta "sww_available" of
        Just "false" -> NotOffered
        _ -> OnSale
  pure
    CatalogItem
      { ciName = T.pack name
      , ciBlurb = blurb
      , ciImage = img
      , ciPriceId = T.pack pid
      , ciPrice = Price (Cents minor) cur
      , ciSeats = seats
      , ciAvailability = availability
      }

checkedMinorUnits :: Double -> Maybe Int
checkedMinorUnits amount
  | isNaN amount || isInfinite amount || amount < 0 || amount > 9007199254740991 = Nothing
  | amount > fromIntegral (maxBound :: Int) = Nothing
  | fromInteger (round amount) /= amount = Nothing
  | otherwise = Just (round amount)

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
        (ciAvailability i)

-- | Does a buy target name a Stripe price (a catalogue purchase) rather than
-- a content-defined product?
isPriceId :: Text -> Bool
isPriceId = ("price_" `T.isPrefixOf`)
