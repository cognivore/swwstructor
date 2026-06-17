{-# LANGUAGE OverloadedStrings #-}

-- | Stripe as a section provider + the server-side buy registry (CONSTRUCTOR
-- §3.6 / C1). 'checkoutSection' turns priced products into a storefront grid
-- whose buy buttons post to @\/buy\/<target>@; 'siteBuyRegistry' folds every
-- product across the whole site into @target -> LineItem@, so the checkout
-- amount is ALWAYS resolved server-side from the owner's content and never taken
-- from the client. Pure — the live Stripe API call lives in the server.
module Swwstructor.Plugins.Stripe
  ( siteBuyRegistry
  , productLineItem
  , checkoutSection
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Swwstructor.Block (BuyTarget (BuyTarget))
import Swwstructor.Checkout (LineItem, lineItem)
import Swwstructor.Content (Product (prodBuy, prodName, prodPrice), SectionSpec (ProductGrid), SiteSpec, pricedItems)

-- | The server-side buy registry: buy-target id → priced line item, folded over
-- every product on the site. The single source of truth for checkout amounts.
siteBuyRegistry :: SiteSpec -> Map Text LineItem
siteBuyRegistry site =
  M.fromList [(tgt, lineItem name pr) | (BuyTarget tgt, name, pr) <- pricedItems site]

-- | One product's @(target, line item)@ registry entry.
productLineItem :: Product -> (Text, LineItem)
productLineItem p =
  let BuyTarget t = prodBuy p
   in (t, lineItem (prodName p) (prodPrice p))

-- | A titled storefront section from products. Each buy button drives a hosted
-- Stripe Checkout session via the server's @\/buy\/:id@ route.
checkoutSection :: Maybe Text -> [Product] -> SectionSpec
checkoutSection title = ProductGrid title Nothing
