{-# LANGUAGE OverloadedStrings #-}

-- | A minimal, safe HTML fragment type — ported from @Okashi.Html@ (the
-- reference app) and kept identical in spirit: the only ways to put /content/
-- into an 'Html' are 'esc' (HTML-escapes text) and 'rawHtml' (an explicit,
-- audited escape hatch for trusted markup — our own tags and CSS). All
-- owner-/content-derived 'Text' goes through 'esc', which is what keeps the
-- constructor's renderer injection-safe without a templating dependency.
--
-- The 'Monoid' instance is fragment concatenation, so building a page is '<>'.
module Swwstructor.Html
  ( Html (..)
  , renderHtml
  , esc
  , escAttr
  , rawHtml
  , htmlConcat
  , el
  , elAttr
  , voidElAttr
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B

-- | A safe HTML fragment. Construct only via the helpers in this module.
newtype Html = Html {unHtml :: Builder}

instance Semigroup Html where
  Html a <> Html b = Html (a <> b)

instance Monoid Html where
  mempty = Html mempty

-- | Realise the fragment to strict 'Text' (what the server hands to scotty).
renderHtml :: Html -> Text
renderHtml = TL.toStrict . B.toLazyText . unHtml

-- | Escape text destined for element content. Neutralises @<@, @>@, @&@.
esc :: Text -> Html
esc = Html . B.fromText . escapeBody

-- | Escape text destined for a double-quoted attribute value.
escAttr :: Text -> Html
escAttr = Html . B.fromText . escapeAttr

-- | Trusted-markup escape hatch — used only with literal markup in this code
-- base (our element tags and stylesheet text), never with owner content.
rawHtml :: Text -> Html
rawHtml = Html . B.fromText

-- | Concatenate a list of fragments.
htmlConcat :: [Html] -> Html
htmlConcat = mconcat

-- | An element with no attributes: @\<tag>children\</tag>@.
el :: Text -> Html -> Html
el tag = elAttr tag []

-- | An element with attributes. Attribute values are escaped via 'escAttr'.
elAttr :: Text -> [(Text, Text)] -> Html -> Html
elAttr tag attrs children =
  rawHtml ("<" <> tag) <> renderAttrs attrs <> rawHtml ">" <> children <> rawHtml ("</" <> tag <> ">")

-- | A void element (no closing tag), e.g. @\<link ...>@, @\<meta ...>@.
voidElAttr :: Text -> [(Text, Text)] -> Html
voidElAttr tag attrs = rawHtml ("<" <> tag) <> renderAttrs attrs <> rawHtml ">"

renderAttrs :: [(Text, Text)] -> Html
renderAttrs = htmlConcat . map one
  where
    one (k, v) = rawHtml (" " <> k <> "=\"") <> escAttr v <> rawHtml "\""

-- NB. composition applies right-to-left, so the @&@ pass (rightmost) runs first;
-- the later passes introduce @&@ sequences that must not be re-escaped.
escapeBody :: Text -> Text
escapeBody =
  T.replace ">" "&gt;"
    . T.replace "<" "&lt;"
    . T.replace "&" "&amp;"

escapeAttr :: Text -> Text
escapeAttr =
  T.replace "\"" "&quot;"
    . T.replace ">" "&gt;"
    . T.replace "<" "&lt;"
    . T.replace "&" "&amp;"
