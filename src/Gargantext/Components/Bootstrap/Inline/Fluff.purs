module Gargantext.Components.Bootstrap.Fluff
  ( fluff
  , fluff'
  , fluff_
  ) where

import Gargantext.Prelude

import Data.Foldable (intercalate)
import Reactix as R
import Reactix.DOM.HTML as H

componentName :: String
componentName = "b-fluff"

-- | Structural Component for a simple Element only serving the purpose to add
-- | some classes in it
-- |
-- | Hence the name: Fluff (uncountable noun): Consists of soft threads or
-- | fibres in the  form of small, light balls or lump [or a set of utility
-- | classes...]
fluff :: Array String -> Array R.Element -> R.Element
fluff classes children = R.createDOMElement "span" cls children
  where
    cls = { className: intercalate " " $
              [ componentName
              ] <> classes
          }

-- | Shorthand for using <fluff> Component without writing its text node
fluff' :: Array String -> String -> R.Element
fluff' classes text = R.createDOMElement "span" cls chd
  where
    cls = { className: intercalate " " $
              [ componentName
              ] <> classes
          }

    chd = [ H.text text ]

-- | Shorthand for using <fluff> Component without any child
fluff_ :: Array String -> R.Element
fluff_ classes = R.createDOMElement "span" cls []
  where
    cls = { className: intercalate " " $
              [ componentName
              ] <> classes
          }
