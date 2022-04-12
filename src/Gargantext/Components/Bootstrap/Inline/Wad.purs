module Gargantext.Components.Bootstrap.Wad
  ( wad
  , wad'
  , wad_
  ) where

import Gargantext.Prelude

import Data.Foldable (intercalate)
import Reactix as R
import Reactix.DOM.HTML as H

componentName :: String
componentName = "b-wad"

-- | Structural Component for a simple Element only serving the purpose to add
-- | some classes in it
-- |
-- | Hence the name: Wad (noun): a small mass, lump, or ball of anything ;
-- | a roll of something
wad :: Array String -> Array R.Element -> R.Element
wad classes children = R.createDOMElement "div" cls children
  where
    cls = { className: intercalate " " $
              [ componentName
              ] <> classes
          }

-- | Shorthand for using <wad> Component without writing its text node
wad' :: Array String -> String -> R.Element
wad' classes text = R.createDOMElement "div" cls chd
  where
    cls = { className: intercalate " " $
              [ componentName
              ] <> classes
          }

    chd = [ H.text text ]

-- | Shorthand for using <wad> Component without any child
wad_ :: Array String -> R.Element
wad_ classes = R.createDOMElement "div" cls []
  where
    cls = { className: intercalate " " $
              [ componentName
              ] <> classes
          }
