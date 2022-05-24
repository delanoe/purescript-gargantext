module Gargantext.Components.Bootstrap.ButtonGroup
  ( buttonGroup
  ) where

import Gargantext.Prelude

import Data.Foldable (intercalate)
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

type Props   = ( | Options )
type Options =
  ( className :: String
  , collapse  :: Boolean
  )

options :: Record Options
options =
  { className : ""
  , collapse  : true
  }

-- | Structural Component for the Bootstrap Button Group
-- |
-- | https://getbootstrap.com/docs/4.0/components/button-group/
buttonGroup :: forall r. R2.OptComponent Options Props r
buttonGroup = R2.optComponent component options

componentName :: String
componentName = "b-button-group"

bootstrapName :: String
bootstrapName = "btn-group"

component :: R.Component Props
component = R.hooksComponent componentName cpt where
  cpt props children = do
    -- Computed
    let
      className = intercalate " "
        -- provided custom className
        [ props.className
        -- BEM classNames
        , componentName
        , props.collapse ?
            componentName <> "--collapse" $
            componentName <> "--no-collapse"
        -- Bootstrap specific classNames
        , bootstrapName
        ]

    -- Render
    pure $

      H.div
      { className
      , role: "group"
      }
      children
