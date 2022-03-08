module Gargantext.Components.Bootstrap.Icon (icon) where

import Gargantext.Prelude

import Data.Foldable (intercalate)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

type Props =
  ( name  :: String
  | Options
  )

type Options =
  ( className :: String
  )

options :: Record Options
options =
  { className : ""
  }

-- | Structural Component for a simple Glyphicon element
-- |
-- | https://forkaweso.me/Fork-Awesome/icons/
icon :: forall r. R2.OptLeaf Options Props r
icon = R2.optLeaf component options

componentName :: String
componentName = "b-icon"

bootstrapName :: String
bootstrapName = "fa"

component :: R.Component Props
component = R.hooksComponent componentName cpt where
  cpt props _ = do
    -- Computed
    className <- pure $ intercalate " "
      -- provided custom className
      [ props.className
      -- BEM classNames
      , componentName
      -- Bootstrap specific classNames
      , bootstrapName
      , bootstrapName <> "-" <> props.name
      ]
    -- Render
    pure $

      H.i
      { className }
      []
