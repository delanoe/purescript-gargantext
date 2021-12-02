module Gargantext.Components.Bootstrap.Spinner(spinner) where

import Gargantext.Prelude

import Data.Foldable (intercalate)
import Gargantext.Components.Bootstrap.Types (SpinnerTheme(..))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

type Props   = ( | Options)
type Options =
  ( theme     :: SpinnerTheme
  , className :: String
  )

options :: Record Options
options =
  { theme     : BorderTheme
  , className : ""
  }

-- | Structural Component for the Bootstrap spinner
-- |
-- | https://getbootstrap.com/docs/4.4/components/spinners/
spinner :: forall r. R2.OptLeaf Options Props r
spinner = R2.optLeaf component options

componentName :: String
componentName = "b-spinner"

bootstrapName :: String
bootstrapName = "spinner"

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
      , bootstrapName <> "-" <> show props.theme
      ]
    -- Render
    pure $

      H.div
      { className }
      []
