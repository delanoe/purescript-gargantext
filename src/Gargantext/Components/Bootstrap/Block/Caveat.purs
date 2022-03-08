module Gargantext.Components.Bootstrap.Caveat(caveat) where

import Gargantext.Prelude

import Data.Foldable (intercalate)
import Gargantext.Components.Bootstrap.Types (Variant(..))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

type Props   = ( | Options )
type Options =
  ( className :: String
  , variant   :: Variant
  )

options :: Record Options
options =
  { className : ""
  , variant   : Light
  }

-- | Smart reference to the <alert> Bootstrap component,
-- | trimming every features regarding the alert system
-- |
-- | https://getbootstrap.com/docs/4.6/components/alerts/
caveat :: forall r. R2.OptComponent Options Props r
caveat = R2.optComponent component options

componentName :: String
componentName = "b-caveat"

bootstrapName :: String
bootstrapName = "alert"

component :: R.Component Props
component = R.hooksComponent componentName cpt where
  cpt props children = do
    -- Computed
    className <- pure $ intercalate " "
      -- provided custom className
      [ props.className
      -- BEM classNames
      , componentName
      -- Bootstrap specific classNames
      , bootstrapName
      , bootstrapName <> "-" <> show props.variant
      ]
    -- Render
    pure $

      H.div
      { className }
      children
