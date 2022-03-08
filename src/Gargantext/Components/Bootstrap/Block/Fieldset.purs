module Gargantext.Components.Bootstrap.Fieldset
  ( fieldset
  ) where

import Gargantext.Prelude

import Data.Array (intercalate)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

type Props =
  ( titleSlot :: R.Element
  | Options
  )

type Options =
  ( className :: String
  )

options :: Record Options
options =
  { className: ""
  }

-- | Component simulating a native <fieldset>
-- | (which has been completly reset by Bootstrap libraries)
fieldset :: forall r. R2.OptComponent Options Props r
fieldset = R2.optComponent component options

componentName :: String
componentName = "b-fieldset"

component :: R.Component Props
component = R.hooksComponent componentName cpt where
  cpt props@{ titleSlot
            } children = do
    -- Computed
    className <- pure $ intercalate " "
      -- provided custom className
      [ props.className
      -- BEM classNames
      , componentName
      ]
    -- Render
    pure $

      H.section
      { className }
      [
        H.div
        { className: componentName <> "__legend" }
        [ titleSlot ]
      ,
        H.div
        { className: componentName <> "__content" }
        children
      ]
