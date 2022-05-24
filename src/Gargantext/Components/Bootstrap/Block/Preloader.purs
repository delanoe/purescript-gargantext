module Gargantext.Components.Bootstrap.Preloader(preloader) where

import Gargantext.Prelude

import Data.Foldable (intercalate)
import Gargantext.Components.Bootstrap.Spinner (spinner)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

type Props   = ( | Options)
type Options =
  ( className :: String
  )

options :: Record Options
options =
  { className : ""
  }

-- | Structural Component wrapping our <Spinner.BorderTheme> within
-- | a basic layout
preloader :: forall r. R2.OptLeaf Options Props r
preloader = R2.optLeaf component options

componentName :: String
componentName = "b-preloader"

component :: R.Component Props
component = R.hooksComponent componentName cpt where
  cpt props _ = do
    -- Computed
    let
      className = intercalate " "
        -- provided custom className
        [ props.className
        -- BEM classNames
        , componentName
        ]
    -- Render
    pure $

      H.div
      { className }
      [
        spinner
        { className: componentName <> "__spinner" }
      ]
