module Gargantext.Components.Bootstrap.ProgressBar(progressBar) where

import Gargantext.Prelude

import Data.Foldable (intercalate)
import Gargantext.Components.Bootstrap.Types (Variant(..))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

type Props   =
  ( value     :: Number
  , waitingTextClass :: String
  | Options
  )

type Options =
  ( className :: String
  , variant   :: Variant
  )

options :: Record Options
options =
  { className : ""
  , variant   : Primary
  }

-- | Structural Component for the Bootsrap "Progress Bar"
-- |
-- | https://getbootstrap.com/docs/4.6/components/progress/
progressBar :: forall r. R2.OptLeaf Options Props r
progressBar = R2.optLeaf component options

componentName :: String
componentName = "b-progress-bar"

bootstrapName :: String
bootstrapName = "progress"

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
        -- Bootstrap specific classNames
        , bootstrapName
        ]
    -- Render
    pure $

      H.div
      { className }
      [
        H.div
        { className: intercalate " "
            [ "progress-bar"
            , "bg-" <> show props.variant
            ]
        , style: { width: (show props.value) <> "%" }
        , role: "progress-bar"
        , "aria-valuenow": show $ props.value
        , "aria-valuemin": "0"
        , "aria-valuemax": "100"
        }
        []
      ,
        H.div
        { className: intercalate " "
          [ "progress-text" 
          , props.waitingTextClass
          ]
        } 
        [ H.text "Waiting task..." ]
      ]
