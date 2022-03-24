module Gargantext.Components.Bootstrap.IconButton (iconButton) where

import Gargantext.Prelude

import Data.Foldable (elem, intercalate)
import Effect (Effect)
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..), Variant(..))
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import React.SyntheticEvent as SE
import Reactix as R
import Reactix.DOM.HTML as H

type Props =
  ( name      :: String
  , callback  :: Unit -> Effect Unit
  | Options
  )

type Options =
  ( className :: String
  , status    :: ComponentStatus
  , title     :: String
  , overlay   :: Boolean
  , variant   :: Variant
  )

options :: Record Options
options =
  { className : ""
  , status    : Enabled
  , title     : ""
  , overlay   : false
  , variant   : Dark
  }

-- | Structural Component for a simple Glyphicon element with call-to-action
-- |
-- | https://forkaweso.me/Fork-Awesome/icons/
iconButton :: forall r. R2.OptLeaf Options Props r
iconButton = R2.optLeaf component options

componentName :: String
componentName = "b-icon-button"

bootstrapName :: String
bootstrapName = "fa"

component :: R.Component Props
component = R.hooksComponent componentName cpt where
  cpt props@{ callback
            , status
            , name } _ = do
    -- Computed
    let
      wrapperClassName = intercalate " "
        -- provided custom className
        [ props.className
        -- BEM classNames
        , componentName
        , componentName <> "--" <> show status
        , componentName <> "--" <> show props.variant
        , props.overlay ?
            componentName <> "--overlay" $
            ""
        ]

      contentClassName = intercalate " "
        -- Bootstrap specific classNames
        [ bootstrapName
        , bootstrapName <> "-" <> name
        ]
    -- Behaviors
    let
      click = onClick status callback
    -- Render
    pure $

      H.span
      { className: wrapperClassName
      , on: { click }
      , disabled: elem status [ Disabled, Deferred ]
      }
      [
        H.i
        { title: props.title
        , className: contentClassName
        }
        []
      ]

-- | Clicked event will effectively be triggered according to the
-- | component status props
onClick :: forall event.
     ComponentStatus
  -> (Unit -> Effect Unit)
  -> SE.SyntheticEvent_ event
  -> Effect Unit
onClick status callback event = do
  SE.preventDefault event
  if   status == Enabled
  then callback unit
  else pure unit
