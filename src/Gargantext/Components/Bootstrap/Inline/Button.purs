module Gargantext.Components.Bootstrap.Button (button) where

import Gargantext.Prelude

import Data.Array (elem)
import Data.Foldable (intercalate)
import Effect (Effect)
import Gargantext.Components.Bootstrap.Spinner (spinner)
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..))
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import React.SyntheticEvent as SE
import Reactix as R
import Reactix.DOM.HTML as H

type Props =
  ( callback  :: Unit -> Effect Unit
  | Options
  )

type Options =
  ( status    :: ComponentStatus
  , size      :: String
  , variant   :: String
  , type      :: String
  , className :: String
  , block     :: Boolean
  )

options :: Record Options
options =
  { status    : Enabled
  , size      : "md"
  , variant   : "primary"
  , type      : "button"
  , className : ""
  , block     : false
  }

-- | Structural Component for the Bootstrap button
-- |
-- |    * size: `"md" (default) | "sm" | "lg"`
-- |
-- | https://getbootstrap.com/docs/4.0/components/buttons/
button :: forall r. R2.OptComponent Options Props r
button = R2.optComponent component options

componentName :: String
componentName = "b-button"

bootstrapName :: String
bootstrapName = "btn"

component :: R.Component Props
component = R.hooksComponent componentName cpt where
  cpt props@{ callback
            , status
            } children = do
    -- Computed
    className <- pure $ intercalate " "
      -- provided custom className
      [ props.className
      -- BEM classNames
      , componentName
      , componentName <> "--" <> show status
      -- Bootstrap specific classNames
      , bootstrapName
      , bootstrapName <> "-" <> props.variant
      , bootstrapName <> "-" <> props.size
      , props.block == true ?
          bootstrapName <> "-block" $
          mempty
      ]
    -- @click
    click <- pure $ \event -> onClick status callback event
    -- Render
    pure $

      H.button
      { className
      , on: { click }
      , disabled: elem status [ Disabled, Deferred ]
      , type: props.type
      }

      [ R2.if' (status == Deferred) $
          spinner
          { className: componentName <> "__spinner"
          }

      , H.span
        { className: componentName <> "__inner"
        }
        children
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
