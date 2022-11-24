module Gargantext.Components.Bootstrap.Button (button) where

import Gargantext.Prelude

import DOM.Simple.Event as DE
import Data.Array (elem)
import Data.Foldable (intercalate)
import Effect (Effect)
import Gargantext.Components.Bootstrap.Spinner (spinner)
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), ComponentStatus(..), Sizing(..), Variant(..))
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Reactix.SyntheticEvent as RE

type Props =
  ( callback  :: Unit -> Effect Unit
  | Options
  )

type Options =
  ( block     :: Boolean
  , className :: String
  , size      :: Sizing
  , status    :: ComponentStatus
  , title     :: String
  , type      :: String
  , variant   :: ButtonVariant
  )

options :: Record Options
options =
  { block     : false
  , className : ""
  , status    : Enabled
  , size      : MediumSize
  , title     : ""
  , type      : "button"
  , variant   : ButtonVariant Primary
  }

-- | Structural Component for the Bootstrap button
-- |
-- | https://getbootstrap.com/docs/4.6/components/buttons/
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
    let
      className = intercalate " "
        -- provided custom className
        [ props.className
        -- BEM classNames
        , componentName
        , componentName <> "--" <> show status
        -- Bootstrap specific classNames
        , bootstrapName
        , bootstrapName <> "-" <> show props.variant
        , bootstrapName <> "-" <> show props.size
        , props.block == true ?
            bootstrapName <> "-block" $
            mempty
        ]

      click = onClick status callback

    -- Render
    pure $

      H.button
      { className
      , on: { click }
      , disabled: elem status [ Disabled, Deferred ]
      , type: props.type
      , title: props.title
      }
      [
        R2.when (status == Deferred) $
          spinner
          { className: componentName <> "__spinner" }
      ,
        H.span
        { className: componentName <> "__inner" }
        children
      ]

-- | Clicked event will effectively be triggered according to the
-- | component status props
onClick ::
     ComponentStatus
  -> (Unit -> Effect Unit)
  -> RE.SyntheticEvent DE.Event
  -> Effect Unit
onClick status callback event = do
  RE.preventDefault event
  when (status == Enabled) $ callback unit
