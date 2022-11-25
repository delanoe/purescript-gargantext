module Gargantext.Components.Bootstrap.FormCheckbox (formCheckbox) where

import Gargantext.Prelude

import Data.Foldable (elem, intercalate)
import Effect (Effect)
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Unsafe.Coerce (unsafeCoerce)

type Props =
  ( callback    :: Boolean -> Effect Unit
  , value       :: Boolean
  | Options
  )

type Options =
  ( status      :: ComponentStatus
  , className   :: String
  )

options :: Record Options
options =
  { status      : Enabled
  , className   : ""
  }

-- | Structural Component for an <input type="checkbox">
-- |
-- |    - not using "bootstrap" here, as their checkbox does not have many
-- |      feature (sizing, colour variant), and their use of checkbox/label
-- |      is far too opinionated
formCheckbox :: forall r. R2.OptLeaf Options Props r
formCheckbox = R2.optLeaf component options

componentName :: String
componentName = "b-form-checkbox"

component :: R.Component Props
component = R.hooksComponent componentName cpt where
  cpt props@{ callback
            , status
            } _ = do
    -- Computed
    className <- pure $ intercalate " "
      -- provided custom className
      [ props.className
      -- BEM classNames
      , componentName
      , componentName <> "--" <> show status
      ]

    change <- pure $ onChange status callback
    -- Render
    pure $

      H.input
      { className
      , on: { change }
      , type: "checkbox"
      , disabled: elem status [ Disabled ]
      , readOnly: elem status [ Idled ]
      , value: props.value
      , checked: props.value
      }

-- | * Change event will effectively be triggered according to the
-- | component status props
-- | * Also directly returns the newly input value
-- | (usage not so different from `targetValue` of ReactBasic)
onChange :: forall event.
     ComponentStatus
  -> (Boolean -> Effect Unit)
  -> event
  -> Effect Unit
onChange status callback event = do
  if   status == Enabled
  then callback $ (unsafeCoerce event).target.checked
  else pure unit
