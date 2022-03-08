module Gargantext.Components.Bootstrap.FormSelect (formSelect) where

import Gargantext.Prelude

import Data.Foldable (elem, intercalate)
import Effect (Effect)
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..), Sizing(..))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Unsafe.Coerce (unsafeCoerce)

type Props =
  ( callback    :: String -> Effect Unit
  , value       :: String
  | Options
  )

type Options =
  ( status      :: ComponentStatus
  , className   :: String
  , type        :: String
  , placeholder :: String
  , size        :: Sizing
  )

options :: Record Options
options =
  { status      : Enabled
  , className   : ""
  , type        : "text"
  , placeholder : ""
  , size        : MediumSize
  }

-- | Structural Component for the Bootstrap select
-- |
-- |  ```purescript
-- |  formSelect { callback, value }
-- |  [
-- |    H.option
-- |    { value: "foo" }
-- |    [ H.text "foo" ]
-- |  ,
-- |    H.option
-- |    { value: "bar" }
-- |    [ H.text "bar" ]
-- |  ]
-- |  ```
-- |
-- |
-- | (?) note that it handled `value` as a String, as it is the KISS solution
-- |     here
-- |
-- | https://getbootstrap.com/docs/4.1/components/forms/
formSelect :: forall r. R2.OptComponent Options Props r
formSelect = R2.optComponent component options

componentName :: String
componentName = "b-form-select"

bootstrapName :: String
bootstrapName = "form-control"

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
      , bootstrapName <> "-" <> show props.size
      ]

    change <- pure $ onChange status callback
    -- Render
    pure $
      R2.select
      { className
      , on: { change }
      , disabled: elem status [ Disabled ]
      , readOnly: elem status [ Idled ]
      , type: props.type
      , value: props.value
      }
      children

-- | * Change event will effectively be triggered according to the
-- | component status props
-- | * Also directly returns the newly input value
-- | (usage not so different from `targetValue` of ReactBasic)
onChange :: forall event.
     ComponentStatus
  -> (String -> Effect Unit)
  -> event
  -> Effect Unit
onChange status callback event = do
  if   status == Enabled
  then callback $ (unsafeCoerce event).target.value
  else pure unit
