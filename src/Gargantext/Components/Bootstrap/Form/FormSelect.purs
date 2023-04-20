module Gargantext.Components.Bootstrap.FormSelect
  ( formSelect
  , formSelect'
  ) where

import Gargantext.Prelude

import Data.Foldable (elem, intercalate)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..), Sizing(..))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Show as GUS
import Reactix as R
import Reactix.DOM.HTML as H
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
-- |     here. Please use `formSelect'` for any other type
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
    let
      className = intercalate " "
        -- provided custom className
        [ props.className
        -- BEM classNames
        , componentName
        , componentName <> "--" <> show status
        -- Bootstrap specific classNames
        , bootstrapName
        , bootstrapName <> "-" <> show props.size
        ]

    -- Behaviors
    let
      change = onChange status callback

    -- Render
    pure $
      R2.select
      { className
      , on: { change }
      , disabled: elem status [ Disabled, Idled ]
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
  else R.nothing


-----------------------------------------------------------------------

type AnyTypeProps a =
  ( callback :: a -> Effect Unit
  , value :: a
  , list :: Array a
  | Options
  )

-- | Derived component for `formSelect` with any value type (with `Read`
-- | and `Show` constraint)
-- |
-- |
-- |  ```purescript
-- |  formSelect'
-- |  { callback: flip T.write_ box
-- |  , value: anyType
-- |  , list: [ anyType, anyType, ... ]
-- |  }
-- |  ```
-- |
-- |  (?) Note that HTML option tags will be automatically added thanks to
-- |      to the provided `list` prop. You can add additional HTML option within
-- |      the `children` prop
formSelect' :: forall r a.
     Show a
  => R2.OptComponent Options (AnyTypeProps a) r
formSelect' = R2.optComponent component' options

component' :: forall a.
     Show a
  => R.Component (AnyTypeProps a)
component' = R.hooksComponent (componentName <> "__helper") cpt where
  cpt props@{ callback
            , list
            , status
            , value
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
        , bootstrapName <> "-" <> show props.size
        ]

    -- Behaviors
    let
      change = onChange' status reader callback

    -- Render
    pure $
      R2.select
      { className
      , on: { change }
      , disabled: elem status [ Disabled ]
      , readOnly: elem status [ Idled ]
      , type: props.type
      , value: show value
      }
      (
        children
      <>
        flip map list \raw ->
          H.option
          { value: show raw }
          [ H.text $ show raw ]
      )
    where
      reader = GUS.reader list

-- | * Change event will effectively be triggered according to the
-- | component status props
-- | * Also directly returns the newly input value
-- | (usage not so different from `targetValue` of ReactBasic)
onChange' :: forall event a.
     Show a
  => ComponentStatus
  -> (String -> Maybe a)
  -> (a -> Effect Unit)
  -> event
  -> Effect Unit
onChange' status reader callback event = do
  if   status == Enabled
  then event # unsafeCoerce >>> _.target.value >>> reader >>> case _ of
    Nothing -> R.nothing
    Just v  -> callback v
  else R.nothing
