module Gargantext.Components.InputWithEnter where

import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.InputWithEnter"

type Props a =
  ( onBlur         :: String -> Effect Unit
  , onEnter        :: Unit -> Effect Unit
  , onValueChanged :: String -> Effect Unit

  , autoFocus      :: Boolean
  , className      :: String
  , defaultValue   :: String
  , placeholder    :: String
  , type           :: String
  )

type PropsKey a =
  ( key :: String
  | Props a )

inputWithEnterWithKey :: forall a. R2.Leaf (PropsKey a)
inputWithEnterWithKey = R2.leafComponent inputWithEnterWithKeyCpt
inputWithEnterWithKeyCpt = here.component "inputWithEnterWithKey" cpt where
  cpt { onBlur, onEnter, onValueChanged, autoFocus, className, defaultValue, placeholder, type: t } _ = do
    pure $ inputWithEnter { onBlur
                          , onEnter
                          , onValueChanged
                          , autoFocus
                          , className
                          , defaultValue
                          , placeholder
                          , type: t }

inputWithEnter :: forall a. R2.Leaf (Props a)
inputWithEnter = R2.leafComponent inputWithEnterCpt
inputWithEnterCpt :: forall a. R.Component (Props a)
inputWithEnterCpt = here.component "inputWithEnter" cpt
  where
    cpt props@{ onBlur, onEnter, onValueChanged
              , autoFocus, className, defaultValue, placeholder } _ = do
      pure $ H.input { on: { blur: onBlur'
                           , input: onInput
                           , keyPress: onKeyPress }
                     , autoFocus
                     , className
                     , defaultValue
                     , placeholder
                     , type: props.type }

      where
        onBlur' e = onBlur $ R.unsafeEventValue e
        onInput e = onValueChanged $ R.unsafeEventValue e

        onKeyPress e = do
          char <- R2.keyCode e
          if char == 13 then
            onEnter unit
          else
            pure unit

