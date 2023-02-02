module Gargantext.Components.InputWithEnter where

import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.InputWithEnter"

type Props =
  ( onBlur         :: String -> Effect Unit
  , onEnter        :: Unit -> Effect Unit
  , onValueChanged :: String -> Effect Unit

  , autoFocus      :: Boolean
  , className      :: String
  , defaultValue   :: String
  , placeholder    :: String
  , type           :: String
  )

inputWithEnterWithKey :: R2.Leaf ( key :: String | Props )
inputWithEnterWithKey = R2.leaf inputWithEnterWithKeyCpt
inputWithEnterWithKeyCpt :: R.Component ( key :: String | Props )
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

inputWithEnter :: R2.Leaf Props
inputWithEnter = R2.leaf inputWithEnterCpt
inputWithEnterCpt :: R.Component Props
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
