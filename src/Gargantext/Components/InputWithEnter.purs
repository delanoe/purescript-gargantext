module Gargantext.Components.InputWithEnter where

import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.InputWithEnter"

type Props a = (
    onEnter :: Unit -> Effect Unit
  , onValueChanged :: String -> Effect Unit

  , autoFocus :: Boolean
  , className :: String
  , defaultValue :: String
  , placeholder :: String
  , type :: String
  )

inputWithEnter :: forall a. Record (Props a) -> R.Element
inputWithEnter props = R.createElement inputWithEnterCpt props []

inputWithEnterCpt :: forall a. R.Component (Props a)
inputWithEnterCpt = R.hooksComponentWithModule thisModule "inputWithEnter" cpt
  where
    cpt props@{ onEnter, onValueChanged
              , autoFocus, className, defaultValue, placeholder } _ = do
      pure $ H.input { on: { input: onInput
                           , keyPress: onKeyPress }
                     , autoFocus
                     , className
                     , defaultValue
                     , placeholder
                     , type: props.type }

      where
        onInput e = do
           onValueChanged $ R.unsafeEventValue e

        onKeyPress e = do
          char <- R2.keyCode e
          if char == 13 then
            onEnter unit
          else
            pure unit

