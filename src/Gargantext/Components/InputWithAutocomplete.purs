module Gargantext.Components.InputWithAutocomplete where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null, toMaybe)
import Data.Tuple.Nested ((/\))
import DOM.Simple as DOM
import DOM.Simple.Event as DE
import Effect (Effect)
import Effect.Timer (setTimeout)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Utils.Reactix as R2

here = R2.here "Gargantext.Components.InputWithAutocomplete"


type Completions = Array String

type Props =
  (
    autocompleteSearch :: String -> Completions
  , onAutocompleteClick :: String -> Effect Unit
  , onEnterPress :: String -> Effect Unit
  , state :: R.State String
  )

inputWithAutocomplete :: Record Props -> R.Element
inputWithAutocomplete props = R.createElement inputWithAutocompleteCpt props []

inputWithAutocompleteCpt :: R.Component Props
inputWithAutocompleteCpt = here.component "inputWithAutocomplete" cpt
  where
    cpt props@{autocompleteSearch, onAutocompleteClick, onEnterPress, state: state@(state' /\ setState)} _ = do
      inputRef <- R.useRef null
      completions <- R.useState' $ autocompleteSearch state'

      pure $
        H.span { className: "input-with-autocomplete" }
        [
          completionsCpt completions
        , H.input { type: "text"
                  , ref: inputRef
                  , className: "form-control"
                  , value: state'
                  , on: { blur: onBlur completions
                        , focus: onFocus completions
                        , input: onInput completions
                        , change: onInput completions
                        , keyUp: onInputKeyUp inputRef completions } }
        ]

      where

        -- setTimeout is a bit of a hack here -- clicking on autocomplete
        -- element will clear out the blur first, so the autocomplete click
        -- won't fire without a timeout here.  However, blur is very handy and
        -- handles automatic autocomplete search, otherwise I'd have to hide it
        -- in various different places (i.e. carefully handle all possible
        -- events where blur happens and autocomplete should hide).
        onBlur (_ /\ setCompletions) e = setTimeout 100 $ do
          setCompletions $ const []

        onFocus (_ /\ setCompletions) e = setCompletions $ const $ autocompleteSearch state'

        onInput (_ /\ setCompletions) e = do
          let val = R.unsafeEventValue e
          setState $ const val
          setCompletions $ const $ autocompleteSearch val

        onInputKeyUp :: R.Ref (Nullable DOM.Element) -> R.State Completions -> DE.KeyboardEvent -> Effect Unit
        onInputKeyUp inputRef (_ /\ setCompletions) e = do
          if DE.key e == "Enter" then do
            let val = R.unsafeEventValue e
            let mInput = toMaybe $ R.readRef inputRef
            setState $ const val
            onEnterPress val
            case mInput of
              Nothing -> pure unit
              Just input -> R2.blur input
          else
            pure $ unit

        completionsCpt :: R.State Completions -> R.Element
        completionsCpt (completions /\ setCompletions) =
          H.div { className }
          [
            H.div { className: "list-group" } (cCpt <$> completions)
          ]
          where
            className = "completions " <> (if completions == [] then "d-none" else "")

            cCpt c =
              H.button { type: "button"
                       , className: "list-group-item"
                       , on: { click: onClick c } } [ H.text c ]
            onClick c _ = do
              setState $ const c
              onAutocompleteClick c
