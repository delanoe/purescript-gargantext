module Gargantext.Components.InputWithAutocomplete where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null, toMaybe)
import DOM.Simple as DOM
import DOM.Simple.Event as DE
import Effect (Effect)
import Effect.Timer (setTimeout)
import Reactix as R
import Reactix.DOM.HTML as H
import React.SyntheticEvent as E
import Toestand as T

import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.InputWithAutocomplete"


type Completions = Array String

type Props =
  (
    autocompleteSearch  :: String -> Completions
  , classes             :: String
  , onAutocompleteClick :: String -> Effect Unit
  , onEnterPress        :: String -> Effect Unit
  , state               :: T.Box String
  )

inputWithAutocomplete :: R2.Component Props
inputWithAutocomplete = R.createElement inputWithAutocompleteCpt
inputWithAutocompleteCpt :: R.Component Props
inputWithAutocompleteCpt = here.component "inputWithAutocomplete" cpt
  where
    cpt props@{ autocompleteSearch
              , classes
              , onAutocompleteClick
              , onEnterPress
              , state } _ = do
      state' <- T.useLive T.unequal state
      inputRef <- R.useRef null
      completions <- T.useBox $ autocompleteSearch state'

      let onFocus completions' _ = T.write_ (autocompleteSearch state') completions'

      pure $
        H.span { className: "input-with-autocomplete " <> classes }
        [
          completionsCpt { completions, onAutocompleteClick, state } []
        , H.input { type: "text"
                  , ref: inputRef
                  , className: "form-control"
                  , value: state'
                  , on: { blur: onBlur completions
                        , focus: onFocus completions
                        , input: onInput completions
                        , change: onInput completions
                        , keyUp: onInputKeyUp inputRef } }
        ]

      where

        -- setTimeout is a bit of a hack here -- clicking on autocomplete
        -- element will clear out the blur first, so the autocomplete click
        -- won't fire without a timeout here.  However, blur is very handy and
        -- handles automatic autocomplete search, otherwise I'd have to hide it
        -- in various different places (i.e. carefully handle all possible
        -- events where blur happens and autocomplete should hide).
        onBlur completions _ = setTimeout 100 $ do
          T.write_ [] completions

        onInput completions e = do
          let val = R.unsafeEventValue e
          T.write_ val state
          T.write_ (autocompleteSearch val) completions

        onInputKeyUp :: R.Ref (Nullable DOM.Element) -> DE.KeyboardEvent -> Effect Boolean
        onInputKeyUp inputRef e = do
          if DE.key e == "Enter" then do
            let val = R.unsafeEventValue e
            let mInput = toMaybe $ R.readRef inputRef
            T.write_ val state
            onEnterPress val
            case mInput of
              Nothing -> pure false
              Just input -> do
                R2.blur input
                pure false
          else
            pure $ false

type CompletionsProps =
  ( completions :: T.Box Completions
  , onAutocompleteClick :: String -> Effect Unit
  , state :: T.Box String
  )

completionsCpt :: R2.Component CompletionsProps
completionsCpt = R.createElement completionsCptCpt

completionsCptCpt :: R.Component CompletionsProps
completionsCptCpt = here.component "completionsCpt" cpt
  where
    cpt { completions, onAutocompleteClick, state } _ = do
      completions' <- T.useLive T.unequal completions

      let className = "completions " <> (if completions' == [] then "d-none" else "")

      pure $ H.div { className }
        [
          H.div { className: "list-group" } (cCpt <$> completions')
        ]
      where
        cCpt c =
          H.button { type: "button"
                    , className: "list-group-item"
                    , on: { click: onClick c } } [ H.text c ]
        onClick c _ = do
          T.write_ c state
          onAutocompleteClick c
