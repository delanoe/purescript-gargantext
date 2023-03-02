module Gargantext.Components.InputWithAutocomplete where

import Prelude

import DOM.Simple (contains)
import DOM.Simple as DOM
import DOM.Simple.Event as DE
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, null, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import FFI.Simple ((..))
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (Elevation(..))
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

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

inputWithAutocomplete :: R2.Leaf Props
inputWithAutocomplete = R2.leaf inputWithAutocompleteCpt
inputWithAutocompleteCpt :: R.Component Props
inputWithAutocompleteCpt = here.component "inputWithAutocomplete" cpt
  where
    cpt { autocompleteSearch
        , classes
        , onAutocompleteClick
        , onEnterPress
        , state } _ = do
      -- States
      state'        <- T.useLive T.unequal state
      containerRef  <- R.useRef null
      inputRef      <- R.useRef null
      completions   <- T.useBox $ autocompleteSearch state'

      -- Render
      pure $

        H.div
        { className: "input-with-autocomplete " <> classes
        , ref: containerRef
        }
        [
          completionsCpt { completions, onAutocompleteClick, state } []
        , H.input { type: "text"
                  , ref: inputRef
                  , className: "form-control"
                  , value: state'
                  , on: { focus: onFocus completions state'
                        , input: onInput completions
                        , change: onInput completions
                        , keyUp: onInputKeyUp inputRef
                        , blur: onBlur completions containerRef
                        }
                  }
        ]

      -- Helpers
      where
        -- (!) `onBlur` DOM.Event is triggered before any `onClick` DOM.Event
        --     So when a completion is being clicked, the UX will be broken
        --
        --        ↳ As a solution we chose to check if the click is made from
        --          the autocompletion list
        onBlur :: forall event.
             T.Box Completions
          -> R.Ref (Nullable DOM.Element)
          -> event
          -> Effect Unit
        onBlur completions containerRef event =

          if isInnerEvent
          then
            pure $ (event .. "preventDefault")
          else
            T.write_ [] completions

          where
            mContains = do
              a <- toMaybe $ R.readRef containerRef
              b <- toMaybe (event .. "relatedTarget")
              Just (contains a b)

            isInnerEvent = maybe false identity mContains


        onFocus :: forall event. T.Box Completions -> String -> event -> Effect Unit
        onFocus completions st _ = T.write_ (autocompleteSearch st) completions

        onInput :: forall event. T.Box Completions -> event -> Effect Unit
        onInput completions e = do
          let val = R.unsafeEventValue e
          T.write_ val state
          T.write_ (autocompleteSearch val) completions

        onInputKeyUp :: R.Ref (Nullable DOM.Element) -> DE.KeyboardEvent -> Effect Boolean
        onInputKeyUp inputRef e = do
          if DE.key e == "Enter" then do
            R2.preventDefault e
            R2.stopPropagation e
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

type Props' =
  (
    autocompleteSearch  :: String -> Completions
  , classes             :: String
  , onAutocompleteClick :: String -> Effect Unit
  , dispatch            :: Action -> Aff Unit
  , boxAction           :: String -> Action
  , state               :: T.Box String
  , text                :: T.Box String
  )

inputWithAutocomplete' :: R2.Leaf Props'
inputWithAutocomplete' = R2.leaf inputWithAutocompleteCpt'
inputWithAutocompleteCpt' :: R.Component Props'
inputWithAutocompleteCpt' = here.component "inputWithAutocomplete" cpt
  where
    cpt { autocompleteSearch
        , classes
        , onAutocompleteClick
        , dispatch
        , boxAction
        , state
        , text } _ = do
      -- States
      state'        <- T.useLive T.unequal state
      containerRef  <- R.useRef null
      inputRef      <- R.useRef null
      completions   <- T.useBox $ autocompleteSearch state'

      -- Render
      pure $

        H.div
        { className: "input-with-autocomplete " <> classes
        , ref: containerRef
        }
        [
          completionsCpt { completions, onAutocompleteClick, state } []
        , H.input { type: "text"
                  , ref: inputRef
                  , className: "form-control"
                  , value: state'
                  , on: { focus: onFocus completions state'
                        , input: onInput completions
                        , change: onInput completions
                        , keyUp: onInputKeyUp inputRef
                        , blur: onBlur completions containerRef
                        }
                  }
        , B.iconButton
            { callback: submit state'
            , title: "Submit"
            , name: "plus"
            , elevation: Level1
            }
        ]

      -- Helpers
      where
        -- (!) `onBlur` DOM.Event is triggered before any `onClick` DOM.Event
        --     So when a completion is being clicked, the UX will be broken
        --
        --        ↳ As a solution we chose to check if the click is made from
        --          the autocompletion list
        onBlur :: forall event.
             T.Box Completions
          -> R.Ref (Nullable DOM.Element)
          -> event
          -> Effect Unit
        onBlur completions containerRef event =

          if isInnerEvent
          then
            pure $ (event .. "preventDefault")
          else
            T.write_ [] completions

          where
            mContains = do
              a <- toMaybe $ R.readRef containerRef
              b <- toMaybe (event .. "relatedTarget")
              Just (contains a b)

            isInnerEvent = maybe false identity mContains


        onFocus :: forall event. T.Box Completions -> String -> event -> Effect Unit
        onFocus completions st _ = T.write_ (autocompleteSearch st) completions

        onInput :: forall event. T.Box Completions -> event -> Effect Unit
        onInput completions e = do
          let val = R.unsafeEventValue e
          T.write_ val state
          T.write_ (autocompleteSearch val) completions

        onInputKeyUp :: R.Ref (Nullable DOM.Element) -> DE.KeyboardEvent -> Effect Boolean
        onInputKeyUp inputRef e = do
          if DE.key e == "Enter" then do
            R2.preventDefault e
            R2.stopPropagation e
            let val = R.unsafeEventValue e
            let mInput = toMaybe $ R.readRef inputRef
            T.write_ val state
            launchAff_ $ dispatch (boxAction val)
            T.write_ ("Invited " <> val <> " to the team") text
            case mInput of
              Nothing -> pure false
              Just input -> do
                R2.blur input
                pure false
          else
            pure $ false

        submit val _ = do
          T.write_ ("Invited " <> val <> " to the team") text
          launchAff_ $ dispatch (boxAction val)

---------------------------------------------------------

type CompletionsProps =
  ( completions         :: T.Box Completions
  , onAutocompleteClick :: String -> Effect Unit
  , state               :: T.Box String
  )

completionsCpt :: R2.Component CompletionsProps
completionsCpt = R.createElement completionsCptCpt

completionsCptCpt :: R.Component CompletionsProps
completionsCptCpt = here.component "completionsCpt" cpt
  where
    cpt { completions, onAutocompleteClick, state } _ = do
      -- State
      completions' <- T.useLive T.unequal completions

      let className = "completions " <> (if completions' == [] then "d-none" else "")

      -- Render
      pure $

        H.div
        { className }
        [
          H.div { className: "list-group" } (cCpt <$> completions')
        ]

      -- Helpers
      where

        cCpt c =
          H.button { type: "button"
                    , className: "list-group-item"
                    , on: { click: onClick c } } [ H.text c ]

        onClick c _ = do
          T.write_ c state
          T.write_ [] completions
          onAutocompleteClick c
