module Gargantext.Components.Renameable where

import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Prelude

import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Utils.Reactix as R2


here :: R2.Here
here = R2.here "Gargantext.Components.Renameable"

type RenameableProps =
  (
    onRename :: String -> Effect Unit
  , text     :: String
  )

renameable :: R2.Component RenameableProps
renameable = R.createElement renameableCpt

renameableCpt :: R.Component RenameableProps
renameableCpt = here.component "renameableCpt" cpt
  where
    cpt { onRename, text } _ = do
      isEditing <- T.useBox false
      state <- T.useBox text
      textRef <- R.useRef text

      -- handle props change of text
      R.useEffect1' text $ do
        if R.readRef textRef == text then
          pure unit
        else do
          R.setRef textRef text
          T.write_ text state

      pure $ H.div { className: "renameable" } [
        renameableText { isEditing, onRename, state } []
      ]

type RenameableTextProps =
  (
    isEditing :: T.Box Boolean
  , onRename  :: String -> Effect Unit
  , state     :: T.Box String
  )

renameableText :: R2.Component RenameableTextProps
renameableText = R.createElement renameableTextCpt

renameableTextCpt :: R.Component RenameableTextProps
renameableTextCpt = here.component "renameableText" cpt
  where
    cpt props@{ isEditing, state } _ = do
      isEditing' <- T.useLive T.unequal isEditing

      pure $ if isEditing' then
               notEditing props []
             else
               editing props []


notEditing :: R2.Component RenameableTextProps
notEditing = R.createElement notEditingCpt

notEditingCpt :: R.Component RenameableTextProps
notEditingCpt = here.component "notEditing" cpt
  where
    cpt props@{ isEditing, state } _ = do
      state' <- T.useLive T.unequal state

      pure $ H.div { className: "input-group" }
        [ H.input { className: "form-control"
                  , defaultValue: state'
                  , disabled: 1
                  , type: "text" }
        , H.div { className: "btn input-group-append"
                , on: { click: \_ -> T.write_ true isEditing } }
          [ H.span { className: "fa fa-pencil" } []
          ]
        ]


editing :: R2.Component RenameableTextProps
editing = R.createElement editingCpt

editingCpt :: R.Component RenameableTextProps
editingCpt = here.component "editing" cpt
  where
    cpt props@{ isEditing, onRename, state } _ = do
      state' <- T.useLive T.unequal state

      pure $ H.div { className: "input-group" }
        [ inputWithEnter {
            autoFocus: false
          , className: "form-control text"
          , defaultValue: state'
          , onBlur: \s -> T.write_ s state
          , onEnter: submit state'
          , onValueChanged: \s -> T.write_ s state
          , placeholder: ""
          , type: "text"
          }
        , H.div { className: "btn input-group-append"
                , on: { click: submit } }
          [ H.span { className: "fa fa-floppy-o" } []
          ]
        ]
      where
        submit text _ = do
          T.write_ false isEditing
          onRename text
