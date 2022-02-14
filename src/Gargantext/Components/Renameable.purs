module Gargantext.Components.Renameable where

import Gargantext.Prelude

import Effect (Effect)
import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T


here :: R2.Here
here = R2.here "Gargantext.Components.Renameable"

type RenameableProps =
  (
    onRename :: String -> Effect Unit
  , text     :: String
  , icon     :: String
  )

renameable :: R2.Component RenameableProps
renameable = R.createElement renameableCpt
renameableCpt :: R.Component RenameableProps
renameableCpt = here.component "renameableCpt" cpt
  where
    cpt { onRename, text, icon } _ = do
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
        renameableText { isEditing, onRename, state, icon } []
      ]

type RenameableTextProps =
  (
    isEditing :: T.Box Boolean
  , onRename  :: String -> Effect Unit
  , state     :: T.Box String
  , icon      :: String
  )

renameableText :: R2.Component RenameableTextProps
renameableText = R.createElement renameableTextCpt
renameableTextCpt :: R.Component RenameableTextProps
renameableTextCpt = here.component "renameableText" cpt
  where
    cpt props@{ isEditing } _ = do
      isEditing' <- T.useLive T.unequal isEditing

      pure $ if isEditing' then
               editing props []
             else
               notEditing props []


notEditing :: R2.Component RenameableTextProps
notEditing = R.createElement notEditingCpt
notEditingCpt :: R.Component RenameableTextProps
notEditingCpt = here.component "notEditing" cpt
  where
    cpt { isEditing, state, icon} _ = do
      state' <- T.useLive T.unequal state

      pure $ H.div { className: "input-group" }
        [ H.span {className: icon} []
        , H.text state'
        , H.button { className: "btn input-group-append"
                , on: { click: \_ -> T.write_ true isEditing } }
          [ H.span { className: "fa fa-pencil" } []
          ]
        ]


editing :: R2.Component RenameableTextProps
editing = R.createElement editingCpt
editingCpt :: R.Component RenameableTextProps
editingCpt = here.component "editing" cpt
  where
    cpt { isEditing, onRename, state, icon } _ = do
      state' <- T.useLive T.unequal state

      pure $ H.div { className: "input-group" }
        [ H.span {className: icon} []
        , inputWithEnter {
            autoFocus: false
          , className: "form-control text"
          , defaultValue: state'
          , onBlur: \s -> T.write_ s state
          , onEnter: submit state'
          , onValueChanged: \s -> T.write_ s state
          , placeholder: ""
          , type: "text"
          }
        , H.button { className: "btn input-group-append"
                , on: { click: submit state' } }
          [ H.span { className: "fa fa-floppy-o" } []
          ]
        ]
      where
        submit text _ = do
          T.write_ false isEditing
          onRename text
