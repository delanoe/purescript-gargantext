module Gargantext.Components.Renameable where

import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Utils.Reactix as R2


thisModule :: String
thisModule = "Gargantext.Components.Renameable"

type RenameableProps =
  (
    onRename :: String -> Effect Unit
  , text :: String
  )

renameable :: Record RenameableProps -> R.Element
renameable props = R.createElement renameableCpt props []

renameableCpt :: R.Component RenameableProps
renameableCpt = R.hooksComponentWithModule thisModule "renameableCpt" cpt
  where
    cpt {onRename, text} _ = do
      isEditing <- R.useState' false
      state <- R.useState' text
      textRef <- R.useRef text

      -- handle props change of text
      R.useEffect1' text $ do
        if R.readRef textRef == text then
          pure unit
        else do
          R.setRef textRef text
          snd state $ const text

      pure $ H.div { className: "renameable" } [
        renameableText { isEditing, onRename, state }
      ]

type RenameableTextProps =
  (
    isEditing :: R.State Boolean
  , onRename :: String -> Effect Unit
  , state :: R.State String
  )

renameableText :: Record RenameableTextProps -> R.Element
renameableText props = R.createElement renameableTextCpt props []

renameableTextCpt :: R.Component RenameableTextProps
renameableTextCpt = R.hooksComponentWithModule thisModule "renameableTextCpt" cpt
  where
    cpt {isEditing: (false /\ setIsEditing), state: (text /\ _)} _ = do
      pure $ H.div { className: "input-group" }
        [ H.input { className: "form-control"
                  , defaultValue: text
                  , disabled: 1
                  , type: "text" }
        , H.div { className: "btn input-group-append"
                , on: { click: \_ -> setIsEditing $ const true } }
          [ H.span { className: "fa fa-pencil" } []
          ]
        ]
    cpt {isEditing: (true /\ setIsEditing), onRename, state: (text /\ setText)} _ = do
      pure $ H.div { className: "input-group" }
        [ inputWithEnter {
            autoFocus: false
          , className: "form-control text"
          , defaultValue: text
          , onBlur: setText <<< const
          , onEnter: submit
          , onValueChanged: setText <<< const
          , placeholder: ""
          , type: "text"
          }
        , H.div { className: "btn input-group-append"
                , on: { click: submit } }
          [ H.span { className: "fa fa-floppy-o" } []
          ]
        ]
      where
        submit _ = do
          setIsEditing $ const false
          onRename text
