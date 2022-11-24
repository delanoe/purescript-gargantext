module Gargantext.Components.Renameable
  ( RenameableProps
  , RenameableTextProps
  , RenameableOptions
  , editingCpt
  , here
  , notEditing
  , notEditingCpt
  , renameable
  , renameableCpt
  , renameableText
  , renameableTextCpt
  )
  where

import Gargantext.Prelude

import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), Elevation(..), Variant(..))
import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T


here :: R2.Here
here = R2.here "Gargantext.Components.Renameable"

type RenameableProps =
  ( onRename :: String -> Effect Unit
  , text     :: String
  | RenameableOptions
  )

type RenameableOptions =
  ( className :: String
  , icon      :: Maybe String
  )

renameableOptions :: Record RenameableOptions
renameableOptions =
  { className : ""
  , icon      : Nothing
  }

renameable :: forall r. R2.OptLeaf RenameableOptions RenameableProps r
renameable = R2.optLeaf renameableCpt renameableOptions

renameableCpt :: R.Component RenameableProps
renameableCpt = here.component "renameableCpt" cpt where
  cpt { onRename, text, icon, className } _ = do
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

    pure $

      H.div
      { className: intercalate " "
          [ "renameable-wrapper"
          , className
          ]
      }
      [
        renameableText
        { isEditing, onRename, state, icon }
      ]

----------------------------------------------------------------

type RenameableTextProps =
  (
    isEditing :: T.Box Boolean
  , onRename  :: String -> Effect Unit
  , state     :: T.Box String
  , icon      :: Maybe String
  )

renameableText :: R2.Leaf RenameableTextProps
renameableText = R2.leaf renameableTextCpt

renameableTextCpt :: R.Component RenameableTextProps
renameableTextCpt = here.component "renameableText" cpt where
  cpt props@{ isEditing } _ = do
    isEditing' <- T.useLive T.unequal isEditing

    pure $

      if isEditing' then
        editing props
      else
        notEditing props

------------------------------------------------------

notEditing :: R2.Leaf RenameableTextProps
notEditing = R2.leaf notEditingCpt

notEditingCpt :: R.Component RenameableTextProps
notEditingCpt = here.component "notEditing" cpt where
  cpt { isEditing, state, icon} _ = do
    -- | States
    -- |
    state' <- T.useLive T.unequal state

    -- | Behaviors
    -- |
    let
      onClick _ = T.write_ true isEditing

    -- | Render
    -- |
    pure $

      H.div
      { className: intercalate " "
          [ "renameable-container"
          , "renameable-container--no-editing"
          ]
      }
      [
        R2.fromMaybe icon \icon' ->

          B.icon
          { name: icon'
          , className: "renameable-container__icon"
          }
      ,
        B.span'
        { className: "renameable-container__text" }
        state'
      ,
        B.iconButton
        { name: "pencil"
        , variant: Dark
        , callback: onClick
        , elevation: Level1
        , className: "renameable-container__button"
        }
      ]

-------------------------------------------------------------------

editing :: R2.Leaf RenameableTextProps
editing = R2.leaf editingCpt

editingCpt :: R.Component RenameableTextProps
editingCpt = here.component "editing" cpt where
  cpt { isEditing, onRename, state, icon } _ = do
    -- | States
    -- |
    state' <- T.useLive T.unequal state

    -- | Behaviors
    -- |
    let
      onSubmit text _ = do
        T.write_ false isEditing
        onRename text

      -- onReset _ = do
      --   T.write_ false isEditing

    -- | Render
    -- |
    pure $

      H.div
      { className: intercalate " "
          [ "renameable-container"
          , "renameable-container--editing"
          ]
      }
      [
        R2.fromMaybe icon \icon' ->

          B.icon
          { name: icon'
          , className: "renameable-container__icon"
          }
      ,
        H.div
        { className: intercalate " "
            [ "renameable-container__input"
            , "input-group input-group-sm"
            ]
        }
        [
          inputWithEnter
          { autoFocus: false
          , className: "form-control"
          , defaultValue: state'
          , onBlur: \s -> T.write_ s state
          , onEnter: onSubmit state'
          , onValueChanged: \s -> T.write_ s state
          , placeholder: ""
          , type: "text"
          }
        ,
        -- @TODO make a "reset" CTA
        --   H.div
        --   { className: "input-group-append" }
        --   [
        --     B.button
        --     { variant: ButtonVariant Light
        --     , callback: onReset
        --     , className: "input-group-text"
        --     }
        --     [
        --       B.icon
        --       { name: "times"
        --       , className: "text-danger"
        --       }
        --     ]
        --   ]
        -- ,
          H.div
          { className: "input-group-append" }
          [
            B.button
            { variant: ButtonVariant Light
            , callback: onSubmit state'
            , className: "input-group-text"
            }
            [
              B.icon
              { name: "floppy-o"
              , className: "text-primary"
              }
            ]
          ]
        ]
      ]
