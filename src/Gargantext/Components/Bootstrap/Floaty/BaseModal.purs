module Gargantext.Components.Bootstrap.BaseModal
  (baseModal
  , showModal, hideModal
  ) where

import Gargantext.Prelude

import DOM.Simple (Window, window)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.UUID as UUID
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Gargantext.Components.Bootstrap.Types (ModalSizing(..))
import Gargantext.Hooks.UpdateEffect (useUpdateEffect1')
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

foreign import _show :: EffectFn2
  Window
  String
  Unit

showModal ::
     Window
  -> String
  -> Effect Unit
showModal = runEffectFn2 _show

foreign import _hide :: EffectFn2
  Window
  String
  Unit

hideModal ::
     Window
  -> String
  -> Effect Unit
hideModal = runEffectFn2 _hide


type Props =
  ( isVisibleBox              :: T.Box Boolean
  | Options
  )

type Options =
  ( modalClassName            :: String
  , title                     :: Maybe String
  , hasCollapsibleBackground  :: Boolean
  , hasInnerScroll            :: Boolean
  , noHeader                  :: Boolean
  , noBody                    :: Boolean -- ie. Bootstrap Body
  , size                      :: ModalSizing
  )

options :: Record Options
options =
  { modalClassName            : ""
  , title                     : Nothing
  , hasCollapsibleBackground  : true
  , hasInnerScroll            : false
  , noHeader                  : false
  , noBody                    : false
  , size                      : MediumModalSize
  }

componentName :: String
componentName = "b-modal"

-- | Structural Component for the Bootstrap modal
-- |
-- | @XXX Bootstrap not removing some modal elements on "hide" method
-- |      This implies that:
-- |        - a FFI fix has been added to remove left elements
-- |        - an overlay has been added to synchronise the close button
-- |        - the keyboard shortcut has been removed
-- | @link https://stackoverflow.com/questions/50168312/bootstrap-4-close-modal-backdrop-doesnt-disappear
-- |
-- | @link https://getbootstrap.com/docs/4.6/components/modal/
baseModal :: forall r. R2.OptComponent Options Props r
baseModal = R2.optComponent component options

component :: R.Memo Props
component = R.memo' $ R.hooksComponent componentName cpt where
  cpt props@{ isVisibleBox
            , title
            , hasCollapsibleBackground
            , hasInnerScroll
            , noHeader
            , noBody
            , size
            } children
      = R.unsafeHooksEffect (UUID.genUUID >>= pure <<< UUID.toString)
    >>= \uuid -> do
    -- | States
    -- |
    isVisible <- R2.useLive' isVisibleBox

    -- | Computed
    -- |
    let
      className = intercalate " "
        -- Component
        [ componentName
        -- Bootstrap
        , "modal"
        ]

      id = componentName <> "-" <> uuid

      selector = "#" <> id

    -- | Hooks
    -- |
    useUpdateEffect1' isVisible
      if isVisible
      then showModal window selector
      else hideModal window selector

    -- | Behaviors
    -- |
    let
      onCloseButtonClick _ = T.modify_ (not) isVisibleBox

    -- [ Render
    -- |
    R.createPortal
      [
        H.div
        { id: id
        , className
        , tabIndex: "-1"
        , key: id
        , data:
            { keyboard: "false"
            , backdrop: hasCollapsibleBackground ?
              "true" $
              "static"
            }
        }
        [
          -- Overlay fixing collapsable click event
          R2.when (hasCollapsibleBackground) $

            H.div
            { className: componentName <> "__overlay"
            , on: { click: onCloseButtonClick }
            }
            []
        ,
          H.div
          { className: intercalate " "
              -- Bootstrap classNames
              [ "modal-dialog"
              , show size
              , "modal-dialog-centered"
              , hasInnerScroll ? "modal-dialog-scrollable" $ ""
              -- provided custom className
              , props.modalClassName
              ]
          }
          [
            H.div
            { className: intercalate " "
                [ componentName <> "__content"
                , "modal-content"
                ]
            }
            [
              -- Header
              R2.when (not noHeader) $

                H.div
                { className: intercalate " "
                    [ componentName <> "__header"
                    , "modal-header"
                    ]
                }
                [
                  R2.fromMaybe (title) \title' ->

                    H.div
                    { className: componentName <> "__header__title" }
                    [ H.text title' ]
                ,
                  H.button
                  { type: "button"
                  }
                  [
                    H.a
                    {
                      on: { click: onCloseButtonClick }
                    , className: "btn fa fa-times"
                    }
                    []
                  ]
                ]
            ,
              -- Body
              H.div
              { className: intercalate " "
                  [ componentName <> "__body"
                  , noBody ? "" $ "modal-body"
                  ]
              }
              children
            ]
          ]
        ]
      ]
      <$> R2.getPortalHost
