module Gargantext.Components.Bootstrap.BaseModal (baseModal) where

import Gargantext.Prelude

import DOM.Simple (Window)
import Data.Foldable (intercalate)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Gargantext.Utils (nbsp, (?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

foreign import _addClassName :: EffectFn2 Window String Unit
foreign import _removeClassName :: EffectFn2 Window String Unit

type Props =
  ( isVisibleBox :: T.Box Boolean
  | Options
  )

type Options =
  ( id :: String
  , title :: String
  , hasBackground :: Boolean
  , hasCollapsibleBackground :: Boolean
  )

options :: Record Options
options =
  { id: ""
  , title: ""
  , hasBackground: true
  , hasCollapsibleBackground: true
  }

componentName :: String
componentName = "b-modal"

vendorName :: String
vendorName = "modal"

baseModal :: forall r. R2.OptComponent Options Props r
baseModal = R2.optComponent component options

component :: R.Component Props
component = R.hooksComponent componentName cpt where
  cpt { isVisibleBox
      , id
      , title
      , hasBackground
      , hasCollapsibleBackground
      } children = do
    -- State
    isVisible <- R2.useLive' isVisibleBox

    -- Hooks
    -- R.useEffect1' isVisible $
      -- (isVisible ? addClassName $ removeClassName) window "modal-open"

    -- Computed
    let
      className = intercalate " "
        -- Component
        [ componentName
        , isVisible ?
            componentName <> "--visible" $
            componentName <> "--hidden"
        -- Vendor
        , vendorName
        ]

      hasHeader = not $ eq title ""

    -- Render
    R.createPortal
      [
        H.div
        { id
        , className
        , role: "dialog"
        , data: { show: true }
        }
        [
          R2.if' (hasBackground) $
            H.div
            { className: intercalate " "
                [ componentName <> "__overlay"
                , hasCollapsibleBackground ?
                    componentName <> "__overlay--collapsible" $
                    ""
                ]
            , on: { click: hasCollapsibleBackground ?
                      toggle isVisibleBox $
                      const $ pure unit
                  }
            }
            [ H.text $ nbsp 1 ]
        ,
          H.div
          { className: "modal-dialog modal-lg"
          , role: "document"
          }
          [
            H.div
            { className: intercalate " "
                [ componentName <> "__content"
                , vendorName <> "-content"
                ]
            }
            [
              R2.if' (hasHeader) $
                H.div
                { className: intercalate " "
                    [ componentName <> "__header"
                    , vendorName <> "-header"
                    ]
                }
                [
                  H.div
                  { className: componentName <> "__header__content" }
                  [ H.text title ]
                ,
                  H.button
                  { type: "button"
                  , className: "close"
                  , data: { dismiss: "modal" }
                  }
                  [
                    H.a
                    { on: { click: toggle isVisibleBox }
                    , className: "btn fa fa-times" }
                    []
                  ]
                ]
            ,
              H.div
              { className: "modal-body" }
              children
            ]
          ]
        ]
      ]
      <$> R2.getPortalHost


toggle :: forall event. T.Box Boolean -> event -> Effect Unit
toggle box _ = T.modify_ not box

addClassName :: Window -> String -> Effect Unit
addClassName = runEffectFn2 _addClassName

removeClassName :: Window -> String -> Effect Unit
removeClassName = runEffectFn2 _removeClassName
