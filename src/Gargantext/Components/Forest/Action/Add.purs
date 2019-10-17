module Gargantext.Components.Forest.Action.Add where

import DOM.Simple.Console (log2)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Array (filter, null)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff, runAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn1)
import FFI.Simple ((..))
import Gargantext.Components.Forest.Action
import Gargantext.Components.Forest.NodeActions
import Gargantext.Ends (Frontends, url)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (AppRoute, SessionRoute(..))
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId, get, put, post, postWwwUrlencoded, delete)
import Gargantext.Types (class ToQuery, toQuery, NodeType(..), NodePath(..), readNodeType)
import Gargantext.Utils (id, glyphicon)
import Gargantext.Utils.Reactix as R2
import Partial.Unsafe (unsafePartial)
import Prelude hiding (div)
import React.SyntheticEvent as E
import Reactix as R
import Reactix.DOM.HTML as H
import URI.Extra.QueryPairs as QP
import URI.Query as Q
import Web.File.File (toBlob)
import Web.File.FileList (FileList, item)
import Web.File.FileReader.Aff (readAsText)

-- START Create Node

data NodePopup = CreatePopup | NodePopup

type CreateNodeProps =
  ( id       :: ID
  , name     :: Name
  , nodeType :: NodeType
  )

createNodeView :: (Action -> Aff Unit)
               -> Record CreateNodeProps
               -> R.State (Maybe NodePopup)
               -> R.Element
createNodeView d p@{nodeType} (Just CreatePopup /\ setPopupOpen) = R.createElement el p []
  where
    el = R.hooksComponent "CreateNodeView" cpt
    cpt {id, name} _ = do
      nodeName <- R.useState' ""
      nodeType <- R.useState' NodeUser
      pure $ H.div tooltipProps $
        [ H.div {className: "panel panel-default"}
          [ panelHeading
          , panelBody   nodeName nodeType
          , panelFooter nodeName nodeType
          ]
        ]
      where
        SettingsBox {add:nodeTypes} = settingsBox nodeType

        tooltipProps = { className: ""
                       , id: "create-node-tooltip"
                       , title: "Add new node"
                       , data: {toggle: "tooltip", placement: "right"}
                       }

        panelHeading =
          H.div {className: "panel-heading"}
          [ H.div {className: "row"}
            [ H.div {className: "col-md-10"}
              [ H.h5 {} [H.text "Add"] ]
            , H.div {className: "col-md-2"}
              [ H.a { className: "btn glyphitem glyphicon glyphicon-remove-circle"
                    , onClick: mkEffectFn1 $ \_ -> setPopupOpen $ const Nothing
                    , title: "Close"} []
              ]
            ]
          ]

        panelBody :: R.State String -> R.State NodeType -> R.Element
        panelBody (_ /\ setNodeName) (nt /\ setNodeType) =
          H.div {className: "panel-body"}
          [ H.div {className: "row"}
            [ H.div {className: "col-md-12"}
              [ H.form {className: "form-horizontal"}
                [ {- H.div {className: "form-group"}
                  [ H.input { type: "text"
                            , placeholder: "Node name"
                            , defaultValue: name
                            , className: "form-control"
                            , onInput: mkEffectFn1 $ \e -> setNodeName $ const $ e .. "target" .. "value"
                            }
                  ]
                  , -} H.div {className: "form-group"}
                  [ R2.select { className: "form-control"
                              , onChange: mkEffectFn1 $ \e -> setNodeType $ const $ readNodeType $ e .. "target" .. "value"
                              }
                    (map renderOption nodeTypes)
                  ]
                ]
              ]
            ]
          ]

        renderOption (opt :: NodeType) = H.option {} [ H.text $ show opt ]

        panelFooter :: R.State String  -> R.State NodeType -> R.Element
        panelFooter (name' /\ _) (nt /\ _) =
          H.div {className: "panel-footer"}
          [ H.button {className: "btn btn-success"
                     , type: "button"
                     , onClick: mkEffectFn1 $ \_ -> do
                         setPopupOpen $ const Nothing
                         launchAff $ d $ CreateSubmit name' nt
                     } [H.text "Add"]
          ]

createNodeView _ _ _ = R.createElement el {} []
  where
    el = R.hooksComponent "CreateNodeView" cpt
    cpt props _ = pure $ H.div {} []
-- END Create Node

