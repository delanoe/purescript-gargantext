module Gargantext.Components.Forest.Tree.Node.Action.Add where

import DOM.Simple.Console (log2)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Array (filter, null, length, head, elem)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff, runAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn1)
import FFI.Simple ((..))
import Gargantext.Components.Forest.Tree.Node.Action
import Gargantext.Components.Forest.Tree.Node
import Gargantext.Ends (Frontends, url)
import Gargantext.Components.Loader (loader)
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
               -> Array NodeType
               -> R.Element
createNodeView d p@{nodeType} (_ /\ setPopupOpen) nodeTypes = R.createElement el p []
  where
    el = R.hooksComponent "CreateNodeView" cpt
    cpt {id, name} _ = do
      nodeName <- R.useState' ""
      nodeType <- R.useState' $ fromMaybe NodeUser $ head nodeTypes
      pure $ H.div {}
          [ panelBody   readNodeType nodeName nodeType
          , panelFooter nodeName nodeType
          ]
      where
        panelBody :: (String -> NodeType)
                  -> R.State String
                  -> R.State NodeType
                  -> R.Element
        panelBody readIt (_ /\ setNodeName) (nt /\ setNodeType) =
          H.div {className: "panel-body"}
          [ H.div {className: "row"}
            [ H.div {className: "col-md-10"}
              [ H.form {className: "form-horizontal"} $ maybeChoose <> maybeEdit ]
              ]
            ]
              where
                SettingsBox {edit} = settingsBox nt
                maybeEdit = [ if edit
                              then
                                H.div {className: "form-group"}
                                      [ H.input { type: "text"
                                                , placeholder: "Node name"
                                                , defaultValue: "Chose name"
                                                , className: "form-control"
                                                , onInput: mkEffectFn1 $ \e -> setNodeName
                                                                       $ const
                                                                       $ e .. "target" .. "value"
                                               }
                                      ]
                              else
                                H.div {} []
                             ]

                maybeChoose = [ if length nodeTypes > 1
                                then 
                                  R.fragment [H.div {className: "form-group"} $ [ R2.select { className: "form-control"
                                                    , onChange: mkEffectFn1 $ \e -> setNodeType
                                                                  $ const
                                                                  $ readIt 
                                                                  $ e .. "target" .. "value"
                                                    }
                                         (map (\opt -> H.option {} [ H.text $ show opt ]) nodeTypes)
                                         ]
                                         -- , showConfig nt
                                         ]
                                else
                                H.button { className : "btn btn-primary"
                                           , type : "button"
                                           , onClick : mkEffectFn1 $ \_ -> setNodeType ( const
                                                                                       $ fromMaybe nt 
                                                                                       $ head nodeTypes
                                                                                       )
                                           } []
                               ]



        panelFooter :: R.State String  -> R.State NodeType -> R.Element
        panelFooter (name' /\ _) (nt /\ _) =
          H.div {className: "panel-footer"}
          [ H.button {className: "btn btn-primary text-center"
                     , type: "button"
                     , onClick: mkEffectFn1 $ \_ -> do
                         setPopupOpen $ const Nothing
                         launchAff    $ d $ CreateSubmit name' nt
                     } [H.text "Add"]
          ]

createNodeView _ _ _ _ = R.createElement el {} []
  where
    el = R.hooksComponent "CreateNodeView" cpt
    cpt props _ = pure $ H.div {} []
-- END Create Node


showConfig :: NodeType -> R.Element
showConfig NodeUser      = H.div {} []
showConfig FolderPrivate = H.div {} [H.text "This folder will be private only"]
showConfig FolderShared  = H.div {} [H.text "This folder will be shared"]
showConfig FolderPublic  = H.div {} [H.text "This folder will be public"]
showConfig nt            = H.div {} [H.h4  {} [H.text $ "Config of " <> show nt ]]


