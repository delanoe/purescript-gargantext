module Gargantext.Components.Forest.Tree.Node.Action.Add where

import Data.Array (length, head)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Maybe (Maybe(..), fromMaybe)
-- import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff)
import Effect.Uncurried (mkEffectFn1)
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node (SettingsBox(..), settingsBox)
import Gargantext.Types (NodeType(..), readNodeType)
import Gargantext.Utils.Reactix as R2
import Gargantext.Sessions (Session, post)
import Gargantext.Routes as GR
import Gargantext.Types  as GT
import Prelude (Unit, bind, const, discard, map, pure, show, ($), (<>), (>), (<<<))
import Reactix as R
import Reactix.DOM.HTML as H

----------------------------------------------------------------------
addNode :: Session -> GT.ID -> AddNodeValue -> Aff (Array GT.ID)
addNode session parentId = post session $ GR.NodeAPI GT.Node (Just parentId) ""

addNodeAsync :: Session
                -> GT.ID
                -> AddNodeValue
                -> Aff GT.AsyncTaskWithType
addNodeAsync session parentId q = do
  task <- post session p q
  pure $ GT.AsyncTaskWithType {task, typ: GT.AddNode}
  where
    p = GR.NodeAPI GT.Node (Just parentId) (GT.asyncTaskTypePath GT.AddNode)

----------------------------------------------------------------------
-- TODO AddNodeParams
newtype AddNodeValue = AddNodeValue
  { name     :: GT.Name
  , nodeType :: GT.NodeType
  }

instance encodeJsonAddNodeValue :: EncodeJson AddNodeValue where
  encodeJson (AddNodeValue {name, nodeType})
     = "pn_name"     := name
    ~> "pn_typename" := nodeType
    ~> jsonEmptyObject

----------------------------------------------------------------------
data NodePopup = CreatePopup | NodePopup

type CreateNodeProps =
  ( id        :: GT.ID
  , dispatch  :: Action -> Aff Unit
  , name      :: GT.Name
  , nodeType  :: NodeType
  , nodeTypes :: Array NodeType
  )

addNodeView :: Record CreateNodeProps
               -> R.Element
addNodeView p@{ dispatch, nodeType, nodeTypes } = R.createElement el p []
  where
    el = R.hooksComponent "AddNodeView" cpt
    cpt {id, name} _ = do
      nodeName <- R.useState' "Name"
      nodeType' <- R.useState' $ fromMaybe NodeUser $ head nodeTypes
      pure $ H.div {}
          [ panelBody   readNodeType nodeName nodeType'
          , panelFooter nodeName nodeType'
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
                maybeEdit = [ if edit then
                                H.div {className: "form-group"}
                                   [ H.input { type: "text"
                                             , placeholder: "Node name"
                                             , defaultValue: "Write Name here"
                                             , className: "form-control"
                                             , onInput: mkEffectFn1 $ setNodeName <<< const <<< R2.unsafeEventValue
                                            }
                                   ]
                              else
                                H.div {} []
                             ]

                maybeChoose = [ if length nodeTypes > 1 then
                                  R.fragment [
                                    H.div {className: "form-group"} $ [
                                       R2.select { className: "form-control"
                                                 , onChange: mkEffectFn1 $ setNodeType <<< const <<< readIt <<< R2.unsafeEventValue
                                                 }
                                       (map (\opt -> H.option {} [ H.text $ show opt ]) nodeTypes)
                                         ]
                                         -- , showConfig nt
                                    ]
                                else
                                H.button { className : "btn btn-primary flex-center"
                                           , type : "button"
                                           , onClick : mkEffectFn1 $ \_ -> setNodeType ( const
                                                                                       $ fromMaybe nt 
                                                                                       $ head nodeTypes
                                                                                       )
                                           } []
                               ]



        panelFooter :: R.State String  -> R.State NodeType -> R.Element
        panelFooter (name' /\ _) (nt /\ _) =
          H.div { className: "panel-footer"}
                [ H.div {} []
                , H.div {className: "flex-center"} 
                        [ H.button {className: "btn btn-primary"
                           , type: "button"
                           , style    : { width: "100%" }
                           , onClick: mkEffectFn1 $ \_ -> do
                               -- TODO
                               --setPopupOpen $ const Nothing
                               launchAff    $ dispatch $ AddNode name' nt
                           } [H.text "Add"]
                         ]
                ]

-- END Create Node

showConfig :: NodeType -> R.Element
showConfig NodeUser      = H.div {} []
showConfig FolderPrivate = H.div {} [H.text "This folder will be private only"]
showConfig FolderShared  = H.div {} [H.text "This folder will be shared"]
showConfig FolderPublic  = H.div {} [H.text "This folder will be public"]
showConfig nt            = H.div {} [H.h4  {} [H.text $ "Config of " <> show nt ]]

