module Gargantext.Components.Forest.Tree.Node.Action.Add where

import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))
import Data.Array (head)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Gargantext.Components.Forest.Tree.Node.Settings (SettingsBox(..), settingsBox)
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Tools (submitButton, formEdit, formChoiceSafe)
import Gargantext.Prelude (Unit, bind, pure, show, ($), (<>))
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, post)
import Gargantext.Types  as GT
import Gargantext.Types (NodeType(..))
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
      nodeName@(name' /\ _) <- R.useState' "Name"
      nodeType'@(nt /\ _)  <- R.useState' $ fromMaybe NodeUser $ head nodeTypes
      pure $ H.div {}
          [ panelBody nodeName nodeType'
          , submitButton (AddNode name' nt) dispatch -- panelFooter nodeName nodeType'
          ]
      where
        panelBody :: R.State String
                  -> R.State NodeType
                  -> R.Element
        panelBody (_ /\ setNodeName) (nt /\ setNodeType) =
          H.div {className: "panel-body"}
          [ H.div { className: "row"
                  , style: {"margin":"10px"}
                  }
                  [ H.div { className: "col-md-10" }
                          [ H.form {className: "form-horizontal"}
                          $ maybeChoose <> maybeEdit 
                          ]
                  ]
            ]
              where
                maybeChoose = [ formChoiceSafe nodeTypes Error setNodeType ]

                SettingsBox {edit} = settingsBox nt
                maybeEdit = [ if edit
                                then formEdit "Node Name" setNodeName
                                else H.div {} []
                            ]


-- END Create Node

showConfig :: NodeType -> R.Element
showConfig NodeUser      = H.div {} []
showConfig FolderPrivate = H.div {} [H.text "This folder will be private only"]
showConfig FolderShared  = H.div {} [H.text "This folder will be shared"]
showConfig FolderPublic  = H.div {} [H.text "This folder will be public"]
showConfig nt            = H.div {} [H.h4  {} [H.text $ "Config of " <> show nt ]]

