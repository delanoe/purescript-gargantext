module Gargantext.Components.Forest.Tree.Node.Action.Add where

import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))
import Data.Array (head, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff_)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Prelude

import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Settings (SettingsBox(..), settingsBox)
import Gargantext.Components.Forest.Tree.Node.Tools (formChoiceSafe', panel, submitButton)
import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Components.Lang (Lang(..), translate)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, post)
import Gargantext.Types (NodeType(..), charCodeIcon)
import Gargantext.Types as GT
import Gargantext.Utils (nbsp)
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Add"

addNode :: Session -> GT.ID -> AddNodeValue -> Aff (Array GT.ID)
addNode session parentId = post session $ GR.NodeAPI GT.Node (Just parentId) ""

addNodeAsync :: Session
             -> GT.ID
             -> AddNodeValue
             -> Aff GT.AsyncTaskWithType
addNodeAsync session parentId q = do
  task <- post session p q
  pure $ GT.AsyncTaskWithType {task, typ: GT.AddNode}
  where p = GR.NodeAPI GT.Node (Just parentId) (GT.asyncTaskTypePath GT.AddNode)

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

addNodeView :: R2.Component CreateNodeProps
addNodeView = R.createElement addNodeViewCpt
addNodeViewCpt :: R.Component CreateNodeProps
addNodeViewCpt = here.component "addNodeView" cpt where
  cpt { dispatch
      , id
      , name
      , nodeTypes } _ = do
    nodeName <- T.useBox "Name"
    nodeName' <- T.useLive T.unequal nodeName
    nodeType <- T.useBox $ fromMaybe Folder $ head nodeTypes
    nodeType' <- T.useLive T.unequal nodeType

    let
        print nt = charCodeIcon nt
                <> nbsp 4
                <> translate EN nt -- @TODO "EN" assumption

        SettingsBox {edit} = settingsBox nodeType'
        setNodeType' nt = do
          T.write_ (GT.prettyNodeType nt) nodeName
          T.write_ nt nodeType
        (maybeChoose /\ nt') = if length nodeTypes > 1
                         then ([ formChoiceSafe' nodeTypes Error setNodeType' print ] /\ nodeType')
                         else ([H.div {} [H.text $ "Creating a node of type "
                                                <> show defaultNt
                                                <> " with name:"
                                         ]
                                ] /\ defaultNt
                               )
                            where
                              defaultNt = (fromMaybe Error $ head nodeTypes)
        maybeEdit   = [ if edit
                        then inputWithEnter {
                            onBlur: \val -> T.write_ val nodeName
                          , onEnter: \_ -> launchAff_ $ dispatch (AddNode nodeName' nt')
                          , onValueChanged: \val -> T.write_ val nodeName
                          , autoFocus: true
                          , className: "form-control"
                          , defaultValue: nodeName' -- (prettyNodeType nt')
                          , placeholder: nodeName'  -- (prettyNodeType nt')
                          , type: "text"
                          }
                        else H.div {} []
                      ]

    pure $ panel (maybeChoose <> maybeEdit) (submitButton (AddNode nodeName' nt') dispatch)

-- END Create Node

showConfig :: NodeType -> R.Element
showConfig NodeUser      = H.div {} []
showConfig FolderPrivate = H.div {} [H.text "This folder will be private only"]
showConfig FolderShared  = H.div {} [H.text "This folder will be shared"]
showConfig FolderPublic  = H.div {} [H.text "This folder will be public"]
showConfig nt            = H.div {} [H.h4  {} [H.text $ "Config of " <> show nt ]]
