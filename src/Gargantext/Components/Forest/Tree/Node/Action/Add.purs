module Gargantext.Components.Forest.Tree.Node.Action.Add where

import Gargantext.Prelude

import Data.Array (head, length)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), indexOf)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(..))
import Gargantext.Components.Forest.Tree.Node.Settings (SettingsBox(..), settingsBox)
import Gargantext.Components.Forest.Tree.Node.Tools (formChoice, panel, submitButton)
import Gargantext.Components.InputWithEnter (inputWithEnterWithKey)
import Gargantext.Components.Lang (Lang(..), translate)
import Gargantext.Config.REST (RESTError, AffRESTError)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, post)
import Gargantext.Types (NodeType(..), charCodeIcon)
import Gargantext.Types as GT
import Gargantext.Utils (nbsp)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON
import Toestand as T
import Web.HTML (window)
import Web.HTML.Navigator (userAgent)
import Web.HTML.Window (navigator)

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Add"

addNode :: Session -> GT.ID -> AddNodeValue -> AffRESTError (Array GT.ID)
addNode session parentId = post session $ GR.NodeAPI GT.Node (Just parentId) ""

addNodeAsync :: Session
             -> GT.ID
             -> AddNodeValue
             -> AffRESTError GT.AsyncTaskWithType
addNodeAsync session parentId q = do
  eTask :: Either RESTError GT.AsyncTask <- post session p q
  case eTask of
    Left err -> pure $ Left err
    Right task -> pure $ Right $ GT.AsyncTaskWithType { task, typ: GT.AddNode }
  where
    p = GR.NodeAPI GT.Node (Just parentId) (GT.asyncTaskTypePath GT.AddNode)

----------------------------------------------------------------------
-- TODO AddNodeParams
newtype AddNodeValue = AddNodeValue
  { name     :: GT.Name
  , nodeType :: GT.NodeType
  }
derive instance Generic AddNodeValue _
derive instance Newtype AddNodeValue _
instance JSON.WriteForeign AddNodeValue where
  writeImpl (AddNodeValue {name, nodeType}) = JSON.writeImpl { pn_name: name
                                                             , pn_typename: nodeType }

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
      , nodeTypes } _ = do
    let defaultNodeType = fromMaybe Folder $ head nodeTypes
    nodeName <- T.useBox $ GT.prettyNodeType defaultNodeType
    nodeName' <- T.useLive T.unequal nodeName
    nodeType <- T.useBox defaultNodeType
    nodeType' <- T.useLive T.unequal nodeType

    hasChromeAgent' <- R.unsafeHooksEffect hasChromeAgent

    let
        SettingsBox {edit} = settingsBox nodeType'
        setNodeType' nt = do
          T.write_ (GT.prettyNodeType nt) nodeName
          T.write_ nt nodeType
        (maybeChoose /\ nt') = if length nodeTypes > 1
                         then ([ formChoice { items: nodeTypes
                                            , default: Error
                                            , callback: setNodeType'
                                            , print: print hasChromeAgent' } [] ] /\ nodeType')
                         else ([H.div {} [H.text $ "Creating a node of type "
                                                <> show defaultNt
                                                <> " with name:"
                                         ]
                                ] /\ defaultNt
                               )
                            where
                              defaultNt = (fromMaybe Error $ head nodeTypes)
        maybeEdit   = [ if edit
                        then inputWithEnterWithKey {
                            onBlur: \val -> T.write_ val nodeName
                          , onEnter: \_ -> launchAff_ $ dispatch (AddNode nodeName' nt')
                          , onValueChanged: \val -> T.write_ val nodeName
                          , autoFocus: true
                          , className: "form-control"
                          , defaultValue: nodeName' -- (prettyNodeType nt')
                          , placeholder: nodeName'  -- (prettyNodeType nt')
                          , type: "text"
                          , key: show nodeType'
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

-- (?) Regarding `print` and `hasChromeAgent`
--
--  As described in #309:
--    * while sticking to solution a) for icon display, it only works on
--      Chrome engine
--    * for now, we just patch surgery the like of display according to the
--      user browser (ie. has Chrome -> has icons)

print :: Boolean -> NodeType -> String
print withIconFlag nt =
  let txt = translate EN -- @TODO "EN" assumption

  in if withIconFlag

  then
    charCodeIcon nt true
    --- as we are printing within an HTML text node,
    -- margins will directly rely on content text spacing
    <> nbsp 4
    <> txt nt

  else
    txt nt

hasChromeAgent :: Effect Boolean
hasChromeAgent = window >>= navigator >>= userAgent >>= \ua -> pure $ check ua
  where
    check = indexOf (Pattern "Chrome") >>> isJust
