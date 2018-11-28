module Gargantext.Components.Tree where

import Prelude hiding (div)
import Unsafe.Coerce

import Affjax (defaultRequest, printResponseFormatError, request)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat as ResponseFormat
import CSS (backgroundColor, borderRadius, boxShadow, justifyContent, marginTop)
import Control.Monad.Cont.Trans (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Prelude (identity)
import React (ReactElement)
import React.DOM (a, button, div, h5, i, input, li, span, text, ul)
import React.DOM.Props (_id, _type, className, href, title, onClick, onInput, placeholder, style, defaultValue, _data)
import React.DOM.Props as DOM
import Thermite (PerformAction, Render, Spec, createClass, defaultPerformAction, defaultRender, modifyState_, simpleSpec)

import Gargantext.Config (toUrl, End(..), NodeType(..), defaultRoot)
import Gargantext.Config.REST (get, put, post, delete, deleteWithBody)

type Name = String
type Open = Boolean
type URL  = String
type ID   = Int

data NTree a = NTree a (Array (NTree a))

type FTree = NTree LNode

data Action =  ShowPopOver ID
              | ToggleFolder ID
              | RenameNode  String ID
              | Submit ID String
            --| Initialize
              | DeleteNode ID
              | Create  ID
              | SetNodeValue String ID
              | ToggleCreateNode ID
              | ShowRenameBox ID
              | CancelRename ID


type State = { state :: FTree }

initialState :: State
initialState = { state: NTree (LNode {id : 3, name : "hello", nodeType : Node, open : true, popOver : false, renameNodeValue : "", createNode : false, nodeValue : "InitialNode", showRenameBox : false}) [] }

mapFTree :: (FTree -> FTree) -> State -> State
mapFTree f {state} = {state: f state}


performAction :: forall props. PerformAction State props Action

performAction (ToggleFolder i) _ _ =
  modifyState_ $ mapFTree $ toggleNode i

performAction (ShowPopOver id) _ _ =
  modifyState_ $ mapFTree $ popOverNode id

performAction (ShowRenameBox id) _ _ =
  modifyState_ $ mapFTree $ showPopOverNode id

performAction (CancelRename id) _ _ =
  modifyState_ $ mapFTree $ showPopOverNode id

performAction (ToggleCreateNode id) _ _ =
  modifyState_ $ mapFTree $ showCreateNode id

performAction (DeleteNode nid) _ _ = do
  d <- lift $ deleteNode nid
  --- TODO : Need to update state once API is called
  pure unit

--- TODO : Need to update state once API is called
performAction (Submit rid s'') _  _  = do
  d <- lift $ renameNode rid $ RenameValue { name : s''}
  -- modifyState_ $ mapFTree $ popOverNode rid
  modifyState_ $ mapFTree $ showPopOverNode rid -- add this function to toggle rename function

performAction (RenameNode  r nid) _ _ =
  modifyState_ $ mapFTree $ rename nid r

performAction (Create  nid) _ _ =
  modifyState_ $ mapFTree $ showCreateNode nid

performAction (SetNodeValue v nid) _ _ =
  modifyState_ $ mapFTree $ setNodeValue nid v


popOverNode :: Int -> NTree LNode -> NTree LNode
popOverNode sid (NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue, createNode, nodeValue, showRenameBox}) ary) =
  NTree (LNode {id,name, nodeType, open , popOver : npopOver, renameNodeValue, createNode, nodeValue, showRenameBox}) $ map (popOverNode sid) ary
  where
    npopOver = if sid == id then not popOver else popOver


showPopOverNode :: Int -> NTree LNode -> NTree LNode
showPopOverNode sid (NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue, createNode, nodeValue, showRenameBox}) ary) =
  NTree (LNode {id,name, nodeType, open , popOver , renameNodeValue, createNode, nodeValue, showRenameBox: nshowRenameBox}) $ map (showPopOverNode sid) ary
  where
    nshowRenameBox = if sid == id then not showRenameBox else showRenameBox


showCreateNode :: Int -> NTree LNode -> NTree LNode
showCreateNode sid (NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue, createNode, nodeValue, showRenameBox}) ary) =
  NTree (LNode {id,name, nodeType, open , popOver, renameNodeValue, createNode : createNode', nodeValue, showRenameBox}) $ map (showCreateNode sid) ary
  where
    createNode' = if sid == id then not createNode else createNode

----TODO get id and value to send API to call

-- getCreateNode :: Int -> NTree LNode -> String
-- getCreateNode sid (NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue, createNode, nodeValue}) ary) =
--   createNode
--   where
--     NTree (LNode {id,name, nodeType, open , popOver, renameNodeValue, createNode , nodeValue}) $ map (getCreateNode sid) ary
--     createNode' = if sid == id then  nodeValue else ""


rename :: Int ->  String -> NTree LNode  -> NTree LNode
rename sid v (NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue, createNode, nodeValue, showRenameBox}) ary)  =
  NTree (LNode {id,name, nodeType, open , popOver , renameNodeValue : rvalue, createNode, nodeValue, showRenameBox}) $ map (rename sid  v) ary
  where
    rvalue = if sid == id then  v   else ""


setNodeValue :: Int ->  String -> NTree LNode  -> NTree LNode
setNodeValue sid v (NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue, createNode, nodeValue, showRenameBox}) ary)  =
  NTree (LNode {id,name, nodeType, open , popOver , renameNodeValue , createNode, nodeValue : nvalue, showRenameBox}) $ map (setNodeValue sid  v) ary
  where
    nvalue = if sid == id then  v   else ""


toggleNode :: Int -> NTree LNode -> NTree LNode
toggleNode sid (NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue, createNode, nodeValue, showRenameBox}) ary) =
  NTree (LNode {id,name, nodeType, open : nopen, popOver, renameNodeValue, createNode, nodeValue, showRenameBox}) $ map (toggleNode sid) ary
  where
    nopen = if sid == id then not open else open



------------------------------------------------------------------------
-- Realistic Tree for the UI

exampleTree :: NTree LNode
exampleTree = NTree (LNode {id : 1, name : "", nodeType : Node, open : false, popOver : false, renameNodeValue : "", createNode : false, nodeValue : "", showRenameBox : false}) []

-- exampleTree :: NTree LNode
-- exampleTree =
--   NTree 1 true "françois.pineau"
--   [ --annuaire 2 "Annuaire"
--   --, corpus   3 "IMT publications"
--   ]

-- annuaire :: Int -> String -> NTree (Tuple String String)
-- annuaire n name = NTree n false name
--     [ NTree (Tuple "IMT community"    "#/docView")
--     ]

-- corpus :: Int -> String -> NTree (Tuple String String)
-- corpus n name = NTree (LNode {id : n, name, nodeType : "", open : false})
--     [ NTree (Tuple "Facets"    "#/corpus") []
--     , NTree (Tuple "Dashboard" "#/dashboard") []
--     , NTree (Tuple "Graph"     "#/graphExplorer") []
--     ]


------------------------------------------------------------------------
-- TODO
-- alignment to the right
nodeOptionsCorp :: Boolean -> Array ReactElement
nodeOptionsCorp activated = case activated of
                         true  -> [ i [className "fab fa-whmcs" ] []]
                         false -> []

-- TODO
-- alignment to the right
-- on hover make other options available:
nodeOptionsView :: Boolean -> Array ReactElement
nodeOptionsView activated = case activated of
                         true -> [ i [className "fas fa-sync-alt" ] []
                                 , i [className "fas fa-upload"   ] []
                                 , i [className "fas fa-share-alt"] []
                                 ]
                         false -> []


nodeOptionsRename :: (Action -> Effect Unit) ->  Boolean ->  ID -> Array ReactElement
nodeOptionsRename d activated  id =  case activated of
                         true -> [ a [className "glyphicon glyphicon-pencil", style {marginLeft : "15px"}
                                        
                                        ] []
                                 ]
                         false -> []



treeview :: Spec State {} Action
treeview = simpleSpec performAction render
  where
    render :: Render State {} Action
    render dispatch _ {state} _ =
      [ div [className "tree"]
        [ toHtml dispatch state

        ]
      ]



renameTreeView :: (Action -> Effect Unit) -> FTree -> Int -> ReactElement
renameTreeView d s@(NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue, showRenameBox }) ary) nid  =
        
       div [className "col-md-12", _id "rename-tooltip",className "btn btn-secondary", _data {toggle  : "tooltip", placement : "right"}, title "Settings on right"]
       [  div [_id "arrow"] []
       , div [className "panel panel-default", style {border:"1px solid rgba(0,0,0,0.2)", boxShadow : "0 2px 5px rgba(0,0,0,0.2)"}]
           [
             div [className "panel-heading", style {float:"left", width: "100%"}]
             [
               if (showRenameBox) then div [_id "afterClick"] 
               [ 
                 div [className "col-md-12"] 
               [
                 input [ _type "text"
                    , placeholder "Rename Node"
                    , defaultValue $ getRenameNodeValue s
                    , style {float: "left"}
                    , className "col-md-2 form-control"
                    , onInput \e -> d (RenameNode (unsafeEventValue e) nid)
                    ]
               ]
              , div [className "col-md-12"] 
              [ div [className "row", style {marginTop : "11px"}]
                [ div [className "col-md-6"] [
                     a [className "btn btn-danger"
                    , _type "button"
                    , onClick \_ -> d $ (Submit nid renameNodeValue)
                    , style {float:"left"}
                    ] [text "Rename"]
                    ]
                , div [className "col-md-6"] 
                  [a [className "btn btn-primary"
                     , _type "button"
                     , onClick \_ -> d $ (CancelRename nid)
                     , style {float:"left", backgroundColor: "white", color:"black"}
                     ] [text "cancel"]

                  ]
                ]
              
                ]
               
            ]
              else 
                div [ _id "beforeClick", className "col-md-12"] 
             [  div [className "row"]
                [ div [className "col-md-6"] [text name] 
                , a [ style {color:"black"},className "glyphitem glyphicon glyphicon-pencil col-md-2", _id "rename1", title "Rename", onClick $ (\_-> d $ (ShowRenameBox id))] [ ]
                ]
             ]
             ]
           ,div [className "panel-body", style {display:"flex", justifyContent : "center", backgroundColor: "white", border: "none"}]
            [   div [className "col-md-4"] [a [ style {color:"black", paddingTop: "6px", paddingBottom: "6px"},className "glyphitem glyphicon glyphicon-download-alt", _id "rename1", title "Download [WIP]"] [ ]]
           , div [className "col-md-4"] [a [ style {color:"black", paddingTop: "6px", paddingBottom: "6px"},className "glyphitem glyphicon glyphicon-duplicate", _id "rename1", title "Duplicate [WIP]"] [ ]]
           ,  div [className "col-md-4"] [ a [style {color:"black", paddingTop: "6px", paddingBottom: "6px"}, className "glyphitem glyphicon glyphicon-trash", _id "rename2",title "Delete", onClick $ (\_-> d $ (DeleteNode id))] [ ]]

           ]
          
          ]
        ]
       


createNodeView :: (Action -> Effect Unit) -> FTree -> Int -> ReactElement
createNodeView d s@(NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue }) ary) nid  =
       div [className ""]
        [  div [className "panel panel-default"]
           [
             div [className "panel-heading"]
             [
               h5 [] [text "Create Node"]
             ]
           ,div [className "panel-body"]
            [
              input [ _type "text"
                    , placeholder "Create Node"
                    , defaultValue $ getCreateNodeValue s
                    , className "col-md-12 form-control"
                    , onInput \e -> d (SetNodeValue (unsafeEventValue e) nid)
                    ]
            ]
          , div [className "panel-footer"]
            [ button [className "btn btn-success"
                     , _type "button"
                     , onClick \_ -> d $ (Create nid )
                     ] [text "Create"]
            ]
          ]
        ]



renameTreeViewDummy :: (Action -> Effect Unit) -> FTree -> ReactElement
renameTreeViewDummy d s = div [] []

popOverValue :: FTree -> Boolean
popOverValue (NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue, showRenameBox }) ary) = popOver

getRenameNodeValue :: FTree -> String
getRenameNodeValue (NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue, showRenameBox }) ary) = renameNodeValue


getCreateNodeValue :: FTree -> String
getCreateNodeValue (NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue, nodeValue, showRenameBox}) ary) = nodeValue


toHtml :: (Action -> Effect Unit) -> FTree -> ReactElement
toHtml d s@(NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue, createNode,nodeValue, showRenameBox }) []) =
  ul []
  [
    li [] $
    [
     a [onClick $ (\e-> d $ ToggleFolder id)] [i [fldr open] []]
      , a [ href (toUrl Front nodeType (Just id)), style {position:"absolute",left:"44px"}]
      ( [ text (name <> "    ")
        ]
      )
    
    ,  a [className "glyphicon glyphicon-cog", _id "rename",onClick $ (\_-> d $ (ShowPopOver id))]
       [ 
       ]
     , if (popOver == true) then (renameTreeView d s id) else (renameTreeViewDummy d s)
    , if (createNode == true) then (createNodeView d s id) else (renameTreeViewDummy d s)
    ]
  ]
--- need to add renameTreeview value to this function
toHtml d s@(NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue,createNode, nodeValue, showRenameBox}) ary) =
    ul []
  [ li [] $
    ( [ a [onClick $ (\e-> d $ ToggleFolder id)] [i [fldr open] []]
      ,  a [ href (toUrl Front nodeType (Just id)), style {position:"absolute",left:"44px"}]
         [ text $ " " <> name <> " " ]
,      a [className "glyphicon glyphicon-cog", _id "rename",onClick $ (\_-> d $ (ShowPopOver id))]
       [ 
       ]
     , if (popOver == true) then (renameTreeView d s id) else (renameTreeViewDummy d s)
    , if (createNode == true) then (createNodeView d s id) else (renameTreeViewDummy d s)

      ] <>
      if open then
        map (toHtml d) ary
        else []
    )
  ]



fldr :: Boolean -> DOM.Props
fldr open = if open then className "fas fa-folder-open" else className "fas fa-folder"


newtype LNode = LNode {id :: Int, name :: String, nodeType :: NodeType, open :: Boolean, popOver :: Boolean, renameNodeValue :: String, nodeValue :: String, createNode :: Boolean, showRenameBox :: Boolean}

derive instance newtypeLNode :: Newtype LNode _

instance decodeJsonLNode :: DecodeJson LNode where
  decodeJson json = do
    obj <- decodeJson json
    id_ <- obj .? "id"
    name <- obj .? "name"
    nodeType <- obj .? "type"
    pure $ LNode {id : id_, name, nodeType, open : true, popOver : false, renameNodeValue : "", createNode : false, nodeValue : "", showRenameBox : false}

instance decodeJsonFTree :: DecodeJson (NTree LNode) where
  decodeJson json = do
    obj <- decodeJson json
    node <- obj .? "node"
    nodes <- obj .? "children"
    node' <- decodeJson node
    nodes' <- decodeJson nodes
    pure $ NTree node' nodes'

loadNode :: Int -> Aff FTree
loadNode = get <<< toUrl Back Tree <<< Just

----- TREE CRUD Operations

newtype RenameValue = RenameValue
  {
    name :: String
  }

instance encodeJsonRenameValue :: EncodeJson RenameValue where
  encodeJson (RenameValue post)
     = "r_name" := post.name
    ~> jsonEmptyObject

renameNode :: Int -> RenameValue -> Aff (Array Int)
renameNode renameNodeId = put $ toUrl Back Node (Just renameNodeId) <> "/rename"

deleteNode :: Int -> Aff Int
deleteNode = delete <<< toUrl Back Node <<< Just

-- UNUSED
-- deleteNodes :: TODO -> Aff Int
-- deleteNodes = deleteWithBody (toUrl Back Nodes Nothing)

-- UNUSED
-- createNode :: TODO -> Aff Int
-- createNode = post (toUrl Back Node Nothing)

fnTransform :: LNode -> FTree
fnTransform n = NTree n []

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value
