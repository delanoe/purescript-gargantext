module Gargantext.Components.Tree where

import Prelude hiding (div)

import Affjax (defaultRequest, printResponseFormatError, request)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat as ResponseFormat
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
import React.DOM.Props (Props, _id, _type, className, href, onClick, onInput, placeholder, style, value)
import Thermite (PerformAction, Render, Spec, cotransform, defaultPerformAction, defaultRender, modifyState, simpleSpec)
import Unsafe.Coerce (unsafeCoerce)

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

type State = FTree

initialState :: State
initialState = NTree (LNode {id : 3, name : "hello", nodeType : "", open : true, popOver : false, renameNodeValue : ""}) []



performAction :: PerformAction State {} Action

performAction (ToggleFolder i) _ _ = void $
 cotransform (\td -> toggleNode i td)



performAction (ShowPopOver id) _ _ = void $
 cotransform (\td -> popOverNode id td)


--- TODO : Need to update state once API is called
performAction (DeleteNode nid) _ _ = void $ do
  s' <- lift $ deleteNode nid
  case s' of
    Left err -> modifyState identity
    Right d -> modifyState identity


--- TODO : Need to update state once API is called
performAction (Submit rid s'') _  _  = void $ do
  s' <- lift $ renameNode  rid  $ RenameValue { name : s''}
  case s' of
    Left err -> modifyState identity
    Right d -> modifyState identity


performAction (RenameNode  r id) _ _ = void $
  cotransform (\td -> rename  id r td)


-- performAction Initialize _ _ = void $ do
--  s <- lift $ loadDefaultNode
--  case s of
--    Left err -> modifyState identity
--    Right d -> modifyState (\state -> d)

popOverNode :: Int -> NTree LNode -> NTree LNode
popOverNode sid (NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue}) ary) =
  NTree (LNode {id,name, nodeType, open , popOver : npopOver, renameNodeValue}) $ map (popOverNode sid) ary
  where
    npopOver = if sid == id then not popOver else popOver


rename :: Int ->  String -> NTree LNode  -> NTree LNode
rename sid v (NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue}) ary)  =
  NTree (LNode {id,name, nodeType, open , popOver , renameNodeValue : rvalue}) $ map (rename sid  v) ary
  where
    rvalue = if sid == id then  v   else ""


toggleNode :: Int -> NTree LNode -> NTree LNode
toggleNode sid (NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue}) ary) =
  NTree (LNode {id,name, nodeType, open : nopen, popOver, renameNodeValue}) $ map (toggleNode sid) ary
  where
    nopen = if sid == id then not open else open



------------------------------------------------------------------------
-- Realistic Tree for the UI

exampleTree :: NTree LNode
exampleTree = NTree (LNode {id : 1, name : "", nodeType : "", open : false, popOver : false, renameNodeValue : ""}) []

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
                                        , onClick $ (\_-> d $ (ShowPopOver id))
                                        ] []
                                 ]
                         false -> []



treeview :: Spec State {} Action
treeview = simpleSpec performAction render
  where
    render :: Render State {} Action
    render dispatch _ state _ =
      [ div [className "tree"]
        [ toHtml dispatch state

        ]
      ]



renameTreeView :: (Action -> Effect Unit) -> State -> Int -> ReactElement
renameTreeView d s@(NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue }) ary) nid  =
       div [className ""]
        [  div [className "panel panel-default"]
           [
             div [className "panel-heading"]
             [
               h5 [] [text "Rename Node"]
             ]
           ,div [className "panel-body"]
            [
              input [ _type "text"
                    , placeholder "Rename Node"
                    , value $ getRenameNodeValue s
                    , className "col-md-12 form-control"
                    , onInput \e -> d (RenameNode (unsafeEventValue e) nid)
                    ]
            ]
          , div [className "panel-footer"]
            [ button [className "btn btn-danger"
                     , _type "button"
                     , onClick \_ -> d $ (Submit nid renameNodeValue)
                     ] [text "Rename"]
            ]
          ]
        ]



renameTreeViewDummy :: (Action -> Effect Unit) -> State -> ReactElement
renameTreeViewDummy d s = div [] []

popOverValue :: State -> Boolean
popOverValue (NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue }) ary) = popOver

getRenameNodeValue :: State -> String
getRenameNodeValue (NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue }) ary) = renameNodeValue


toHtml :: (Action -> Effect Unit) -> FTree -> ReactElement
toHtml d s@(NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue}) []) =
  ul []
  [
    li [ style {width:"100%"}, _id "rename"]
    [

      a [ href "#"]
      ( [ text (name <> "    ")
        ]
      )
    ,  a [className "glyphicon glyphicon-pencil", _id "rename-a",onClick $ (\_-> d $ (ShowPopOver id))] [ ]
    ,  a [className "glyphicon glyphicon-trash", _id "rename-d",onClick $ (\_-> d $ (DeleteNode id))] [ ]
    , if (popOver == true) then (renameTreeView d s id) else (renameTreeViewDummy d s)
    ]
  ]
--- need to add renameTreeview value to this function
toHtml d s@(NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue}) ary) =
  ul [ ]
  [ li [style {width : "100%"}] $
    ( [ a [onClick $ (\e-> d $ ToggleFolder id)] [i [fldr open] []]
      ,  text $ " " <> name <> " "

      ] <>
      if open then
        map (toHtml d) ary
        else []
    )
  ]



fldr :: Boolean -> Props
fldr open = if open then className "fas fa-folder-open" else className "fas fa-folder"


newtype LNode = LNode {id :: Int, name :: String, nodeType :: String, open :: Boolean, popOver :: Boolean, renameNodeValue :: String}

derive instance newtypeLNode :: Newtype LNode _

instance decodeJsonLNode :: DecodeJson LNode where
  decodeJson json = do
    obj <- decodeJson json
    id_ <- obj .? "id"
    name <- obj .? "name"
    nodeType <- obj .? "type"
    pure $ LNode {id : id_, name, nodeType, open : true, popOver : false, renameNodeValue : ""}

instance decodeJsonFTree :: DecodeJson (NTree LNode) where
  decodeJson json = do
    obj <- decodeJson json
    node <- obj .? "node"
    nodes <- obj .? "children"
    node' <- decodeJson node
    nodes' <- decodeJson nodes
    pure $ NTree node' nodes'

loadDefaultNode :: Aff (Either String (NTree LNode))
loadDefaultNode = do
  res <- request $ defaultRequest
         { url = "http://localhost:8008/api/v1.0/tree/1"     --- http://localhost:8008/api/v1.0/tree/1
         , responseFormat = ResponseFormat.json
         , method = Left GET
         , headers = []
         }
  case res.body of
    Left err -> do
      _ <- liftEffect $ log $ printResponseFormatError err
      pure $ Left $ printResponseFormatError err
    Right json -> do
      --_ <- liftEffect $ log $ show a.status
      --_ <- liftEffect $ log $ show a.headers
      --_ <- liftEffect $ log $ show a.body
      let obj = decodeJson json
      pure obj

----- TREE CRUD Operations

newtype RenameValue = RenameValue
  {
    name :: String
  }

instance encodeJsonRenameValue :: EncodeJson RenameValue where
  encodeJson (RenameValue post)
     = "name" := post.name
    ~> jsonEmptyObject


renameNode :: Int -> RenameValue -> Aff (Either String (Int))     --- need to change return type herre
renameNode renameNodeId reqbody = do
  res <- request $ defaultRequest
         { url = "http://localhost:8008/api/v1.0/node/" <> show renameNodeId  <> "/rename"
         , responseFormat = ResponseFormat.json
         , method = Left PUT
         , headers = []
         , content  = Just $ Json $ encodeJson reqbody
         }
  case res.body of
    Left err -> do
      _ <- liftEffect $ log $ printResponseFormatError err
      pure $ Left $ printResponseFormatError err
    Right json -> do
      --_ <- liftEffect $ log $ show a.status
      --_ <- liftEffect $ log $ show a.headers
      --_ <- liftEffect $ log $ show a.body
      let obj = decodeJson json
      pure obj



deleteNode :: Int -> Aff (Either String (Int))
deleteNode renameNodeId = do
  res <- request $ defaultRequest
         { url = "http://localhost:8008/api/v1.0/node/" <> show renameNodeId
         , responseFormat = ResponseFormat.json
         , method = Left DELETE
         , headers = []
         }

  case res.body of
    Left err -> do
      _ <- liftEffect $ log $ printResponseFormatError err
      pure $ Left $ printResponseFormatError err
    Right json -> do
      --_ <- liftEffect $ log $ show a.status
      --_ <- liftEffect $ log $ show a.headers
      --_ <- liftEffect $ log $ show a.body
      let obj = decodeJson json
      pure obj



deleteNodes :: String -> Aff (Either String  Int)
deleteNodes reqbody = do
  res <- request $ defaultRequest
         { url = "http://localhost:8008/tree"
         , responseFormat = ResponseFormat.json
         , method = Left DELETE
         , headers = []
         , content = Just $ Json $ encodeJson reqbody
         }
  case res.body of
    Left err -> do
      _ <- liftEffect $ log $ printResponseFormatError err
      pure $ Left $ printResponseFormatError err
    Right json -> do
      --_ <- liftEffect $ log $ show a.status
      --_ <- liftEffect $ log $ show a.headers
      --_ <- liftEffect $ log $ show a.body
      let obj = decodeJson json
      pure  obj


createNode :: String -> Aff (Either String (Int))
createNode  reqbody= do
  res <- request $ defaultRequest
         { url = "http://localhost:8008/tree"
         , responseFormat = ResponseFormat.json
         , method = Left POST
         , headers = []
         , content = Just $ Json $ encodeJson reqbody
         }
  case res.body of
    Left err -> do
      _ <- liftEffect $ log $ printResponseFormatError err
      pure $ Left $ printResponseFormatError err
    Right json -> do
      --_ <- liftEffect $ log $ show a.status
      --_ <- liftEffect $ log $ show a.headers
      --_ <- liftEffect $ log $ show a.body
      let obj = decodeJson json
      pure obj



fnTransform :: LNode -> FTree
fnTransform n = NTree n []


unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value
