module Gargantext.Components.Tree where

import Prelude hiding (div)

import Affjax (defaultRequest, printResponseFormatError, request)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Cont.Trans (lift)
import Data.Argonaut (class DecodeJson, Json, decodeJson, encodeJson, (.?))
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import React (ReactElement)
import React.DOM (a, div, i, li, text, ul)
import React.DOM.Props (Props, className, href, onClick)
import Thermite (PerformAction, Render, Spec, cotransform, modifyState, simpleSpec)

type Name = String
type Open = Boolean
type URL  = String
type ID   = Int

data NTree a = NTree a (Array (NTree a))

type FTree = NTree LNode

data Action = ToggleFolder ID --| Initialize

type State = FTree

initialState :: State
initialState = NTree (LNode {id : 1, name : "", nodeType : "", open : true}) []

performAction :: PerformAction State {} Action
performAction (ToggleFolder i) _ _ = void $
 cotransform (\td -> toggleNode i td)

-- performAction Initialize _ _ = void $ do
--  s <- lift $ loadDefaultNode
--  case s of
--    Left err -> modifyState identity
--    Right d -> modifyState (\state -> d)


toggleNode :: Int -> NTree LNode -> NTree LNode
toggleNode sid (NTree (LNode {id, name, nodeType, open}) ary) =
  NTree (LNode {id,name, nodeType, open : nopen}) $ map (toggleNode sid) ary
  where
    nopen = if sid == id then not open else open



------------------------------------------------------------------------
-- Realistic Tree for the UI

exampleTree :: NTree LNode
exampleTree = NTree (LNode {id : 1, name : "", nodeType : "", open : false}) []

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


treeview :: Spec State {} Action
treeview = simpleSpec performAction render
  where
    render :: Render State {} Action
    render dispatch _ state _ =
      [div [className "tree"] [toHtml dispatch state]]


toHtml :: (Action -> Effect Unit) -> FTree -> ReactElement
toHtml d (NTree (LNode {id, name, nodeType, open}) []) =
  li []
  [ a [ href "#"]
    ( [ text (name <> "    ")
      ] <> nodeOptionsView false
    )
  ]
toHtml d (NTree (LNode {id, name, nodeType, open}) ary) =
  ul [ ]
  [ li [] $
    ( [ a [onClick $ (\e-> d $ ToggleFolder id)] [i [fldr open] []]
      ,  text $ " " <> name <> "    "
      ] <> nodeOptionsCorp false <>
      if open then
        map (toHtml d) ary
        else []
    )
  ]

fldr :: Boolean -> Props
fldr open = if open then className "fas fa-folder-open" else className "fas fa-folder"


newtype LNode = LNode {id :: Int, name :: String, nodeType :: String, open :: Boolean}

derive instance newtypeLNode :: Newtype LNode _

instance decodeJsonLNode :: DecodeJson LNode where
  decodeJson json = do
    obj <- decodeJson json
    id_ <- obj .? "id"
    name <- obj .? "name"
    nodeType <- obj .? "type"
    pure $ LNode {id : id_, name, nodeType, open : true}

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
         { url = "http://localhost:8008/tree/1"
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

renameNode :: Aff (Either String (Int))     --- need to change return type herre
renameNode = do
  res <- request $ defaultRequest
         { url = "http://localhost:8008/tree/1"
         , responseFormat = ResponseFormat.json
         , method = Left PUT
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



deleteNode :: Aff (Either String (Int))
deleteNode = do
  res <- request $ defaultRequest
         { url = "http://localhost:8008/tree/1"
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
