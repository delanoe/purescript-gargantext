module Gargantext.Components.Tree where

import Control.Monad.Cont.Trans (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Prelude (identity)
import React (ReactElement)
import React.DOM (a, button, div, h5, i, input, li, text, ul)
import React.DOM.Props (Props, _type, className, href, onClick, onInput, placeholder, style, value)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)
import Unsafe.Coerce (unsafeCoerce)

import Gargantext.Prelude
import Gargantext.Config.REST (get, put, post, delete)
import Gargantext.Config (NodeType(..), toUrl, End(..), defaultRoot)

type Name = String
type Open = Boolean
type URL  = String
type ID   = Int

data NTree a = NTree a (Array (NTree a))

type FTree = NTree LNode

data Action =  ShowPopOver
              | ToggleFolder ID
              | RenameNode  String
              | Submit
            -- | Initialize

type State = FTree

initialState :: State
initialState = NTree (LNode { id : 3
                            , name : ""
                            , nodeType : NodeUser
                            , open : true
                            , popOver : false
                            , renameNodeValue : ""
                          }) []


toggleNode :: Int -> NTree LNode -> NTree LNode
toggleNode sid (NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue}) ary) =
  NTree (LNode {id,name, nodeType, open : nopen, popOver, renameNodeValue}) $ map (toggleNode sid) ary
  where
    nopen = if sid == id then not open else open

------------------------------------------------------------------------
-- Realistic Tree for the UI

exampleTree :: NTree LNode
exampleTree = NTree (LNode { id : 1
                           , name : ""
                           , nodeType : NodeUser
                           , open : false
                           , popOver : false
                           , renameNodeValue : ""
                           }
                     ) []

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
--     [ NTree (Tuple "Tabs"    "#/corpus") []
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


nodeOptionsRename :: (Action -> Effect Unit) ->  Boolean -> Array ReactElement
nodeOptionsRename d activated =  case activated of
                         true -> [ a [className "glyphicon glyphicon-pencil", style {marginLeft : "15px"}
                                        , onClick $ (\_-> d $ ShowPopOver)
                                        ] []
                                 ]
                         false -> []



treeview :: Spec State {} Action
treeview = simpleSpec performAction render
  where
    performAction :: PerformAction State {} Action
    performAction (ToggleFolder i) _ _ =
      void $ modifyState (\td -> toggleNode i td)
    performAction ShowPopOver _ _ = void $
      modifyState $ \(NTree (LNode lnode) ary) -> NTree (LNode $ lnode { popOver = true }) ary
    performAction Submit _  s@(NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue}) ary)  = void $ do
      d <- lift $ renameNode  id  $ RenameValue { name : getRenameNodeValue s}
      modifyState identity -- TODO why ???
    performAction (RenameNode  r) _ _ = void $
      modifyState $ \(NTree (LNode lnode) ary) -> NTree (LNode $ lnode { renameNodeValue  = r }) ary
    -- performAction Initialize _ _ = void $ do
    --  s <- lift $ loadDefaultNode
    --  case s of
    --    Left err -> modifyState identity
    --    Right d -> modifyState (\state -> d)

    render :: Render State {} Action
    render dispatch _ state _ =
      [ div [className "tree"]
        [ toHtml dispatch state

        ]
      ]



renameTreeView :: (Action -> Effect Unit) -> State -> ReactElement
renameTreeView d s@(NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue }) ary) =
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
                    , onInput \e -> d (RenameNode (unsafeEventValue e))
                    ]
            ]
          , div [className "panel-footer"]
            [ button [className "btn btn-danger"
                     , _type "button"
                     , onClick \_ -> d $ Submit
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
toHtml d (NTree (LNode {id, name, nodeType : Folder, open, popOver, renameNodeValue}) []) =
  ul [ ]
  [ li [] $
    ( [ a [onClick $ (\e-> d $ ToggleFolder id)] [i [fldr open] []]
      ,  a [ href (toUrl Front Folder id )]
           [ text $ " " <> name <> " " ]
      ] <> nodeOptionsCorp false
    )
  ]
toHtml d s@(NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue}) []) =
  ul []
  [
    li [ style {width:"100%"}]
    [
      a [ href (toUrl Front nodeType id)]
      ( [ text (name <> "    ")
        ]
        <> nodeOptionsView false
        <> (nodeOptionsRename  d false)
        -- <>[ if ((popOverValue s) == true) then (renameTreeView d s ) else (renameTreeView d s)]
      )
    ]
  ]
--- need to add renameTreeview value to this function
toHtml d s@(NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue}) ary) =
  ul [ ]
  [ li [style {width : "100%"}] $
    ( [ a [onClick $ (\e-> d $ ToggleFolder id)] [i [fldr open] []]
      ,  a [ href (toUrl Front nodeType id )]
           [ text $ " " <> name <> " " ]
      ] <> nodeOptionsCorp false <>
      if open then
        map (toHtml d) ary
        else []
     <> nodeOptionsView false
     <> (nodeOptionsRename  d false)
     -- <>[ if ((popOverValue s) == true) then (renameTreeView d s ) else (renameTreeView d s)]
    )
  ]



fldr :: Boolean -> Props
fldr open = if open then className "fas fa-folder-open" else className "fas fa-folder"


newtype LNode = LNode { id       :: Int
                      , name     :: String
                      , nodeType :: NodeType
                      , open     :: Boolean
                      , popOver  :: Boolean
                      , renameNodeValue :: String
                    }

derive instance newtypeLNode :: Newtype LNode _

instance decodeJsonLNode :: DecodeJson LNode where
  decodeJson json = do
    obj <- decodeJson json
    id_ <- obj .? "id"
    name <- obj .? "name"
    nodeType <- obj .? "type"
    pure $ LNode { id : id_
                 , name
                 , nodeType
                 , open : true
                 , popOver : false
                 , renameNodeValue : ""
               }

instance decodeJsonNTree :: DecodeJson a => DecodeJson (NTree a) where
  decodeJson json = do
    obj <- decodeJson json
    node <- obj .? "node"
    nodes <- obj .? "children"
    node' <- decodeJson node
    nodes' <- decodeJson nodes
    pure $ NTree node' nodes'

loadDefaultNode :: Aff (NTree LNode)
loadDefaultNode = get $ toUrl Back Tree defaultRoot

----- TREE CRUD Operations

newtype RenameValue = RenameValue
  {
    name :: String
  }

instance encodeJsonRenameValue :: EncodeJson RenameValue where
  encodeJson (RenameValue post)
     = "name" := post.name
    ~> jsonEmptyObject


renameNode :: Int -> RenameValue -> Aff Int     --- need to change return type herre
renameNode renameNodeId reqbody =
  put ("http://localhost:8008/api/v1.0/node/" <> show renameNodeId <> "/rename")
      reqbody

deleteNode :: Int -> Aff Int
deleteNode = delete <<< toUrl Back Tree

-- See https://stackoverflow.com/questions/21863326/delete-multiple-records-using-rest
-- As of now I would recommend simply issuing many requests.
-- In a second time implement a set of end points for batch edition.
deleteNodes :: Array Int -> Aff (Array Int)
deleteNodes = traverse deleteNode

createNode :: String -> Aff Int
createNode reqbody = post (toUrl Back Tree 1) reqbody

fnTransform :: LNode -> FTree
fnTransform n = NTree n []

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value
