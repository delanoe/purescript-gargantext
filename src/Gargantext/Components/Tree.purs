module Gargantext.Components.Tree where

import Prelude hiding (div)

import Control.Monad.Cont.Trans (lift)
import DOM.Simple.Console (log2)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Array (filter)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, runAff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throwException)
import Effect.Uncurried (mkEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import FFI.Simple ((..), (.=))
import Gargantext.Components.Loader as Loader
import Gargantext.Config (toUrl, End(..), NodeType(..), readNodeType)
import Gargantext.Config.REST (get, put, post, postWwwUrlencoded, delete)
import Gargantext.Types (class ToQuery, toQuery)
import Gargantext.Utils (id)
import Gargantext.Utils.Reactix as R2
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReactElement)
import React as React
import React.DOM (a, div, i, input, li, span, text, ul, b, u)
import React.DOM.Props (_id, _type, className, href, title, onClick, onDrop, onDragOver, onInput, placeholder, style, defaultValue, _data)
import React.DOM.Props as DOM
import React.SyntheticEvent as E
import Reactix as R
import Reactix.DOM.HTML as H
import Thermite (PerformAction, Render, Spec, createClass, defaultPerformAction, simpleSpec, modifyState_)
import URI.Extra.QueryPairs as QP
import URI.Query as Q
import Unsafe.Coerce (unsafeCoerce)
import Web.File.File (toBlob)
import Web.File.FileList (FileList, item)
import Web.File.FileReader.Aff (readAsText)

type Name = String
type Open = Boolean
type URL  = String
type ID   = Int

type Props = { root :: ID }

data NTree a = NTree a (Array (NTree a))

instance ntreeFunctor :: Functor NTree where
  map f (NTree x ary) = NTree (f x) (map (map f) ary)

-- Keep only the nodes matching the predicate.
-- The root of the tree is always kept.
filterNTree :: forall a. (a -> Boolean) -> NTree a -> NTree a
filterNTree p (NTree x ary) =
  NTree x $ map (filterNTree p) $ filter (\(NTree a _) -> p a) ary


newtype LNode = LNode { id :: ID
                      , name :: String
                      , nodeType :: NodeType
                      , open :: Boolean
                      , popOver :: Boolean
                      , nodeValue :: String
                      , createNode :: Boolean
                      , droppedFile :: Maybe DroppedFile
                      , showRenameBox :: Boolean}

derive instance newtypeLNode :: Newtype LNode _

instance decodeJsonLNode :: DecodeJson LNode where
  decodeJson json = do
    obj <- decodeJson json
    id_ <- obj .: "id"
    name <- obj .: "name"
    nodeType <- obj .: "type"
    pure $ LNode { id : id_
                 , name
                 , nodeType
                 , open : true
                 , popOver : false
                 , createNode : false
                 , nodeValue : ""
                 , droppedFile: Nothing
                 , showRenameBox : false}

instance decodeJsonFTree :: DecodeJson (NTree LNode) where
  decodeJson json = do
    obj <- decodeJson json
    node <- obj .: "node"
    nodes <- obj .: "children"
    node' <- decodeJson node
    nodes' <- decodeJson nodes
    pure $ NTree node' nodes'

type FTree = NTree LNode

-- file upload types
data FileType = CSV | PresseRIS
derive instance genericFileType :: Generic FileType _
instance eqFileType :: Eq FileType where
    eq = genericEq
instance showFileType :: Show FileType where
    show = genericShow
readFileType :: String -> FileType
readFileType "CSV" = CSV
readFileType "PresseRIS" = PresseRIS
readFileType ft = unsafePerformEffect $ throwException $ error $ "File type unknown: " <> ft

newtype UploadFileContents = UploadFileContents String
data DroppedFile = DroppedFile {
    contents :: UploadFileContents
  , fileType :: Maybe FileType
    }
type FileHash = String


data Action =   ShowPopOver  ID
              | ToggleFolder ID
              | Submit       ID String
              | DeleteNode   ID
              | Create       ID
              | CreateSubmit       ID String NodeType
              | SetNodeValue String ID
              | ToggleCreateNode ID
              | ShowRenameBox    ID
              | CancelRename     ID
              | CurrentNode      ID
              | PrepareUploadFile ID UploadFileContents
              | UploadFile ID FileType UploadFileContents


type State = { state       :: FTree
             , currentNode :: Maybe ID
             }

mapFTree :: (FTree -> FTree) -> State -> State
mapFTree f {state, currentNode} = {state: f state, currentNode: currentNode}

-- TODO: make it a local function
performAction :: forall props. PerformAction State props Action

performAction (ToggleFolder i) _ _ =
  modifyState_ $ mapFTree $ toggleNode i

performAction (ShowPopOver id) _ _ =
  modifyState_ $ mapFTree $ map $ popOverNode id

performAction (ShowRenameBox id) _ _ =
  modifyState_ $ mapFTree $ map $ showPopOverNode id

performAction (CancelRename id) _ _ =
  modifyState_ $ mapFTree $ map $ showPopOverNode id

performAction (ToggleCreateNode id) _ _ = do
  modifyState_ $ mapFTree $ map $ hidePopOverNode id
  modifyState_ $ mapFTree $ showCreateNode id

performAction (DeleteNode nid) _ _ = do
  void $ lift $ deleteNode nid
  modifyState_ $ mapFTree $ filterNTree (\(LNode {id}) -> id /= nid)

performAction (Submit rid name) _  _  = do
  void $ lift $ renameNode rid $ RenameValue {name}
  modifyState_ $ mapFTree $ map $ popOverNode rid
                              <<< onNode rid (\(LNode node) -> LNode (node { name = name }))

performAction (CreateSubmit nid name nodeType) _ _ = do
  void $ lift $ createNode nid $ CreateValue {name, nodeType}
  modifyState_ $ mapFTree $ map $ hidePopOverNode nid

performAction (Create  nid) _ _ = do
  modifyState_ $ mapFTree $ showCreateNode nid

performAction (SetNodeValue v nid) _ _ =
  modifyState_ $ mapFTree $ setNodeValue nid v

performAction (CurrentNode nid) _ _ =
  modifyState_ $ \{state: s} -> {state: s, currentNode : Just nid}

performAction (PrepareUploadFile nid contents) _ _ = do
  modifyState_ $ mapFTree $ map $ toggleFileTypeBox nid contents

performAction (UploadFile nid fileType contents) _ _ = do
  hashes <- lift $ uploadFile nid fileType contents
  liftEffect $ log2 "uploaded:" hashes


toggleIf :: Boolean -> Boolean -> Boolean
toggleIf true  = not
toggleIf false = const false

onNode :: ID -> (LNode -> LNode) -> LNode -> LNode
onNode id f l@(LNode node)
  | node.id == id = f l
  | otherwise     = l

popOverNode :: ID -> LNode -> LNode
popOverNode sid (LNode node) =
  LNode $ node { popOver = toggleIf (sid == node.id) node.popOver
               , showRenameBox = false }

hidePopOverNode :: ID -> LNode -> LNode
hidePopOverNode sid (LNode node) =
  LNode $ node { popOver = false }

showPopOverNode :: ID -> LNode -> LNode
showPopOverNode sid (LNode node) =
  LNode $ node {showRenameBox = toggleIf (sid == node.id) node.showRenameBox}

toggleFileTypeBox :: ID -> UploadFileContents -> LNode -> LNode
toggleFileTypeBox sid contents (LNode node@{id, droppedFile: Nothing}) | sid == id = LNode $ node {droppedFile = droppedFile}
  where
    droppedFile = Just $ DroppedFile {contents: contents, fileType: Nothing}
toggleFileTypeBox sid _ (LNode node) = LNode $ node {droppedFile = Nothing}

-- TODO: DRY, NTree.map
showCreateNode :: ID -> NTree LNode -> NTree LNode
showCreateNode sid (NTree (LNode node@{id, createNode}) ary) =
  NTree (LNode $ node {createNode = createNode'}) $ map (showCreateNode sid) ary
  where
    createNode' = if sid == id then not createNode else createNode

-- TODO: DRY, NTree.map
setNodeValue :: ID ->  String -> NTree LNode  -> NTree LNode
setNodeValue sid v (NTree (LNode node@{id}) ary)  =
  NTree (LNode $ node {nodeValue = nvalue}) $ map (setNodeValue sid  v) ary
  where
    nvalue = if sid == id then  v   else ""

-- TODO: DRY, NTree.map
toggleNode :: ID -> NTree LNode -> NTree LNode
toggleNode sid (NTree (LNode node@{id, open}) ary) =
  NTree (LNode $ node {open = nopen}) $ map (toggleNode sid) ary
  where
    nopen = if sid == id then not open else open



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

type LoadedTreeViewProps = Loader.InnerProps Int FTree ()

loadedTreeview :: Spec State LoadedTreeViewProps Action
loadedTreeview = simpleSpec performAction render
  where
    render :: Render State LoadedTreeViewProps Action
    render dispatch _ {state, currentNode} _ =
      [ div [className "tree"]
        [ toHtml dispatch state currentNode

        ]
      ]

treeViewClass :: ReactClass (Loader.InnerProps Int FTree (children :: React.Children))
treeViewClass = createClass "TreeView" loadedTreeview (\{loaded: t} -> {state: t, currentNode: Nothing})

treeLoaderClass :: Loader.LoaderClass Int FTree
treeLoaderClass = Loader.createLoaderClass "TreeLoader" loadNode

treeLoader :: Loader.Props' Int FTree -> ReactElement
treeLoader props = React.createElement treeLoaderClass props []

treeview :: Spec {} Props Void
treeview = simpleSpec defaultPerformAction render
  where
    render :: Render {} Props Void
    render _ {root} _ _ =
      [ treeLoader { path: root
                   , component: treeViewClass
                   } ]


nodePopupView :: (Action -> Effect Unit) -> FTree -> R.Element
nodePopupView d s@(NTree (LNode {id, name, popOver: true, showRenameBox }) ary) = R.createElement el {} []
  where
    el = R.hooksComponent "NodePopupView" cpt
    cpt props _ = do
      pure $ H.div tooltipProps $
        [ H.div {id: "arrow"} []
        , H.div { className: "panel panel-default"
                , style: { border:"1px solid rgba(0,0,0,0.2)"
                         , boxShadow : "0 2px 5px rgba(0,0,0,0.2)"}
                }
          [ panelHeading
          , panelBody
          ]
        ]
      where
        tooltipProps = ({ className: ""
                        , id: "node-popup-tooltip"
                        , title: "Node settings"
                        } .= "data-toggle" $ "tooltip") .= "data-placement" $ "right"
        iconAStyle = {color:"black", paddingTop: "6px", paddingBottom: "6px"}
        panelHeading =
          H.div {className: "panel-heading"}
          [ H.div {className: "row" }
            (
              [ H.div {className: if (showRenameBox) then "col-md-10" else "col-md-8"}
                [ renameBox d s ]
              ] <> [ editIcon showRenameBox ] <> [
                H.div {className: "col-md-2"}
                [ H.a {className: "btn text-danger glyphitem glyphicon glyphicon-remove"
                      , onClick: mkEffectFn1 $ \_ -> d $ ShowPopOver id
                      , title: "Close"} []
                ]
              ]
            )
          ]
        glyphicon t = "glyphitem glyphicon glyphicon-" <> t
        editIcon false = H.div {className: "col-md-2"}
                         [ H.a {style: {color:"black"}
                               , className: "btn glyphitem glyphicon glyphicon-pencil"
                               , id: "rename1"
                               , title: "Rename"
                               , onClick: mkEffectFn1 $ (\_-> d $ (ShowRenameBox id))}
                           []
                         ]
        editIcon true = H.div {} []
        panelBody =
          H.div {className: "panel-body"
                , style: { display:"flex"
                         , justifyContent : "center"
                         , backgroundColor: "white"
                         , border: "none"}}
          [ H.div {className: "col-md-4"}
            [ H.a {style: iconAStyle
                  , className: (glyphicon "plus")
                  , id: "create"
                  , title: "Create"
                  , onClick: mkEffectFn1 $ (\_ -> d $ (ToggleCreateNode id))}
              []
            ]
          , H.div {className: "col-md-4"}
            [ H.a {style: iconAStyle
                  , className: (glyphicon "download-alt")
                  , id: "download"
                  , title: "Download [WIP]"}
              []
            ]
          , H.div {className: "col-md-4"}
            [ H.a {style: iconAStyle
                  , className: (glyphicon "duplicate")
                  , id: "duplicate"
                  , title: "Duplicate [WIP]"}
              []
            ]
          , H.div {className: "col-md-4"}
            [ H.a {style: iconAStyle
                  , className: (glyphicon "trash")
                  , id: "rename2"
                  , title: "Delete"
                  , onClick: mkEffectFn1 $ (\_-> d $ (DeleteNode id))}
              []
            ]
          ]
nodePopupView _ _ = R.createElement el {} []
  where
    el = R.hooksComponent "CreateNodeView" cpt
    cpt props _ = pure $ H.div {} []


renameBox :: (Action -> Effect Unit) -> FTree -> R.Element
renameBox d s@(NTree (LNode {id, name, showRenameBox: true}) _) = R.createElement el {} []
  where
    el = R.hooksComponent "RenameBox" cpt
    cpt props _ = do
      renameNodeName <- R.useState $ \_ -> pure name
      pure $ H.div {className: "from-group row-no-padding"}
        [ renameInput renameNodeName
        , renameBtn renameNodeName
        , cancelBtn
        ]
      where
        renameInput (_ /\ setRenameNodeName) =
          H.div {className: "col-md-8"}
          [ H.input { _type: "text"
                    , placeholder: "Rename Node"
                    , defaultValue: name
                    , className: "form-control"
                    , onInput: mkEffectFn1 $ \e -> setRenameNodeName $ e .. "target" .. "value"
                    }
          ]
        renameBtn (newName /\ _) =
          H.a {className: "btn glyphitem glyphicon glyphicon-ok col-md-2 pull-left"
              , _type: "button"
              , onClick: mkEffectFn1 $ \_ -> d $ (Submit id newName)
              , title: "Rename"
              } []
        cancelBtn =
          H.a {className: "btn text-danger glyphitem glyphicon glyphicon-remove col-md-2 pull-left"
              , _type: "button"
              , onClick: mkEffectFn1 $ \_ -> d $ (CancelRename id)
              , title: "Cancel"
              } []
renameBox _ s@(NTree (LNode {name}) _) = R.createElement el {} []
  where
    el = R.hooksComponent "RenameBox" cpt
    cpt props _ = pure $ H.div {} [ H.text name ]


createNodeView :: (Action -> Effect Unit) -> FTree -> R.Element
createNodeView d s@(NTree (LNode {id, createNode: true, nodeValue}) _) = R.createElement el {} []
  where
    el = R.hooksComponent "CreateNodeView" cpt
    cpt props _ = do
      nodeName <- R.useState $ \_ -> pure ""
      nodeType <- R.useState $ \_ -> pure Corpus
      pure $ H.div tooltipProps $
        [ H.div {className: "panel panel-default"}
          [ panelHeading
          , panelBody nodeName nodeType
          , panelFooter nodeName nodeType
          ]
        ]
      where
        tooltipProps = ({ className: ""
                        , id: "create-node-tooltip"
                        , title: "Create new node"} .= "data-toggle" $ "tooltip") .= "data-placement" $ "right"
        panelHeading =
          H.div {className: "panel-heading"}
          [ H.div {className: "row"}
            [ H.div {className: "col-md-10"}
              [ H.h5 {} [H.text "Create Node"] ]
            , H.div {className: "col-md-2"}
              [ H.a { className: "btn text-danger glyphitem glyphicon glyphicon-remove"
                    , onClick: mkEffectFn1 $ \_ -> d $ ToggleCreateNode id
                    , title: "Close"} []
              ]
            ]
          ]
        panelBody (_ /\ setNodeName) (nt /\ setNodeType) =
          H.div {className: "panel-body"}
          [ H.div {className: "row form-group"}
            [ H.div {className: "col-md-12"}
              [ H.div {className: "row"}
                [ H.input { _type: "text"
                          , placeholder: "Create Node"
                          , defaultValue: getCreateNodeValue s
                          , className: "col-md-12 form-control"
                          , onInput: mkEffectFn1 $ \e -> setNodeName $ e .. "target" .. "value"
                          }
                ]
              , H.div {className: "row"}
                [ R2.select { className: "col-md-12 form-control"
                             , onChange: mkEffectFn1 $ \e -> setNodeType $ readNodeType $ e .. "target" .. "value"
                            }
                  (map renderOption [Corpus, Folder])
                ]
              ]
            ]
          ]
        renderOption (opt :: NodeType) = H.option {} [ H.text $ show opt ]
        panelFooter (name /\ _) (nt /\ _) =
          H.div {className: "panel-footer"}
          [ H.button {className: "btn btn-success"
                     , _type: "button"
                     , onClick: mkEffectFn1 $ \_ -> d $ (CreateSubmit id name nt)
                     } [H.text "Create"]
          ]
createNodeView _ _ = R.createElement el {} []
  where
    el = R.hooksComponent "CreateNodeView" cpt
    cpt props _ = pure $ H.div {} []



fileTypeView :: (Action -> Effect Unit) -> FTree -> R.Element
fileTypeView d s@(NTree (LNode {id, droppedFile: Just (DroppedFile {contents, fileType: Nothing})}) _) = R.createElement el {} []
  where
    el = R.hooksComponent "FileTypeView" cpt
    cpt props _ = do
      fileType <- R.useState $ \_ -> pure CSV
      pure $ H.div tooltipProps $
        [ H.div {className: "panel panel-default"}
          [ panelHeading
          , panelBody fileType
          , panelFooter fileType
          ]
        ]
      where
        tooltipProps = ({ className: ""
                        , id: "file-type-tooltip"
                        , title: "Choose file type"} .= "data-toggle" $ "tooltip") .= "data-placement" $ "right"
        panelHeading =
          H.div {className: "panel-heading"}
          [ H.div {className: "row"}
            [ H.div {className: "col-md-10"}
              [ H.h5 {} [H.text "Choose file type"] ]
            , H.div {className: "col-md-2"}
              [ H.a {className: "btn text-danger glyphitem glyphicon glyphicon-remove"
                    , onClick: mkEffectFn1 $ \_ -> d $ PrepareUploadFile id contents
                    , title: "Close"} []
              ]
            ]
          ]
        panelBody (_ /\ setFileType) =
          H.div {className: "panel-body"}
          [ R2.select {className: "col-md-12 form-control"
                      , onChange: onChange}
            (map renderOption [CSV, PresseRIS])
          ]
          where
            onChange = mkEffectFn1 $ \e -> setFileType $ readFileType $ e .. "target" .. "value"
        renderOption opt = H.option {} [ H.text $ show opt ]
        panelFooter (ft /\ _) =
          H.div {className: "panel-footer"}
          [ H.button {className: "btn btn-success"
                     , _type: "button"
                     , onClick: mkEffectFn1 $ \_ -> d $ (UploadFile id ft contents)
                    } [H.text "Upload"]
          ]
fileTypeView _ _ = R.createElement el {} []
  where
    el = R.hooksComponent "FileTypeView" cpt
    cpt props _ = pure $ H.div {} []

popOverValue :: FTree -> Boolean
popOverValue (NTree (LNode {popOver}) ary) = popOver

getCreateNodeValue :: FTree -> String
getCreateNodeValue (NTree (LNode {nodeValue}) ary) = nodeValue


toHtml :: (Action -> Effect Unit) -> FTree -> Maybe ID -> ReactElement
toHtml d s@(NTree (LNode {id, name, nodeType}) []) n =
  ul []
  [
    li [] $ [span []
    [ a [className "glyphicon glyphicon-cog", _id "rename-leaf",onClick $ (\_-> d $ (ShowPopOver id))] []
    , a [ href (toUrl Front nodeType (Just id)), style {"margin-left":"22px"}
        , onClick $ (\e -> d $ CurrentNode id)
        ]
      [ if n == (Just id) then u [] [b [] [text ("| " <> name <> " |    ")]] else text (name <> "    ") ]
    , (R2.scuff $ nodePopupView d s)
    , (R2.scuff $ createNodeView d s)
    , (R2.scuff $ fileTypeView d s)
    ]
  ]]
--- need to add renameTreeview value to this function
toHtml d s@(NTree (LNode {id, name, nodeType, open}) ary) n =
    ul []
  [ li [] $
    ( [span [onDrop dropHandler, onDragOver onDragOverHandler] [
         a [onClick $ (\e-> d $ ToggleFolder id)] [i [fldr open] []]
       , a [ href (toUrl Front nodeType (Just id)), style {"margin-left":"22px"}
           , onClick $ (\e -> d $ CurrentNode id)
           ]
         --[ text name ]
         [ if n == (Just id) then u [] [b [] [text $ "| " <> name <> " |"]] else text name ]
       , a [ className "glyphicon glyphicon-cog"
         , _id "rename"
         , onClick $ (\_-> d $ (ShowPopOver id))
         ] []
       , (R2.scuff $ nodePopupView d s)
       , (R2.scuff $ createNodeView d s)
       , (R2.scuff $ fileTypeView d s)
       ]
      ] <> if open then
        map (\cs -> toHtml d cs n) ary
      else []
    )
  ]
  where
    dropHandler = \e -> unsafePartial $ do
      let ff = fromJust $ item 0 $ ((e .. "dataTransfer" .. "files") :: FileList)
      liftEffect $ log2 "drop:" ff
      -- prevent redirection when file is dropped
      E.preventDefault e
      E.stopPropagation e
      let blob = toBlob $ ff
      void $ runAff (\_ -> pure unit) do
        contents <- readAsText blob
        liftEffect $ d $ PrepareUploadFile id (UploadFileContents contents)
    onDragOverHandler = \e -> do
      -- prevent redirection when file is dropped
      -- https://stackoverflow.com/a/6756680/941471
      E.preventDefault e
      E.stopPropagation e


fldr :: Boolean -> DOM.Props
fldr open = if open then className "fas fa-folder-open" else className "fas fa-folder"


loadNode :: ID -> Aff FTree
loadNode = get <<< toUrl Back Tree <<< Just

----- TREE CRUD Operations

newtype RenameValue = RenameValue
  {
    name :: String
  }

instance encodeJsonRenameValue :: EncodeJson RenameValue where
  encodeJson (RenameValue {name})
     = "r_name" := name
    ~> jsonEmptyObject

newtype CreateValue = CreateValue
  {
    name :: String
  , nodeType :: NodeType
  }

instance encodeJsonCreateValue :: EncodeJson CreateValue where
  encodeJson (CreateValue {name, nodeType})
     = "pn_name" := name
    ~> "pn_typename" := nodeType
    ~> jsonEmptyObject

createNode :: ID -> CreateValue -> Aff ID
--createNode = post $ urlPlease Back $ "new"
createNode parentId = post $ toUrl Back Node (Just parentId)

renameNode :: ID -> RenameValue -> Aff (Array ID)
renameNode renameNodeId = put $ toUrl Back Node (Just renameNodeId) <> "/rename"

deleteNode :: ID -> Aff ID
deleteNode = delete <<< toUrl Back Node <<< Just

newtype FileUploadQuery = FileUploadQuery {
    fileType :: FileType
  }
derive instance newtypeSearchQuery :: Newtype FileUploadQuery _
instance fileUploadQueryToQuery :: ToQuery FileUploadQuery where
  toQuery (FileUploadQuery {fileType}) =
    QP.print id id $ QP.QueryPairs $
         pair "fileType" fileType
    where pair :: forall a. Show a => String -> a -> Array (Tuple QP.Key (Maybe QP.Value))
          pair k v = [ QP.keyFromString k /\ (Just $ QP.valueFromString $ show v) ]

uploadFile :: ID -> FileType -> UploadFileContents -> Aff (Array FileHash)
uploadFile id fileType (UploadFileContents fileContents) = postWwwUrlencoded url fileContents
  where
    q = FileUploadQuery { fileType: fileType }
    url = toUrl Back Node (Just id) <> "/upload" <> Q.print (toQuery q)

-- UNUSED
-- deleteNodes :: TODO -> Aff ID
-- deleteNodes = deleteWithBody (toUrl Back Nodes Nothing)

-- UNUSED
-- createNode :: TODO -> Aff ID
-- createNode = post (toUrl Back Node Nothing)

fnTransform :: LNode -> FTree
fnTransform n = NTree n []

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value
