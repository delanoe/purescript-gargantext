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
import Effect.Exception (error)
import Effect.Uncurried (mkEffectFn1)
import FFI.Simple ((..), (.=))
import Gargantext.Components.Loader2 (useLoader)
import Gargantext.Config (toUrl, End(..), NodeType(..), readNodeType)
import Gargantext.Config.REST (get, put, post, postWwwUrlencoded, delete)
import Gargantext.Types (class ToQuery, toQuery)
import Gargantext.Utils (id)
import Gargantext.Utils.Reactix as R2
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReactElement)
import React as React
import React.DOM (a, div, i)
import React.DOM.Props (className, style)
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
                      , popOver :: Boolean
                      , nodeValue :: String
                      , createOpen :: Boolean}

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
                 , popOver : false
                 , nodeValue : ""
                 , createOpen : false}

instance decodeJsonFTree :: DecodeJson (NTree LNode) where
  decodeJson json = do
    obj <- decodeJson json
    node <- obj .: "node"
    nodes <- obj .: "children"
    node' <- decodeJson node
    nodes' <- decodeJson nodes
    pure $ NTree node' nodes'

type FTree = NTree LNode

setName v (NTree (LNode s@{name}) ary) = NTree (LNode $ s {name = v}) ary
setPopOver v (NTree (LNode s@{popOver}) ary) = NTree (LNode $ s {popOver = v}) ary
setCreateOpen v (NTree (LNode s@{createOpen}) ary) = NTree (LNode $ s {createOpen = v}) ary

-- file upload types
data FileType = CSV | PresseRIS
derive instance genericFileType :: Generic FileType _
instance eqFileType :: Eq FileType where
    eq = genericEq
instance showFileType :: Show FileType where
    show = genericShow
readFileType :: String -> Maybe FileType
readFileType "CSV" = Just CSV
readFileType "PresseRIS" = Just PresseRIS
readFileType _ = Nothing

newtype UploadFileContents = UploadFileContents String
data DroppedFile = DroppedFile {
    contents :: UploadFileContents
  , fileType :: Maybe FileType
    }
type FileHash = String


data Action =   Submit       ID String
              | DeleteNode   ID
              | CreateSubmit       ID String NodeType
              | SetNodeValue String ID
              | CurrentNode      ID
              | UploadFile ID FileType UploadFileContents


type State = { state       :: FTree
             , currentNode :: Maybe ID
             }

mapFTree :: (FTree -> FTree) -> State -> State
mapFTree f {state, currentNode} = {state: f state, currentNode: currentNode}

-- TODO: make it a local function
performAction :: forall props. PerformAction State props Action

performAction (DeleteNode nid) _ _ = do
  void $ lift $ deleteNode nid
  modifyState_ $ mapFTree $ filterNTree (\(LNode {id}) -> id /= nid)

performAction (Submit rid name) _  _  = do
  void $ lift $ renameNode rid $ RenameValue {name}

performAction (CreateSubmit nid name nodeType) _ _ = do
  void $ lift $ createNode nid $ CreateValue {name, nodeType}
  --modifyState_ $ mapFTree $ map $ hidePopOverNode nid

performAction (SetNodeValue v nid) _ _ =
  modifyState_ $ mapFTree $ setNodeValue nid v

performAction (CurrentNode nid) _ _ =
  modifyState_ $ \{state: s} -> {state: s, currentNode : Just nid}

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

--toggleFileTypeBox :: ID -> UploadFileContents -> LNode -> LNode
--toggleFileTypeBox sid contents (LNode node@{id, droppedFile: Nothing}) | sid == id = LNode $ node {droppedFile = droppedFile}
--  where
--    droppedFile = Just $ DroppedFile {contents: contents, fileType: Nothing}
--toggleFileTypeBox sid _ (LNode node) = LNode $ node {droppedFile = Nothing}

-- TODO: DRY, NTree.map
setNodeValue :: ID ->  String -> NTree LNode  -> NTree LNode
setNodeValue sid v (NTree (LNode node@{id}) ary)  =
  NTree (LNode $ node {nodeValue = nvalue}) $ map (setNodeValue sid  v) ary
  where
    nvalue = if sid == id then  v   else ""



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
                         true -> [ i [className "glyphicon glyphicon-refresh" ] []
                                 , i [className "glyphicon glyphicon-upload"   ] []
                                 , i [className "glyphicon glyphicon-share"] []
                                 ]
                         false -> []


nodeOptionsRename :: (Action -> Effect Unit) ->  Boolean ->  ID -> Array ReactElement
nodeOptionsRename d activated  id =  case activated of
                         true -> [ a [className "glyphicon glyphicon-pencil", style {marginLeft : "15px"}

                                        ] []
                                 ]
                         false -> []

type TreeViewProps = { tree :: FTree }

loadedTreeview :: Spec State TreeViewProps Action
loadedTreeview = simpleSpec performAction render
  where
    render :: Render State TreeViewProps Action
    render dispatch _ {state, currentNode} _ =
      [ div [className "tree"]
        [ --toHtml dispatch state currentNode
          (R2.scuff $ toHtml dispatch state currentNode)
        ]
      ]

treeViewClass :: ReactClass { tree :: FTree, children :: React.Children }
treeViewClass = createClass "TreeView" loadedTreeview (\{tree} -> {state: tree, currentNode: Nothing})

treeview :: Spec {} Props Void
treeview = simpleSpec defaultPerformAction render
  where
    render :: Render {} Props Void
    render _ {root} _ _ =
      R.hooksComponent "TreeView" \props children ->
        useLoader root loadNode \currentPath loaded ->
          React.createElement treeViewClass {tree: loaded}


--nodePopupView :: forall s. (Action -> Effect Unit) -> FTree -> RAction s -> R.Element
nodePopupView d nodeState@(s@(NTree (LNode {id, name, popOver: true, createOpen}) _) /\ setNodeState) = R.createElement el {} []
  where
    el = R.hooksComponent "NodePopupView" cpt
    cpt props _ = do
      renameBoxOpen <- R.useState $ \_ -> pure false
      pure $ H.div tooltipProps $
        [ H.div {id: "arrow"} []
        , H.div { className: "panel panel-default"
                , style: { border:"1px solid rgba(0,0,0,0.2)"
                         , boxShadow : "0 2px 5px rgba(0,0,0,0.2)"}
                }
          [ panelHeading renameBoxOpen
          , panelBody
          ]
        ]
      where
        tooltipProps = ({ className: ""
                        , id: "node-popup-tooltip"
                        , title: "Node settings"
                        } .= "data-toggle" $ "tooltip") .= "data-placement" $ "right"
        iconAStyle = {color:"black", paddingTop: "6px", paddingBottom: "6px"}
        panelHeading renameBoxOpen@(open /\ _) =
          H.div {className: "panel-heading"}
          [ H.div {className: "row" }
            (
              [ H.div {className: if (open) then "col-md-10" else "col-md-8"}
                [ renameBox d nodeState renameBoxOpen ]
              ] <> [ editIcon renameBoxOpen ] <> [
                H.div {className: "col-md-2"}
                [ H.a {className: "btn text-danger glyphitem glyphicon glyphicon-remove-circle"
                      , onClick: mkEffectFn1 $ \_ -> setNodeState $ setPopOver false s
                      , title: "Close"} []
                ]
              ]
            )
          ]
        glyphicon t = "glyphitem glyphicon glyphicon-" <> t
        editIcon (false /\ setRenameBoxOpen) =
          H.div {className: "col-md-2"}
          [ H.a {style: {color: "black"}
                , className: "btn glyphitem glyphicon glyphicon-pencil"
                , id: "rename1"
                , title: "Rename"
                , onClick: mkEffectFn1 $ \_ -> setRenameBoxOpen true
                }
            []
          ]
        editIcon (true /\ _) = H.div {} []
        panelBody =
          H.div {className: "panel-body"
                , style: { display:"flex"
                         , justifyContent : "center"
                         , backgroundColor: "white"
                         , border: "none"}}
          [ createButton
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
          where
            createButton =
              H.div {className: "col-md-4"}
              [ H.a {style: iconAStyle
                    , className: (glyphicon "plus")
                    , id: "create"
                    , title: "Create"
                    , onClick: mkEffectFn1 $ \_ -> setNodeState $ setCreateOpen (not createOpen) $ setPopOver false s
                    }
                []
              ]
nodePopupView _ _ = R.createElement el {} []
  where
    el = R.hooksComponent "CreateNodeView" cpt
    cpt props _ = pure $ H.div {} []


renameBox d (s@(NTree (LNode {id, name}) _) /\ setNodeState) (true /\ setRenameBoxOpen) = R.createElement el {} []
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
          [ H.input { type: "text"
                    , placeholder: "Rename Node"
                    , defaultValue: name
                    , className: "form-control"
                    , onInput: mkEffectFn1 $ \e -> setRenameNodeName $ e .. "target" .. "value"
                    }
          ]
        renameBtn (newName /\ _) =
          H.a {className: "btn glyphitem glyphicon glyphicon-ok col-md-2 pull-left"
              , type: "button"
              , onClick: mkEffectFn1 $ \_ -> do
                    setNodeState $ setPopOver false $ setName newName s
                    d $ (Submit id newName)
              , title: "Rename"
              } []
        cancelBtn =
          H.a {className: "btn text-danger glyphitem glyphicon glyphicon-remove col-md-2 pull-left"
              , type: "button"
              , onClick: mkEffectFn1 $ \_ -> setRenameBoxOpen false
              , title: "Cancel"
              } []
renameBox _ (s@(NTree (LNode {name}) _) /\ _) (false /\ _) = R.createElement el {} []
  where
    el = R.hooksComponent "RenameBox" cpt
    cpt props _ = pure $ H.div {} [ H.text name ]


--createNodeView :: (Action -> Effect Unit) -> FTree -> R.Element
createNodeView d (s@(NTree (LNode {id, nodeValue, createOpen: true}) _) /\ setNodeState) = R.createElement el {} []
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
              [ H.a { className: "btn text-danger glyphitem glyphicon glyphicon-remove-circle"
                    , onClick: mkEffectFn1 $ \_ -> setNodeState $ setCreateOpen false s
                    , title: "Close"} []
              ]
            ]
          ]
        panelBody (_ /\ setNodeName) (nt /\ setNodeType) =
          H.div {className: "panel-body"}
          [ H.div {className: "row"}
            [ H.div {className: "col-md-12"}
              [ H.form {className: "form-horizontal"}
                [ H.div {className: "form-group"}
                  [ H.input { type: "text"
                            , placeholder: "Node name"
                            , defaultValue: getCreateNodeValue s
                            , className: "form-control"
                            , onInput: mkEffectFn1 $ \e -> setNodeName $ e .. "target" .. "value"
                            }
                  ]
                , H.div {className: "form-group"}
                  [ R2.select { className: "form-control"
                              , onChange: mkEffectFn1 $ \e -> setNodeType $ readNodeType $ e .. "target" .. "value"
                              }
                    (map renderOption [Corpus, Folder])
                  ]
                ]
              ]
            ]
          ]
        renderOption (opt :: NodeType) = H.option {} [ H.text $ show opt ]
        panelFooter (name /\ _) (nt /\ _) =
          H.div {className: "panel-footer"}
          [ H.button {className: "btn btn-success"
                     , type: "button"
                     , onClick: mkEffectFn1 $ \_ -> d $ (CreateSubmit id name nt)
                     } [H.text "Create"]
          ]
createNodeView _ _ = R.createElement el {} []
  where
    el = R.hooksComponent "CreateNodeView" cpt
    cpt props _ = pure $ H.div {} []

--fileTypeView :: (Action -> Effect Unit) -> FTree -> R.Element
fileTypeView d (s@(NTree (LNode {id}) _) /\ _) (Just (DroppedFile {contents, fileType}) /\ setDroppedFile) (_ /\ setIsDragOver) = R.createElement el {} []
  where
    el = R.hooksComponent "FileTypeView" cpt
    cpt props _ = do
      pure $ H.div tooltipProps $
        [ H.div {className: "panel panel-default"}
          [ panelHeading
          , panelBody
          , panelFooter
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
              [ H.a {className: "btn text-danger glyphitem glyphicon glyphicon-remove-circle"
                    , onClick: mkEffectFn1 $ \_ -> do
                        setDroppedFile Nothing
                        setIsDragOver false
                    , title: "Close"} []
              ]
            ]
          ]
        panelBody =
          H.div {className: "panel-body"}
          [ R2.select {className: "col-md-12 form-control"
                      , onChange: onChange}
            (map renderOption [CSV, PresseRIS])
          ]
          where
            onChange = mkEffectFn1 $ \e ->
              setDroppedFile $ Just $ DroppedFile $ {contents, fileType: readFileType $ e .. "target" .. "value"}
        renderOption opt = H.option {} [ H.text $ show opt ]
        panelFooter =
          H.div {className: "panel-footer"}
          [
            case fileType of
              Just ft ->
                H.button {className: "btn btn-success"
                         , type: "button"
                         , onClick: mkEffectFn1 $ \_ -> do
                             setDroppedFile $ Nothing
                             d $ (UploadFile id ft contents)
                         } [H.text "Upload"]
              Nothing ->
                H.button {className: "btn btn-success disabled"
                         , type: "button"
                         } [H.text "Upload"]
          ]
fileTypeView _ _ (Nothing /\ _) _ = R.createElement el {} []
  where
    el = R.hooksComponent "FileTypeView" cpt
    cpt props _ = pure $ H.div {} []

getCreateNodeValue :: FTree -> String
getCreateNodeValue (NTree (LNode {nodeValue}) ary) = nodeValue


toHtml :: (Action -> Effect Unit) -> FTree -> Maybe ID -> R.Element
toHtml d s@(NTree (LNode {id, name, nodeType}) ary) n = R.createElement el {} []
  where
    el = R.hooksComponent "NodeView" cpt
    cpt props _ = do
      nodeState <- R.useState $ \_ -> pure s
      folderOpen <- R.useState $ \_ -> pure true
      droppedFile <- R.useState $ \_ -> pure (Nothing :: Maybe DroppedFile)
      isDragOver <- R.useState $ \_ -> pure false

      pure $ H.ul {}
        [ H.li {}
          ( [ mainSpan nodeState folderOpen droppedFile isDragOver ]
            <> childNodes d n ary folderOpen
          )
        ]
      where
        mainSpan nodeState folderOpen droppedFile isDragOver =
          H.span (dropProps droppedFile isDragOver)
          [ folderIcon folderOpen
          , H.a { href: if nodeType == Phylo then (toUrl Static nodeType (Just id))
                                             else (toUrl Front  nodeType (Just id))
                , target : if nodeType == Phylo then "blank" else ""
                , style: {"margin-left": "22px"}
                , onClick: mkEffectFn1 $ (\e -> d $ CurrentNode id)
                }
            [ nodeText s n ]
          , popOverIcon nodeState
          , nodePopupView d nodeState
          , createNodeView d nodeState
          , fileTypeView d nodeState droppedFile isDragOver
          ]
        folderIcon folderOpen@(open /\ _) =
          H.a {onClick: R2.effToggler folderOpen}
          [ H.i {className: fldr open} [] ]
        dropProps droppedFile isDragOver = {
            className: dropClass droppedFile isDragOver
          , onDrop: dropHandler droppedFile
          , onDragOver: onDragOverHandler isDragOver
          , onDragLeave: onDragLeave isDragOver
          }
        dropClass (Just _ /\ _)  _           = "file-dropped"
        dropClass _              (true /\ _) = "file-dropped"
        dropClass (Nothing /\ _) _           = ""
        dropHandler (_ /\ setDroppedFile) = mkEffectFn1 $ \e -> unsafePartial $ do
          let ff = fromJust $ item 0 $ ((e .. "dataTransfer" .. "files") :: FileList)
          liftEffect $ log2 "drop:" ff
          -- prevent redirection when file is dropped
          E.preventDefault e
          E.stopPropagation e
          let blob = toBlob $ ff
          void $ runAff (\_ -> pure unit) do
            contents <- readAsText blob
            liftEffect $ setDroppedFile $ Just $ DroppedFile {contents: (UploadFileContents contents), fileType: Just CSV}
        onDragOverHandler (_ /\ setIsDragOver) = mkEffectFn1 $ \e -> do
          -- prevent redirection when file is dropped
          -- https://stackoverflow.com/a/6756680/941471
          E.preventDefault e
          E.stopPropagation e
          setIsDragOver true
        onDragLeave (_ /\ setIsDragOver) = mkEffectFn1 $ \_ -> setIsDragOver false


childNodes :: forall s. (Action -> Effect Unit) -> Maybe ID -> (Array (NTree LNode)) -> Tuple Boolean (Boolean -> Effect s) -> Array R.Element
childNodes d n [] _ = []
childNodes d n _ (false /\ _) = []
childNodes d n ary (true /\ _) = map (\cs -> toHtml d cs n) ary


nodeText (NTree (LNode {id, name}) _) n = if n == (Just id) then
              H.u {} [H.b {} [H.text ("| " <> name <> " |    ")]]
            else
              H.text (name <> "    ")


popOverIcon (s@(NTree (LNode {popOver}) _) /\ setNodeState) =
  H.a { className: "glyphicon glyphicon-cog"
      , id: "rename-leaf"
      , onClick: mkEffectFn1 $ \_ -> setNodeState $ setPopOver (not popOver) s
      } []


fldr :: Boolean -> String
fldr open = if open then "fas fa-folder-open" else "fas fa-folder"


loadNode :: ID -> Effect FTree
loadNode a = lift ((get <<< toUrl Back Tree <<< Just) a)

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
