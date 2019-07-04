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
import Effect.Uncurried (mkEffectFn1)
import FFI.Simple ((..))
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
                      , nodeType :: NodeType}

derive instance newtypeLNode :: Newtype LNode _

instance decodeJsonLNode :: DecodeJson LNode where
  decodeJson json = do
    obj <- decodeJson json
    id_ <- obj .: "id"
    name <- obj .: "name"
    nodeType <- obj .: "type"
    pure $ LNode { id : id_
                 , name
                 , nodeType}

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
  modifyState_ $ mapFTree $ setNodeName rid name

performAction (CreateSubmit nid name nodeType) _ _ = do
  void $ lift $ createNode nid $ CreateValue {name, nodeType}
  --modifyState_ $ mapFTree $ map $ hidePopOverNode nid

performAction (CurrentNode nid) _ _ =
  modifyState_ $ \{state: s} -> {state: s, currentNode : Just nid}

performAction (UploadFile nid fileType contents) _ _ = do
  hashes <- lift $ uploadFile nid fileType contents
  liftEffect $ log2 "uploaded:" hashes

--toggleFileTypeBox :: ID -> UploadFileContents -> LNode -> LNode
--toggleFileTypeBox sid contents (LNode node@{id, droppedFile: Nothing}) | sid == id = LNode $ node {droppedFile = droppedFile}
--  where
--    droppedFile = Just $ DroppedFile {contents: contents, fileType: Nothing}
--toggleFileTypeBox sid _ (LNode node) = LNode $ node {droppedFile = Nothing}

-- TODO: DRY, NTree.map
setNodeName :: ID -> String -> NTree LNode -> NTree LNode
setNodeName nid n (NTree (LNode node@{id}) ary) =
  NTree (LNode $ node {name = nname}) $ map (setNodeName nid n) ary
  where
    nname = if nid == id then  n   else node.name



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

-- loadedTreeView p = R.createElement el p []
--   where
--     el = R.hooksComponent "LoadedTreeView" cpt
--     cpt {tree} _ = do
--       setTree <- R.useState' tree

--       pure $ H.div {className: "tree"}
--         [ toHtml setTree tree Nothing ]

treeview :: Spec {} Props Void
treeview = simpleSpec defaultPerformAction render
  where
    render :: Render {} Props Void
    render _ props _ _ = [R2.scuff $ R.createElement cpt props []]

    cpt =
      R.hooksComponent "TreeView" \{root} _children ->
        useLoader root loadNode \currentPath loaded ->
          R2.buff $ React.createElement treeViewClass {tree: loaded} []
          --R2.scuff $ loadedTreeView {tree: loaded}


-- START Popup View

type NodePopupProps =
  ( id :: ID
  , name :: String)

nodePopupView ::   (Action -> Effect Unit)
                 -> Record NodePopupProps
                 -> R.State (Maybe NodePopup)
                 -> R.Element
nodePopupView d p (Just NodePopup /\ setPopupOpen) = R.createElement el p []
  where
    el = R.hooksComponent "NodePopupView" cpt
    cpt {id, name} _ = do
      renameBoxOpen <- R.useState' false
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
        tooltipProps = { className: ""
                       , id: "node-popup-tooltip"
                       , title: "Node settings"
                       , data: {toggle: "tooltip", placement: "right"}
                       }
        iconAStyle = {color:"black", paddingTop: "6px", paddingBottom: "6px"}
        rowClass true = "col-md-10"
        rowClass false = "col-md-8"
        panelHeading renameBoxOpen@(open /\ _) =
          H.div {className: "panel-heading"}
          [ H.div {className: "row" }
            [ H.div {className: rowClass open} [ renameBox d {id, name} renameBoxOpen ]
            , editIcon renameBoxOpen
            , H.div {className: "col-md-2"}
              [ H.a {className: "btn text-danger glyphitem glyphicon glyphicon-remove-circle"
                    , onClick: mkEffectFn1 $ \_ -> setPopupOpen $ const Nothing
                    , title: "Close"} []
              ]
            ]
          ]
        glyphicon t = "glyphitem glyphicon glyphicon-" <> t
        editIcon (false /\ setRenameBoxOpen) =
          H.div {className: "col-md-2"}
          [ H.a {style: {color: "black"}
                , className: "btn glyphitem glyphicon glyphicon-pencil"
                , id: "rename1"
                , title: "Rename"
                , onClick: mkEffectFn1 $ \_ -> setRenameBoxOpen $ const true
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
                  , className: (glyphicon "download")
                  , id: "download"
                  , title: "Download [WIP]"}
              []
            ]
          , H.div {className: "col-md-4"}
            [ H.a {style: iconAStyle
                  , className: (glyphicon "upload")
                  , id: "upload"
                  , title: "Upload [WIP]"}
              []
            ]

          , H.div {className: "col-md-4"}
            [ H.a {style: iconAStyle
                  , className: (glyphicon "refresh")
                  , id: "refresh"
                  , title: "Refresh [WIP]"}
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
                    , onClick: mkEffectFn1 $ \_ -> setPopupOpen $ const $ Just CreatePopup
                    }
                []
              ]
nodePopupView _ p _ = R.createElement el p []
  where
    el = R.hooksComponent "CreateNodeView" cpt
    cpt _ _ = pure $ H.div {} []

-- END Popup View


-- START Rename Box

type RenameBoxProps =
  ( id :: ID
  , name :: String)

renameBox :: (Action -> Effect Unit) -> Record RenameBoxProps -> R.State Boolean -> R.Element
renameBox d p (true /\ setRenameBoxOpen) = R.createElement el p []
  where
    el = R.hooksComponent "RenameBox" cpt
    cpt {id, name} _ = do
      renameNodeName <- R.useState' name
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
                    , onInput: mkEffectFn1 $ \e -> setRenameNodeName $ const $ e .. "target" .. "value"
                    }
          ]
        renameBtn (newName /\ _) =
          H.a {className: "btn glyphitem glyphicon glyphicon-ok col-md-2 pull-left"
              , type: "button"
              , onClick: mkEffectFn1 $ \_ -> do
                    setRenameBoxOpen $ const false
                    d $ Submit id newName
              , title: "Rename"
              } []
        cancelBtn =
          H.a {className: "btn text-danger glyphitem glyphicon glyphicon-remove col-md-2 pull-left"
              , type: "button"
              , onClick: mkEffectFn1 $ \_ -> setRenameBoxOpen $ const false
              , title: "Cancel"
              } []
renameBox _ p (false /\ _) = R.createElement el p []
  where
    el = R.hooksComponent "RenameBox" cpt
    cpt {name} _ = pure $ H.div {} [ H.text name ]

-- END Rename Box

createNodeView d p (Just CreatePopup /\ setPopupOpen) = R.createElement el p []
  where
    el = R.hooksComponent "CreateNodeView" cpt
    cpt {id, name} _ = do
      nodeName <- R.useState' ""
      nodeType <- R.useState' Corpus
      pure $ H.div tooltipProps $
        [ H.div {className: "panel panel-default"}
          [ panelHeading
          , panelBody nodeName nodeType
          , panelFooter nodeName nodeType
          ]
        ]
      where
        tooltipProps = { className: ""
                       , id: "create-node-tooltip"
                       , title: "Create new node"
                       , data: {toggle: "tooltip", placement: "right"}
                       }
        panelHeading =
          H.div {className: "panel-heading"}
          [ H.div {className: "row"}
            [ H.div {className: "col-md-10"}
              [ H.h5 {} [H.text "Create Node"] ]
            , H.div {className: "col-md-2"}
              [ H.a { className: "btn text-danger glyphitem glyphicon glyphicon-remove-circle"
                    , onClick: mkEffectFn1 $ \_ -> setPopupOpen $ const Nothing
                    , title: "Close"} []
              ]
            ]
          ]
        panelBody :: R.State String -> R.State NodeType -> R.Element
        panelBody (_ /\ setNodeName) (nt /\ setNodeType) =
          H.div {className: "panel-body"}
          [ H.div {className: "row"}
            [ H.div {className: "col-md-12"}
              [ H.form {className: "form-horizontal"}
                [ H.div {className: "form-group"}
                  [ H.input { type: "text"
                            , placeholder: "Node name"
                            , defaultValue: name
                            , className: "form-control"
                            , onInput: mkEffectFn1 $ \e -> setNodeName $ const $ e .. "target" .. "value"
                            }
                  ]
                , H.div {className: "form-group"}
                  [ R2.select { className: "form-control"
                              , onChange: mkEffectFn1 $ \e -> setNodeType $ const $ readNodeType $ e .. "target" .. "value"
                              }
                    (map renderOption [Corpus, Folder])
                  ]
                ]
              ]
            ]
          ]
        renderOption (opt :: NodeType) = H.option {} [ H.text $ show opt ]
        panelFooter :: R.State String  -> R.State NodeType -> R.Element
        panelFooter (name /\ _) (nt /\ _) =
          H.div {className: "panel-footer"}
          [ H.button {className: "btn btn-success"
                     , type: "button"
                     , onClick: mkEffectFn1 $ \_ -> do
                         setPopupOpen $ const Nothing
                         d $ (CreateSubmit id name nt)
                     } [H.text "Create"]
          ]
createNodeView _ _ _ = R.createElement el {} []
  where
    el = R.hooksComponent "CreateNodeView" cpt
    cpt props _ = pure $ H.div {} []

-- START File Type View

type FileTypeProps =
  ( id :: ID )

fileTypeView :: (Action -> Effect Unit) -> Record FileTypeProps -> R.State (Maybe DroppedFile) -> R.State Boolean -> R.Element
fileTypeView d p (Just (DroppedFile {contents, fileType}) /\ setDroppedFile) (_ /\ setIsDragOver) = R.createElement el p []
  where
    el = R.hooksComponent "FileTypeView" cpt
    cpt {id} _ = do
      pure $ H.div tooltipProps $
        [ H.div {className: "panel panel-default"}
          [ panelHeading
          , panelBody
          , panelFooter
          ]
        ]
      where
        tooltipProps = { className: ""
                       , id: "file-type-tooltip"
                       , title: "Choose file type"
                       , data: {toggle: "tooltip", placement: "right"}
                       }
        panelHeading =
          H.div {className: "panel-heading"}
          [ H.div {className: "row"}
            [ H.div {className: "col-md-10"}
              [ H.h5 {} [H.text "Choose file type"] ]
            , H.div {className: "col-md-2"}
              [ H.a {className: "btn text-danger glyphitem glyphicon glyphicon-remove-circle"
                    , onClick: mkEffectFn1 $ \_ -> do
                        setDroppedFile $ const Nothing
                        setIsDragOver $ const false
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
              setDroppedFile $ const $ Just $ DroppedFile $ {contents, fileType: readFileType $ e .. "target" .. "value"}
        renderOption opt = H.option {} [ H.text $ show opt ]
        panelFooter =
          H.div {className: "panel-footer"}
          [
            case fileType of
              Just ft ->
                H.button {className: "btn btn-success"
                         , type: "button"
                         , onClick: mkEffectFn1 $ \_ -> do
                             setDroppedFile $ const Nothing
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

-- END File Type View


toHtml :: (Action -> Effect Unit) -> FTree -> Maybe ID -> R.Element
toHtml d s@(NTree (LNode {id, name, nodeType}) ary) n = R.createElement el {} []
  where
    el = R.hooksComponent "NodeView" cpt
    cpt props _ = do
      folderOpen <- R.useState' true

      pure $ H.ul {}
        [ H.li {}
          ( [ nodeMainSpan d {id, name, nodeType} n folderOpen ]
            <> childNodes d n ary folderOpen
          )
        ]

type NodeMainSpanProps =
  ( id :: ID
  , name :: String
  , nodeType :: NodeType)

data NodePopup = CreatePopup | NodePopup

nodeMainSpan :: (Action -> Effect Unit)
             -> Record NodeMainSpanProps
             -> Maybe ID
             -> R.State Boolean
             -> R.Element
nodeMainSpan d p n folderOpen = R.createElement el p []
  where
    el = R.hooksComponent "NodeMainSpan" cpt
    cpt {id, name, nodeType} _ = do
      -- only 1 popup at a time is allowed to be opened
      popupOpen <- R.useState' (Nothing :: Maybe NodePopup)
      droppedFile <- R.useState' (Nothing :: Maybe DroppedFile)
      isDragOver <- R.useState' false

      pure $ H.span (dropProps droppedFile isDragOver)
        [ folderIcon folderOpen
        , H.a { href: (toUrl Front nodeType (Just id))
              , style: {"margin-left": "22px"}
              , onClick: mkEffectFn1 $ \e -> d $ CurrentNode id
              }
          [ nodeText {isSelected: n == (Just id), name} ]
        , popOverIcon popupOpen
        , nodePopupView d {id, name} popupOpen
        , createNodeView d {id, name} popupOpen
        , fileTypeView d {id} droppedFile isDragOver
        ]
    folderIcon folderOpen@(open /\ _) =
      H.a {onClick: R2.effToggler folderOpen}
      [ H.i {className: fldr open} [] ]
    popOverIcon (popOver /\ setPopOver) =
      H.a { className: "glyphicon glyphicon-cog"
          , id: "rename-leaf"
          , onClick: mkEffectFn1 $ \_ -> setPopOver $ toggle
          } []
      where
        toggle Nothing = Just NodePopup
        toggle _       = Nothing
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
        liftEffect $ setDroppedFile $ const $ Just $ DroppedFile {contents: (UploadFileContents contents), fileType: Just CSV}
    onDragOverHandler (_ /\ setIsDragOver) = mkEffectFn1 $ \e -> do
      -- prevent redirection when file is dropped
      -- https://stackoverflow.com/a/6756680/941471
      E.preventDefault e
      E.stopPropagation e
      setIsDragOver $ const true
    onDragLeave (_ /\ setIsDragOver) = mkEffectFn1 $ \_ -> setIsDragOver $ const false


fldr :: Boolean -> String
fldr open = if open then "glyphicon glyphicon-folder-open" else "glyphicon glyphicon-folder-close"


childNodes :: forall s. (Action -> Effect Unit) -> Maybe ID -> (Array (NTree LNode)) -> R.State Boolean -> Array R.Element
childNodes d n [] _ = []
childNodes d n _ (false /\ _) = []
childNodes d n ary (true /\ _) = map (\cs -> toHtml d cs n) ary


-- START node text

type NodeTextProps =
  ( isSelected :: Boolean
  , name :: String )

nodeText :: Record NodeTextProps -> R.Element
nodeText p = R.createElement el p []
  where
    el = R.hooksComponent "NodeText" cpt
    cpt {isSelected: true, name} _ = do
      pure $ H.u {} [H.b {} [H.text ("| " <> name <> " |    ")]]
    cpt {isSelected: false, name} _ = do
      pure $ H.text (name <> "    ")

-- END node text

loadNode :: ID -> Aff FTree
-- loadNode a = lift ((get <<< toUrl Back Tree <<< Just) a)
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
