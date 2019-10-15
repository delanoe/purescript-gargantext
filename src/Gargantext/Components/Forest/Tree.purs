module Gargantext.Components.Forest.Tree where

import Prelude hiding (div)

import DOM.Simple.Console (log2)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Array (filter, null)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff, runAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn1)
import FFI.Simple ((..))
import Partial.Unsafe (unsafePartial)
import React.SyntheticEvent as E
import Reactix as R
import Reactix.DOM.HTML as H
import URI.Extra.QueryPairs as QP
import URI.Query as Q
import Web.File.File (toBlob)
import Web.File.FileList (FileList, item)
import Web.File.FileReader.Aff (readAsText)

import Gargantext.Ends (Frontends, url)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes as Routes
import Gargantext.Routes (AppRoute, SessionRoute(..))
import Gargantext.Sessions (Session, sessionId, get, put, post, postWwwUrlencoded, delete)
import Gargantext.Types (class ToQuery, toQuery, NodeType(..), NodePath(..), readNodeType)
import Gargantext.Utils (id, glyphicon)
import Gargantext.Utils.Reactix as R2
import Gargantext.Components.Forest.NodeActions

type Name = String
type Open = Boolean
type URL  = String
type ID   = Int
type Reload = Int

data NodePopup = CreatePopup | NodePopup

type Props = ( root          :: ID
             , mCurrentRoute :: Maybe AppRoute
             , session       :: Session
             , frontends     :: Frontends
             )

type TreeViewProps = ( tree          :: FTree
                     , mCurrentRoute :: Maybe AppRoute
                     , frontends     :: Frontends
                     , session       :: Session 
                     )

data NTree a = NTree a (Array (NTree a))

instance ntreeFunctor :: Functor NTree where
  map f (NTree x ary) = NTree (f x) (map (map f) ary)

-- Keep only the nodes matching the predicate.
-- The root of the tree is always kept.
filterNTree :: forall a. (a -> Boolean) -> NTree a -> NTree a
filterNTree p (NTree x ary) =
  NTree x $ map (filterNTree p) $ filter (\(NTree a _) -> p a) ary


newtype LNode = LNode { id :: ID
                      , name :: Name
                      , nodeType :: NodeType
                      }

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


data Action =   Submit       String
              | DeleteNode
              | CreateSubmit String NodeType
              | UploadFile   FileType UploadFileContents


type Tree = { tree :: FTree }

mapFTree :: (FTree -> FTree) -> Tree -> Tree
mapFTree f s@{tree} = s {tree = f tree}

performAction :: Session -> R.State Int -> R.State Tree -> Action -> Aff Unit

performAction session (_ /\ setReload) (s@{tree: NTree (LNode {id}) _} /\ setTree) DeleteNode = do
  void $ deleteNode session id
  liftEffect $ setReload (_ + 1)

performAction session _ ({tree: NTree (LNode {id}) _} /\ setTree) (Submit name)  = do
  void $ renameNode session id $ RenameValue {name}
  liftEffect $ setTree $ \s@{tree: NTree (LNode node) arr} -> s {tree = NTree (LNode node {name = name}) arr}

performAction session (_ /\ setReload) (s@{tree: NTree (LNode {id}) _} /\ setTree) (CreateSubmit name nodeType) = do
  void $ createNode session id $ CreateValue {name, nodeType}
  liftEffect $ setReload (_ + 1)

performAction session _ ({tree: NTree (LNode {id}) _} /\ _) (UploadFile fileType contents) = do
  hashes <- uploadFile session id fileType contents
  liftEffect $ log2 "uploaded:" hashes

------------------------------------------------------------------------

mCorpusId :: Maybe AppRoute -> Maybe Int
mCorpusId (Just (Routes.Corpus _ id)) = Just id
mCorpusId (Just (Routes.CorpusDocument _ id _ _)) = Just id
mCorpusId _ = Nothing

treeView :: Record Props -> R.Element
treeView props = R.createElement treeViewCpt props []

treeViewCpt :: R.Component Props
treeViewCpt = R.hooksComponent "G.C.Tree.treeView" cpt
  where
    cpt props _children = do
      -- NOTE: this is a hack to reload the tree view on demand
      reload <- R.useState' (0 :: Reload)
      pure $ treeLoadView reload props

treeLoadView :: R.State Reload -> Record Props -> R.Element
treeLoadView reload p = R.createElement el p []
  where
    el = R.hooksComponent "TreeLoadView" cpt
    cpt {root, mCurrentRoute, session, frontends} _ = do
      useLoader root (loadNode session) $ \loaded ->
        loadedTreeView reload {tree: loaded, mCurrentRoute, session, frontends}

loadedTreeView :: R.State Reload -> Record TreeViewProps -> R.Element
loadedTreeView reload p = R.createElement el p []
  where
    el = R.hooksComponent "LoadedTreeView" cpt
    cpt {tree, mCurrentRoute, session, frontends} _ = do
      treeState <- R.useState' {tree}

      pure $ H.div {className: "tree"}
        [ toHtml reload treeState session frontends mCurrentRoute ]

-- | START toHtml
toHtml :: R.State Reload -> R.State Tree -> Session -> Frontends -> Maybe AppRoute -> R.Element
toHtml reload treeState@({tree: (NTree (LNode {id, name, nodeType}) ary)} /\ _) session frontends mCurrentRoute = R.createElement el {} []
  where
    el = R.hooksComponent "NodeView" cpt
    pAction = performAction session reload treeState
    cpt props _ = do
      folderOpen <- R.useState' true

      let withId (NTree (LNode {id: id'}) _) = id'

      pure $ H.ul {}
        [ H.li {}
          ( [ nodeMainSpan pAction {id, name, nodeType, mCurrentRoute} folderOpen session frontends ]
            <> childNodes session frontends reload folderOpen mCurrentRoute ary
          )
        ]

type NodeMainSpanProps =
  ( id            :: ID
  , name          :: Name
  , nodeType      :: NodeType
  , mCurrentRoute :: Maybe AppRoute
  )

nodeMainSpan :: (Action -> Aff Unit)
             -> Record NodeMainSpanProps
             -> R.State Boolean
             -> Session
             -> Frontends
             -> R.Element
nodeMainSpan d p folderOpen session frontends = R.createElement el p []
  where
    el = R.hooksComponent "NodeMainSpan" cpt
    cpt {id, name, nodeType, mCurrentRoute} _ = do
      -- only 1 popup at a time is allowed to be opened
      popupOpen   <- R.useState' (Nothing :: Maybe NodePopup)
      droppedFile <- R.useState' (Nothing :: Maybe DroppedFile)
      isDragOver  <- R.useState' false

      pure $ H.span (dropProps droppedFile isDragOver)
        [ folderIcon folderOpen
        , H.a { href: (url frontends (NodePath (sessionId session) nodeType (Just id)))
              , style: {marginLeft: "22px"}
              }
          [ nodeText {isSelected: (mCorpusId mCurrentRoute) == (Just id), name:name'} ]
        , popOverIcon popupOpen
        , nodePopupView  d {id, name, nodeType} popupOpen
        , createNodeView d {id, name, nodeType} popupOpen
        , fileTypeView   d {id      , nodeType} droppedFile isDragOver
        ]
          where
            name' = if nodeType == NodeUser then show session else name

    folderIcon folderOpen'@(open /\ _) =
      H.a {onClick: R2.effToggler folderOpen'}
      [ H.i {className: fldr open} [] ]

    popOverIcon (popOver /\ setPopOver) =
      H.a { className: "glyphicon glyphicon-cog"
          , id: "rename-leaf"
          , on: { click: \_ -> setPopOver $ toggle }
          } []
      where
        toggle Nothing = Just NodePopup
        toggle _       = Nothing


    dropProps droppedFile isDragOver =
      { className: dropClass droppedFile isDragOver
      , on: { drop: dropHandler droppedFile
            , dragOver: onDragOverHandler isDragOver
            , dragLeave: onDragLeave isDragOver } }
      where
        dropClass (Just _ /\ _)  _           = "file-dropped"
        dropClass _              (true /\ _) = "file-dropped"
        dropClass (Nothing /\ _) _           = ""
        dropHandler (_ /\ setDroppedFile) e = unsafePartial $ do
          let ff = fromJust $ item 0 $ ((e .. "dataTransfer" .. "files") :: FileList)
          liftEffect $ log2 "drop:" ff
          -- prevent redirection when file is dropped
          E.preventDefault e
          E.stopPropagation e
          let blob = toBlob $ ff
          void $ runAff (\_ -> pure unit) do
            contents <- readAsText blob
            liftEffect $ setDroppedFile $ const $ Just $ DroppedFile {contents: (UploadFileContents contents), fileType: Just CSV}
    onDragOverHandler (_ /\ setIsDragOver) e = do
      -- prevent redirection when file is dropped
      -- https://stackoverflow.com/a/6756680/941471
      E.preventDefault e
      E.stopPropagation e
      setIsDragOver $ const true
    onDragLeave (_ /\ setIsDragOver) _ = setIsDragOver $ const false


fldr :: Boolean -> String
fldr open = if open
               then "glyphicon glyphicon-folder-open"
               else "glyphicon glyphicon-folder-close"

childNodes :: Session         -> Frontends
            -> R.State Reload -> R.State Boolean
            -> Maybe AppRoute -> Array FTree
            -> Array R.Element
childNodes _ _ _ _ _ [] = []
childNodes _ _ _ (false /\ _) _ _ = []
childNodes session frontends reload (true /\ _) mCurrentRoute ary =
  map (\ctree -> childNode {tree: ctree}) ary
    where
      childNode :: Tree -> R.Element
      childNode props = R.createElement el props []
      el = R.hooksComponent "ChildNodeView" cpt
      cpt {tree} _ = do
        treeState <- R.useState' {tree}
        pure $ toHtml reload treeState session frontends mCurrentRoute

-- END toHtml


-- | START Popup View
type NodePopupProps =
  ( id       :: ID
  , name     :: Name
  , nodeType :: NodeType
  )

iconAStyle = {color:"black", paddingTop: "6px", paddingBottom: "6px"}

nodePopupView ::   (Action -> Aff Unit)
                 -> Record NodePopupProps
                 -> R.State (Maybe NodePopup)
                 -> R.Element
nodePopupView d p (Just NodePopup /\ setPopupOpen) = R.createElement el p []
  where
    el = R.hooksComponent "NodePopupView" cpt
    cpt {id, name, nodeType} _ = do
      renameBoxOpen <- R.useState' false
      pure $ H.div tooltipProps $
        [ H.div {id: "arrow"} []
        , H.div { className: "panel panel-default"
                , style: { border: "1px solid rgba(0,0,0,0.2)"
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
        rowClass true  = "col-md-10"
        rowClass false = "col-md-8"

        Buttons {edit:edits,click:clicks,pop:pops} = buttons nodeType

        panelHeading renameBoxOpen@(open /\ _) =
          H.div {className: "panel-heading"}
          [ H.div {className: "row" }
            [ H.div {className: "col-md-1"} []
            , buttonClick d (Documentation nodeType)
            , H.div {className: rowClass open} [ renameBox d {id, name} renameBoxOpen ]
            , if not (null edits) then editIcon renameBoxOpen else H.div {} []
            , H.div {className: "col-md-2"}
              [ H.a {className: "btn text-danger glyphitem glyphicon glyphicon-remove-circle"
                    , onClick: mkEffectFn1 $ \_ -> setPopupOpen $ const Nothing
                    , title: "Close"} []
              ]
            ]
          ]
          where
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
          ((map (\a -> buttonPop a setPopupOpen) pops)
          <>
          (map (buttonClick d) clicks))


nodePopupView _ p _ = R.createElement el p []
  where
    el = R.hooksComponent "CreateNodeView" cpt
    cpt _ _ = pure $ H.div {} []


-- buttonAction :: NodeAction -> R.Element
buttonClick _ (Documentation x ) = H.div {className: "col-md-1"}
            [ H.a { style: iconAStyle
                  , className: (glyphicon "question-sign")
                  , id: "doc"
                  , title: "Documentation"
                }
                  -- , onClick: mkEffectFn1 $ \_ -> launchAff $ d $ DeleteNode}
              []
            ]

buttonClick d Disconnect = H.div {className: "col-md-4"}
            [ H.a { style: iconAStyle
                  , className: (glyphicon "log-out")
                  , id: "log-out"
                  , title: "Log Out"
                  -- , onClick: mkEffectFn1 $ \_ -> launchAff $ d $ DeleteNode}
                }
              []
            ]


buttonClick d Delete = H.div {className: "col-md-4"}
            [ H.a { style: iconAStyle
                  , className: (glyphicon "trash")
                  , id: "rename2"
                  , title: "Delete"
                  , onClick: mkEffectFn1 $ \_ -> launchAff $ d $ DeleteNode}
              []
            ]

buttonClick _ Upload = H.div {className: "col-md-4"}
            [ H.a { style: iconAStyle
                  , className: (glyphicon "upload")
                  , id: "upload"
                  , title: "Upload [WIP]"}
              []
            ]

buttonClick _ Download = H.div {className: "col-md-4"}
            [ H.a {style: iconAStyle
                  , className: (glyphicon "download")
                  , id: "download"
                  , title: "Download [WIP]"}
              []
            ]

buttonClick _ _ = H.div {} []



buttonPop (Add _) f =  H.div {className: "col-md-4"}
              [ H.a { style: iconAStyle
                    , className: (glyphicon "plus")
                    , id: "create"
                    , title: "Create"
                    , onClick: mkEffectFn1 $ \_ -> f $ const $ Just CreatePopup
                    }
                []
              ]

buttonPop _ _ = H.div {} []






-- END Popup View


-- START Rename Box

type RenameBoxProps =
  ( id :: ID
  , name :: Name)

renameBox :: (Action -> Aff Unit) -> Record RenameBoxProps -> R.State Boolean -> R.Element
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
                    launchAff $ d $ Submit newName
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


-- START Create Node

type CreateNodeProps =
  ( id       :: ID
  , name     :: Name
  , nodeType :: NodeType)

createNodeView :: (Action -> Aff Unit) -> Record CreateNodeProps -> R.State (Maybe NodePopup) -> R.Element
createNodeView d p (Just CreatePopup /\ setPopupOpen) = R.createElement el p []
  where
    el = R.hooksComponent "CreateNodeView" cpt
    cpt {id, name} _ = do
      nodeName <- R.useState' ""
      nodeType <- R.useState' NodeUser
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
        panelFooter (name' /\ _) (nt /\ _) =
          H.div {className: "panel-footer"}
          [ H.button {className: "btn btn-success"
                     , type: "button"
                     , onClick: mkEffectFn1 $ \_ -> do
                         setPopupOpen $ const Nothing
                         launchAff $ d $ CreateSubmit name' nt
                     } [H.text "Create"]
          ]

createNodeView _ _ _ = R.createElement el {} []
  where
    el = R.hooksComponent "CreateNodeView" cpt
    cpt props _ = pure $ H.div {} []


-- END Create Node


-- START File Type View

type FileTypeProps =
  ( id :: ID
  , nodeType :: NodeType)

fileTypeView :: (Action -> Aff Unit) -> Record FileTypeProps -> R.State (Maybe DroppedFile) -> R.State Boolean -> R.Element
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
                             launchAff $ d $ UploadFile ft contents
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


-- START node text

type NodeTextProps =
  ( isSelected :: Boolean
  , name :: Name )

nodeText :: Record NodeTextProps -> R.Element
nodeText p = R.createElement el p []
  where
    el = R.hooksComponent "NodeText" cpt
    cpt {isSelected: true, name} _ = do
      pure $ H.u {} [H.b {} [H.text ("| " <> name <> " |    ")]]
    cpt {isSelected: false, name} _ = do
      pure $ H.text (name <> "    ")

-- END node text

loadNode :: Session -> ID -> Aff FTree
loadNode session nodeId = get session $ NodeAPI Tree (Just nodeId) ""

----- TREE CRUD Operations

newtype RenameValue = RenameValue
  {
    name :: Name
  }

instance encodeJsonRenameValue :: EncodeJson RenameValue where
  encodeJson (RenameValue {name})
     = "r_name" := name
    ~> jsonEmptyObject

newtype CreateValue = CreateValue
  {
    name :: Name
  , nodeType :: NodeType
  }

instance encodeJsonCreateValue :: EncodeJson CreateValue where
  encodeJson (CreateValue {name, nodeType})
     = "pn_name" := name
    ~> "pn_typename" := nodeType
    ~> jsonEmptyObject

createNode :: Session -> ID -> CreateValue -> Aff ID
createNode session parentId = post session $ NodeAPI Node (Just parentId) ""

renameNode :: Session -> ID -> RenameValue -> Aff (Array ID)
renameNode session renameNodeId = put session $ NodeAPI Node (Just renameNodeId) "rename"

deleteNode :: Session -> ID -> Aff ID
deleteNode session nodeId = delete session $ NodeAPI Node (Just nodeId) ""

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

uploadFile :: Session -> ID -> FileType -> UploadFileContents -> Aff (Array FileHash)
uploadFile session id fileType (UploadFileContents fileContents) =
    postWwwUrlencoded session p fileContents
  where
    q = FileUploadQuery { fileType: fileType }
    p = NodeAPI Node (Just id) $ "upload" <> Q.print (toQuery q)

fnTransform :: LNode -> FTree
fnTransform n = NTree n []
