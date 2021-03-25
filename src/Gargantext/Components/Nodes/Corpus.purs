module Gargantext.Components.Nodes.Corpus where

import Gargantext.Prelude
  ( Unit, bind, const, discard, pure, show, unit
  , ($), (+), (-), (<), (<$>), (<<<), (<>), (==), (>), class Show, class Eq)
import Data.Argonaut (class DecodeJson, decodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as A
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Components.CodeEditor as CE
import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Components.Node (NodePoly(..), HyperdataList)
import Gargantext.Components.Nodes.Types
  ( FTField, FTFieldWithIndex, FTFieldsWithIndex, Field(..), FieldType(..), Hash, Index
  , defaultField, defaultHaskell', defaultJSON', defaultMarkdown', defaultPython' )

import Gargantext.Components.Nodes.Corpus.Types (CorpusData, Hyperdata(..))
import Gargantext.Data.Array as GDA
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(NodeAPI, Children, TreeFirstLevel), AppRoute(Home), appPath, nodeTypeAppRoute)
import Gargantext.Sessions (Session, get, put, sessionId)
import Gargantext.Types (NodeType(..), AffTableResult, SessionId)
import Gargantext.Components.Forest.Tree.Node.Tools.FTree (FTree, LNode(..), NTree(..))
import Gargantext.Utils.Crypto as Crypto
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus"

type Props = ( nodeId  :: Int, session :: R.Context Session )

corpusLayout :: R2.Leaf Props
corpusLayout props = R.createElement corpusLayoutCpt props []

corpusLayoutCpt :: R.Component Props
corpusLayoutCpt = here.component "corpusLayout" cpt where
  cpt { nodeId, session } _ = cp <$> R.useContext session where
    cp s = corpusLayoutMain { key, nodeId, session: s } where
      key = show (sessionId s) <> "-" <> show nodeId

type KeyProps =
  ( nodeId  :: Int
  , key     :: String
  , session :: Session
  )

corpusLayoutMain :: R2.Leaf KeyProps
corpusLayoutMain props = R.createElement corpusLayoutMainCpt props []

corpusLayoutMainCpt :: R.Component KeyProps
corpusLayoutMainCpt = here.component "corpusLayoutMain" cpt
  where
    cpt { nodeId, key, session } _ = do
      viewType <- R.useState' Folders

      pure $ H.div{} [
        H.div{} [viewTypeSelector {state: viewType} ]
      , H.div{} [renderContent (fst viewType) nodeId session key]
      ]

    renderContent Folders nodeId session key = folderViewLoad { nodeId, session }
    renderContent Code nodeId session key = corpusLayoutWithKey { key, nodeId, session }


corpusLayoutWithKey :: R2.Leaf KeyProps
corpusLayoutWithKey props = R.createElement corpusLayoutWithKeyCpt props []

corpusLayoutWithKeyCpt :: R.Component KeyProps
corpusLayoutWithKeyCpt = here.component "corpusLayoutWithKey" cpt where
  cpt { nodeId, session } _ = do
    reload <- T.useBox T2.newReload
    reload' <- T.useLive T.unequal reload
    useLoader { nodeId, reload: reload', session } loadCorpusWithReload $
      \corpus -> corpusLayoutView { corpus, nodeId, reload, session }

type ViewProps =
  ( corpus  :: NodePoly Hyperdata
  , reload  :: T2.ReloadS
  , nodeId  :: Int
  , session :: Session
  )

corpusLayoutView :: Record ViewProps -> R.Element
corpusLayoutView props = R.createElement corpusLayoutViewCpt props []

corpusLayoutViewCpt :: R.Component ViewProps
corpusLayoutViewCpt = here.component "corpusLayoutView" cpt
  where
    cpt {corpus: (NodePoly {hyperdata: Hyperdata {fields}}), nodeId, reload, session} _ = do
      let fieldsWithIndex = List.mapWithIndex (\idx -> \t -> Tuple idx t) fields
      fieldsS <- T.useBox fieldsWithIndex
      fields' <- T.useLive T.unequal fieldsS
      fieldsRef <- R.useRef fields
      
      -- handle props change of fields
      R.useEffect1' fields $ do
        if R.readRef fieldsRef == fields then
          pure unit
        else do
          R.setRef fieldsRef fields
          T.write_ fieldsWithIndex fieldsS

      pure $ H.div {}
        [ H.div { className: "row" }
          [ H.div { className: "btn btn-primary " <> (saveEnabled fieldsWithIndex fields')
                  , on: { click: onClickSave {fields: fields', nodeId, reload, session} }
                  }
            [ H.span { className: "fa fa-floppy-o" } [  ] ]
          ]
        , H.div {}
          [ fieldsCodeEditor { fields: fieldsS
                             , nodeId
                             , session } [] ]
        , H.div { className: "row" }
          [ H.div { className: "btn btn-primary"
                  , on: { click: onClickAdd fieldsS }
                  }
            [ H.span { className: "fa fa-plus" } [  ]
            ]
          ]
        ]

    saveEnabled :: FTFieldsWithIndex -> FTFieldsWithIndex -> String
    saveEnabled fs fsS = if fs == fsS then "disabled" else "enabled"

    onClickSave :: forall e. { fields :: FTFieldsWithIndex
                             , nodeId :: Int
                             , reload :: T2.ReloadS
                             , session :: Session } -> e -> Effect Unit
    onClickSave {fields, nodeId, reload, session} _ = do
      launchAff_ do
        saveCorpus $ { hyperdata: Hyperdata {fields: (\(Tuple _ f) -> f) <$> fields}
                     , nodeId
                     , session }
        liftEffect $ T2.reload reload

    onClickAdd :: forall e. T.Box FTFieldsWithIndex -> e -> Effect Unit
    onClickAdd fieldsS _ = do
      T.modify_ (\fields -> List.snoc fields $ Tuple (List.length fields) defaultField) fieldsS

data FolderStyle = FolderUp | FolderChild

folderViewLoad :: R2.Leaf LoadProps
folderViewLoad props = R.createElement folderViewLoadCpt props []

folderViewLoadCpt :: R.Component LoadProps
folderViewLoadCpt = here.component "folderViewLoadCpt" cpt where
  cpt {nodeId, session} _ = do
    useLoader {nodeId, session} loadFolders $
      \folders -> folderView {folders, nodeId, session}

type FolderViewProps = 
  ( 
    nodeId :: Int
  , folders:: FTree
  , session :: Session
  )

folderView :: Record FolderViewProps -> R.Element
folderView props = R.createElement folderViewCpt props []

folderViewCpt :: R.Component FolderViewProps
folderViewCpt = here.component "folderViewCpt" cpt where
  cpt {nodeId, session, folders: (NTree (LNode {parent_id: parentId}) (foldersS))} _ = do
    let sid = sessionId session 
    let children = makeFolderElements foldersS sid
    let parent = makeParentFolder parentId sid

    pure $ H.div {className: "folders"} $ parent <> children

  makeFolderElements :: Array (NTree LNode) -> SessionId -> Array R.Element
  makeFolderElements foldersS sid = makeFolderElementsMap <$> foldersS where
    makeFolderElementsMap :: NTree LNode -> R.Element
    makeFolderElementsMap (NTree (LNode node) _) = folder {style: FolderChild, text: node.name, nodeId: node.id, nodeType: node.nodeType, sid: sid} []

  makeParentFolder :: Maybe Int -> SessionId -> Array R.Element
  makeParentFolder (Just parentId) sid =
    -- FIXME: The NodeType here should not be hardcoded to FolderPrivate but we currently can't get the actual NodeType
    -- without performing another API call. Also parentId is never being returned by this API even when it clearly exists
    [ folder {style: FolderUp, text: "..", nodeId: parentId, nodeType: FolderPrivate, sid: sid} [] ]
  makeParentFolder Nothing _ = []


type FolderProps = 
  (
    style :: FolderStyle
  , text :: String
  , nodeType :: NodeType
  , nodeId :: Int
  , sid :: SessionId
  )

folder :: R2.Component FolderProps
folder = R.createElement folderCpt

folderCpt :: R.Component FolderProps
folderCpt = here.component "folderCpt" cpt where
  cpt {style, text, nodeId, sid, nodeType} _ = do
    pure $ H.a {className: "btn btn-primary", href: "/#/" <> getFolderPath nodeType sid nodeId}  [ H.i { className: "fa " <> (icon style nodeType) } []
                                                                   , H.br {}
                                                                   , H.text text]
  
  icon :: FolderStyle -> NodeType -> String
  icon FolderUp _ = "fa-folder-open"
  icon _ Dashboard = "fa-signal"
  icon _ Texts = "fa-newspaper-o"
  icon _ NodeList = "fa-list"
  icon _ Graph = "fa-hubzilla"
  icon _ NodeFile = "fa-file"
  icon FolderChild _  = "fa-folder"
  
  getFolderPath :: NodeType -> SessionId -> Int -> String
  getFolderPath nodeType sid nodeId = appPath $ fromMaybe Home $ nodeTypeAppRoute nodeType sid nodeId


type FieldsCodeEditorProps =
  (
    fields :: T.Box FTFieldsWithIndex
    | LoadProps
  )

fieldsCodeEditor :: R2.Component FieldsCodeEditorProps
fieldsCodeEditor = R.createElement fieldsCodeEditorCpt

fieldsCodeEditorCpt :: R.Component FieldsCodeEditorProps
fieldsCodeEditorCpt = here.component "fieldsCodeEditorCpt" cpt
  where
    cpt { fields, nodeId, session } _ = do
      fields' <- T.useLive T.unequal fields
      masterKey <- T.useBox T2.newReload
      masterKey' <- T.useLive T.unequal masterKey

      let editorsMap (Tuple idx field) =
            fieldCodeEditorWrapper { canMoveDown: idx < (List.length fields' - 1)
                                   , canMoveUp: idx > 0
                                   , field
                                   , key: (show masterKey') <> "-" <> (show idx)
                                   , onChange: onChange idx
                                   , onMoveDown: onMoveDown masterKey idx
                                   , onMoveUp: onMoveUp masterKey idx
                                   , onRemove: onRemove idx
                                   , onRename: onRename idx
                                   }

      pure $ H.div {} $ List.toUnfoldable (editorsMap <$> fields')
      where
        onChange :: Index -> FieldType -> Effect Unit
        onChange idx typ = do
          T.modify_ (\fs ->
            fromMaybe fs $
              List.modifyAt idx (\(Tuple _ (Field f)) -> Tuple idx (Field $ f { typ = typ })) fs) fields

        onMoveDown :: T2.ReloadS -> Index -> Unit -> Effect Unit
        onMoveDown masterKey idx _ = do
          T2.reload masterKey
          T.modify_ (recomputeIndices <<< (GDA.swapList idx (idx + 1))) fields

        onMoveUp :: T2.ReloadS -> Index -> Unit -> Effect Unit
        onMoveUp masterKey idx _ = do
          T2.reload masterKey
          T.modify_ (recomputeIndices <<< (GDA.swapList idx (idx - 1))) fields

        onRemove :: Index -> Unit -> Effect Unit
        onRemove idx _ = do
          T.modify_ (\fs -> fromMaybe fs $ List.deleteAt idx fs) fields

        onRename :: Index -> String -> Effect Unit
        onRename idx newName = do
          T.modify_ (\fs ->
            fromMaybe fs $ List.modifyAt idx (\(Tuple _ (Field f)) -> Tuple idx (Field $ f { name = newName })) fs) fields

    recomputeIndices :: FTFieldsWithIndex -> FTFieldsWithIndex
    recomputeIndices = List.mapWithIndex $ \idx -> \(Tuple _ t) -> Tuple idx t

hash :: FTFieldWithIndex -> Hash
hash (Tuple idx f) = Crypto.hash $ "--idx--" <> (show idx) <> "--field--" <> (show f)

type FieldCodeEditorProps =
  (
    canMoveDown :: Boolean
  , canMoveUp   :: Boolean
  , field       :: FTField
  , key         :: String
  , onChange    :: FieldType -> Effect Unit
  , onMoveDown  :: Unit -> Effect Unit
  , onMoveUp    :: Unit -> Effect Unit
  , onRemove    :: Unit -> Effect Unit
  , onRename    :: String -> Effect Unit
  )

fieldCodeEditorWrapper :: Record FieldCodeEditorProps -> R.Element
fieldCodeEditorWrapper props = R.createElement fieldCodeEditorWrapperCpt props []

fieldCodeEditorWrapperCpt :: R.Component FieldCodeEditorProps
fieldCodeEditorWrapperCpt = here.component "fieldCodeEditorWrapperCpt" cpt
  where
    cpt props@{canMoveDown, canMoveUp, field: Field {name, typ}, onMoveDown, onMoveUp, onRemove, onRename} _ = do
      pure $ H.div { className: "row card" } [
        H.div { className: "card-header" } [
          H.div { className: "code-editor-heading row" } [
              H.div { className: "col-4" } [
                 inputWithEnter { onBlur: onRename
                                , onEnter: \_ -> pure unit
                                , onValueChanged: onRename
                                , autoFocus: false
                                , className: "form-control"
                                , defaultValue: name
                                , placeholder: "Enter file name"
                                , type: "text" }
              ]
            , H.div { className: "col-7" } []
            , H.div { className: "buttons-right col-1" } ([
                H.div { className: "btn btn-danger"
                      , on: { click: \_ -> onRemove unit }
                      } [
                  H.span { className: "fa fa-trash" } [  ]
                  ]
              ] <> moveButtons)
            ]
         ]
        , H.div { className: "card-body" } [
           fieldCodeEditor props
           ]
        ]
      where
        moveButtons = [] <> (if canMoveDown then [moveDownButton] else [])
                         <> (if canMoveUp then [moveUpButton] else [])
        moveDownButton =
          H.div { className: "btn btn-primary"
                , on: { click: \_ -> onMoveDown unit }
                } [
            H.span { className: "fa fa-arrow-down" } [  ]
            ]
        moveUpButton =
          H.div { className: "btn btn-primary"
                , on: { click: \_ -> onMoveUp unit }
                } [
            H.span { className: "fa fa-arrow-up" } [  ]
            ]

type RenameableProps =
  (
    onRename :: String -> Effect Unit
  , text :: String
  )

renameable :: Record RenameableProps -> R.Element
renameable props = R.createElement renameableCpt props []

renameableCpt :: R.Component RenameableProps
renameableCpt = here.component "renameableCpt" cpt
  where
    cpt {onRename, text} _ = do
      isEditing <- T.useBox false
      state <- T.useBox text
      textRef <- R.useRef text

      -- handle props change of text
      R.useEffect1' text $ do
        if R.readRef textRef == text then
          pure unit
        else do
          R.setRef textRef text
          T.write_ text state

      pure $ H.div { className: "renameable" } [
        renameableText { isEditing, onRename, state }
      ]

type RenameableTextProps =
  (
    isEditing :: T.Box Boolean
  , onRename  :: String -> Effect Unit
  , state     :: T.Box String
  )

renameableText :: Record RenameableTextProps -> R.Element
renameableText props = R.createElement renameableTextCpt props []

renameableTextCpt :: R.Component RenameableTextProps
renameableTextCpt = here.component "renameableTextCpt" cpt
  where
    cpt { isEditing, onRename, state } _ = do
      isEditing' <- T.useLive T.unequal isEditing
      state' <- T.useLive T.unequal state

      pure $ if isEditing' then
              H.div { className: "input-group" }
                [ inputWithEnter {
                    autoFocus: false
                  , className: "form-control text"
                  , defaultValue: state'
                  , onBlur: \st -> T.write_ st state
                  , onEnter: submit state'
                  , onValueChanged: \st -> T.write_ st state
                  , placeholder: ""
                  , type: "text"
                  }
                , H.div { className: "btn input-group-append"
                        , on: { click: submit state' } }
                  [ H.span { className: "fa fa-floppy-o" } []
                  ]
                ]
             else
               H.div { className: "input-group" }
               [ H.input { className: "form-control"
                         , defaultValue: state'
                         , disabled: 1
                         , type: "text" }
               , H.div { className: "btn input-group-append"
                       , on: { click: \_ -> T.write_ true isEditing } }
                 [ H.span { className: "fa fa-pencil" } []
                 ]
               ]
      where
        submit text _ = do
          T.write_ false isEditing
          onRename text

fieldCodeEditor :: Record FieldCodeEditorProps -> R.Element
fieldCodeEditor props = R.createElement fieldCodeEditorCpt props []

fieldCodeEditorCpt :: R.Component FieldCodeEditorProps
fieldCodeEditorCpt = here.component "fieldCodeEditorCpt" cpt
  where
    cpt {field: Field {typ: typ@(Haskell {haskell})}, onChange} _ = do
      pure $ CE.codeEditor {code: haskell, defaultCodeType: CE.Haskell, onChange: changeCode onChange typ}

    cpt {field: Field {typ: typ@(Python {python})}, onChange} _ = do
      pure $ CE.codeEditor {code: python, defaultCodeType: CE.Python, onChange: changeCode onChange typ}

    cpt {field: Field {typ: typ@(JSON j)}, onChange} _ = do
      pure $ CE.codeEditor {code, defaultCodeType: CE.JSON, onChange: changeCode onChange typ}
      where
        code = R2.stringify (encodeJson j) 2

    cpt {field: Field {typ: typ@(Markdown {text})}, onChange} _ = do
      pure $ CE.codeEditor {code: text, defaultCodeType: CE.Markdown, onChange: changeCode onChange typ}

-- Performs the matrix of code type changes
-- (FieldType -> Effect Unit) is the callback function for fields array
-- FieldType is the current element that we will modify
-- CE.CodeType is the editor code type (might have been the cause of the trigger)
-- CE.Code is the editor code (might have been the cause of the trigger)
changeCode :: (FieldType -> Effect Unit) -> FieldType -> CE.CodeType -> CE.Code -> Effect Unit
changeCode onc (Haskell hs)        CE.Haskell  c = onc $ Haskell $ hs { haskell = c }
changeCode onc (Haskell hs)        CE.Python   c = onc $ Python   $ defaultPython'   { python  = c }
changeCode onc (Haskell {haskell}) CE.JSON     c = onc $ JSON     $ defaultJSON'     { desc = haskell }
changeCode onc (Haskell {haskell}) CE.Markdown c = onc $ Markdown $ defaultMarkdown' { text = haskell }

changeCode onc (Python hs)       CE.Python   c = onc $ Python  $ hs { python  = c }
changeCode onc (Python hs)       CE.Haskell  c = onc $ Haskell $ defaultHaskell' { haskell = c }
changeCode onc (Python {python}) CE.JSON     c = onc $ JSON     $ defaultJSON' { desc = python }
changeCode onc (Python {python}) CE.Markdown c = onc $ Markdown $ defaultMarkdown' { text = python }

changeCode onc (Markdown md) CE.Haskell  c = onc $ Haskell  $ defaultHaskell'  { haskell = c }
changeCode onc (Markdown md) CE.Python   c = onc $ Python   $ defaultPython'   { python  = c }
changeCode onc (Markdown md) CE.JSON     c = onc $ Markdown $ defaultMarkdown' { text    = c }
changeCode onc (Markdown md) CE.Markdown c = onc $ Markdown $ md               { text    = c }

changeCode onc (JSON j@{desc}) CE.Haskell c = onc $ Haskell $ defaultHaskell' { haskell = haskell }
  where
    haskell = R2.stringify (encodeJson j) 2
changeCode onc (JSON j@{desc}) CE.Python c = onc $ Python $ defaultPython' { python = toCode }
  where
    toCode = R2.stringify (encodeJson j) 2
changeCode onc (JSON j) CE.JSON c = do
  case jsonParser c of
    Left err -> log2 "[fieldCodeEditor'] cannot parse json" c
    Right j' -> case decodeJson j' of
      Left err -> log2 "[fieldCodeEditor'] cannot decode json" j'
      Right j'' -> onc $ JSON j''
changeCode onc (JSON j) CE.Markdown c = onc $ Markdown $ defaultMarkdown' { text = text }
  where
    text = R2.stringify (encodeJson j) 2




type LoadProps =
  ( nodeId  :: Int
  , session :: Session
  )

loadCorpus' :: Record LoadProps -> Aff (NodePoly Hyperdata)
loadCorpus' {nodeId, session} = get session $ NodeAPI Corpus (Just nodeId) ""

loadFolders :: Record LoadProps -> Aff FTree
loadFolders {nodeId, session} = get session $ TreeFirstLevel (Just nodeId) ""

-- Just to make reloading effective
loadCorpusWithReload :: { reload :: T2.Reload  | LoadProps } -> Aff (NodePoly Hyperdata)
loadCorpusWithReload {nodeId, session} = loadCorpus' {nodeId, session}

type SaveProps = (
  hyperdata :: Hyperdata
  | LoadProps
  )

saveCorpus :: Record SaveProps -> Aff Unit
saveCorpus {hyperdata, nodeId, session} = do
  id_ <- (put session (NodeAPI Corpus (Just nodeId) "") hyperdata) :: Aff Int
  pure unit

loadCorpus :: Record LoadProps -> Aff CorpusData
loadCorpus {nodeId, session} = do
  -- fetch corpus via lists parentId
  (NodePoly {parentId: corpusId} :: NodePoly {}) <- get session nodePolyRoute
  corpusNode     <-  get session $ corpusNodeRoute     corpusId ""
  defaultListIds <- (get session $ defaultListIdsRoute corpusId)
                    :: forall a. DecodeJson a => AffTableResult (NodePoly a)
  case (A.head defaultListIds.docs :: Maybe (NodePoly HyperdataList)) of
    Just (NodePoly { id: defaultListId }) ->
      pure {corpusId, corpusNode, defaultListId}
    Nothing ->
      throwError $ error "Missing default list"
  where
    nodePolyRoute       = NodeAPI Corpus (Just nodeId) ""
    corpusNodeRoute     = NodeAPI Corpus <<< Just
    defaultListIdsRoute = Children NodeList 0 1 Nothing <<< Just


loadCorpusWithChild :: Record LoadProps -> Aff CorpusData
loadCorpusWithChild { nodeId: childId, session } = do
  -- fetch corpus via lists parentId
  (NodePoly {parentId: corpusId} :: NodePoly {}) <- get session $ listNodeRoute childId ""
  corpusNode     <-  get session $ corpusNodeRoute     corpusId ""
  defaultListIds <- (get session $ defaultListIdsRoute corpusId)
                    :: forall a. DecodeJson a => AffTableResult (NodePoly a)
  case (A.head defaultListIds.docs :: Maybe (NodePoly HyperdataList)) of
    Just (NodePoly { id: defaultListId }) ->
      pure { corpusId, corpusNode, defaultListId }
    Nothing ->
      throwError $ error "Missing default list"
  where
    corpusNodeRoute     = NodeAPI Corpus <<< Just
    listNodeRoute       = NodeAPI Node <<< Just
    defaultListIdsRoute = Children NodeList 0 1 Nothing <<< Just


type LoadWithReloadProps =
  (
    reload :: T2.Reload
  | LoadProps
  )


-- Just to make reloading effective
loadCorpusWithChildAndReload :: Record LoadWithReloadProps -> Aff CorpusData
loadCorpusWithChildAndReload {nodeId, reload, session} = loadCorpusWithChild {nodeId, session}

data ViewType = Code | Folders
derive instance genericViewType :: Generic ViewType _
instance eqViewType :: Eq ViewType where
  eq = genericEq
instance showViewType :: Show ViewType where
  show = genericShow

type ViewTypeSelectorProps =
  (
    state :: R.State ViewType
  )

viewTypeSelector :: Record ViewTypeSelectorProps -> R.Element
viewTypeSelector p = R.createElement viewTypeSelectorCpt p []

viewTypeSelectorCpt :: R.Component ViewTypeSelectorProps
viewTypeSelectorCpt = here.component "viewTypeSelector" cpt
  where
    cpt {state} _ =
      pure $ H.div { className: "btn-group"
                   , role: "group" } [
          viewTypeButton Folders state
        , viewTypeButton Code state
        ]

    viewTypeButton viewType (state /\ setState) =
      H.button { className: "btn btn-primary" <> active
               , on: { click: onClick }
               , type: "button"
               } [
        H.i { className: "fa " <> (icon viewType) } []
      ]
      where
        active = if viewType == state then " active" else ""

        onClick _ = do
          setState $ const viewType

    icon Folders = "fa-folder"
    icon Code = "fa-code"
