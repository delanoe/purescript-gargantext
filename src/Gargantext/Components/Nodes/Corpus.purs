module Gargantext.Components.Nodes.Corpus where

import Data.Array as A
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.CodeEditor as CE
import Gargantext.Components.FolderView as FV
import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Components.Node (NodePoly(..), HyperdataList)
import Gargantext.Components.Nodes.Corpus.Types (CorpusData, Hyperdata(..))
import Gargantext.Components.Nodes.Types (FTField, FTFieldList(..), FTFieldWithIndex, FTFieldsWithIndex(..), Field(..), FieldType(..), Hash, Index, defaultField, defaultHaskell', defaultJSON', defaultMarkdown', defaultPython')
import Gargantext.Config.REST (RESTError(..))
import Gargantext.Data.Array as GDA
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude (class Eq, class Show, Unit, bind, discard, pure, show, unit, ($), (+), (-), (<), (<$>), (<<<), (<>), (==), (>))
import Gargantext.Routes (SessionRoute(Children, NodeAPI))
import Gargantext.Sessions (Session, get, put, sessionId)
import Gargantext.Types (AffETableResult, NodeType(..))
import Gargantext.Utils.Crypto as Crypto
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus"

type Props =
  ( boxes   :: Boxes
  , nodeId  :: Int
  , session :: Session )

corpusLayout :: R2.Leaf Props
corpusLayout props = R.createElement corpusLayoutCpt props []
corpusLayoutCpt :: R.Component Props
corpusLayoutCpt = here.component "corpusLayout" cpt where
  cpt { boxes, nodeId, session } _ = do
    pure $ corpusLayoutMain { boxes, key, nodeId, session }
      where
        key = show (sessionId session) <> "-" <> show nodeId

type KeyProps =
  ( boxes :: Boxes
  , key     :: String
  , nodeId  :: Int
  , session :: Session
  )

corpusLayoutMain :: R2.Leaf KeyProps
corpusLayoutMain props = R.createElement corpusLayoutMainCpt props []
corpusLayoutMainCpt :: R.Component KeyProps
corpusLayoutMainCpt = here.component "corpusLayoutMain" cpt
  where
    cpt { boxes, key, nodeId, session } _ = do
      viewType <- T.useBox Folders

      pure $ H.div {} [
        H.div {} [
          H.div { className: "row" } [
            H.div { className: "col-1" } [ viewTypeSelector {state: viewType} ]
          , H.div { className: "col-1" } [ FV.homeButton ]
          ]
        ]
      , H.div {} [corpusLayoutSelection { boxes, key, session, state: viewType, nodeId }]
      ]

type SelectionProps = 
  ( boxes   :: Boxes
  , nodeId  :: Int
  , key     :: String
  , session :: Session
  , state   :: T.Box ViewType
  )

corpusLayoutSelection :: R2.Leaf SelectionProps
corpusLayoutSelection props = R.createElement corpusLayoutSelectionCpt props []
corpusLayoutSelectionCpt :: R.Component SelectionProps
corpusLayoutSelectionCpt = here.component "corpusLayoutSelection" cpt where
  cpt { boxes, key, nodeId, session, state } _ = do
    state' <- T.useLive T.unequal state

    pure $ renderContent state' nodeId session key boxes

  renderContent Folders nodeId session _ boxes =
    FV.folderView { backFolder: true
                  , boxes
                  , nodeId
                  , session
                   }
  renderContent Code nodeId session key _ = corpusLayoutWithKey { key, nodeId, session }

type CorpusKeyProps =
  ( nodeId  :: Int
  , key     :: String
  , session :: Session
  )

corpusLayoutWithKey :: R2.Leaf CorpusKeyProps
corpusLayoutWithKey props = R.createElement corpusLayoutWithKeyCpt props []
corpusLayoutWithKeyCpt :: R.Component CorpusKeyProps
corpusLayoutWithKeyCpt = here.component "corpusLayoutWithKey" cpt where
  cpt { nodeId, session } _ = do
    reload <- T.useBox T2.newReload
    reload' <- T.useLive T.unequal reload
    useLoader { errorHandler
              , loader: loadCorpusWithReload
              , path: { nodeId, reload: reload', session }
              , render: \corpus -> corpusLayoutView { corpus, nodeId, reload, session } }
    where
      errorHandler err = here.log2 "[corpusLayoutWithKey] RESTError" err

type ViewProps =
  ( corpus  :: NodePoly Hyperdata
  , nodeId  :: Int
  , reload  :: T2.ReloadS
  , session :: Session
  )

corpusLayoutView :: Record ViewProps -> R.Element
corpusLayoutView props = R.createElement corpusLayoutViewCpt props []
corpusLayoutViewCpt :: R.Component ViewProps
corpusLayoutViewCpt = here.component "corpusLayoutView" cpt
  where
    cpt {corpus: (NodePoly {hyperdata: Hyperdata {fields: FTFieldList fields}}), nodeId, reload, session} _ = do
      let fieldsWithIndex = FTFieldsWithIndex $ List.mapWithIndex (\idx -> \ftField -> { idx, ftField }) fields
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
    onClickSave {fields: FTFieldsWithIndex fields, nodeId, reload, session} _ = do
      launchAff_ do
        res <- saveCorpus $ { hyperdata: Hyperdata {fields: FTFieldList $ (_.ftField) <$> fields}
                            , nodeId
                            , session }
        liftEffect $ do
          _ <- case res of
                Left err -> here.log2 "[corpusLayoutView] onClickSave RESTError" err
                _ -> pure unit
          T2.reload reload

    onClickAdd :: forall e. T.Box FTFieldsWithIndex -> e -> Effect Unit
    onClickAdd fieldsS _ = do
      T.modify_ (\(FTFieldsWithIndex fs) -> FTFieldsWithIndex $ 
        List.snoc fs $ { idx: List.length fs, ftField: defaultField }) fieldsS


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
    cpt { fields } _ = do
      (FTFieldsWithIndex fields') <- T.useLive T.unequal fields
      masterKey <- T.useBox T2.newReload
      masterKey' <- T.useLive T.unequal masterKey

      let editorsMap { idx, ftField } =
            fieldCodeEditorWrapper { canMoveDown: idx < (List.length fields' - 1)
                                   , canMoveUp: idx > 0
                                   , field: ftField
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
          T.modify_ (\(FTFieldsWithIndex fs) ->
            FTFieldsWithIndex $ fromMaybe fs $
              List.modifyAt idx (\{ ftField: Field f} -> { idx, ftField: Field $ f { typ = typ } }) fs) fields

        onMoveDown :: T2.ReloadS -> Index -> Unit -> Effect Unit
        onMoveDown masterKey idx _ = do
          T2.reload masterKey
          T.modify_ (\(FTFieldsWithIndex fs) -> recomputeIndices $ FTFieldsWithIndex $ GDA.swapList idx (idx + 1) fs) fields

        onMoveUp :: T2.ReloadS -> Index -> Unit -> Effect Unit
        onMoveUp masterKey idx _ = do
          T2.reload masterKey
          T.modify_ (\(FTFieldsWithIndex fs) -> recomputeIndices $ FTFieldsWithIndex $ GDA.swapList idx (idx - 1) fs) fields

        onRemove :: Index -> Unit -> Effect Unit
        onRemove idx _ = do
          T.modify_ (\(FTFieldsWithIndex fs) -> FTFieldsWithIndex $ fromMaybe fs $ List.deleteAt idx fs) fields

        onRename :: Index -> String -> Effect Unit
        onRename idx newName = do
          T.modify_ (\(FTFieldsWithIndex fs) ->
            FTFieldsWithIndex $ fromMaybe fs $
              List.modifyAt idx (\{ ftField: Field f } -> { idx, ftField: Field $ f { name = newName } }) fs) fields

    recomputeIndices :: FTFieldsWithIndex -> FTFieldsWithIndex
    recomputeIndices (FTFieldsWithIndex lst) = FTFieldsWithIndex $ List.mapWithIndex (\idx -> \{ ftField } -> { idx, ftField }) lst

hash :: FTFieldWithIndex -> Hash
hash { idx, ftField } = Crypto.hash $ "--idx--" <> (show idx) <> "--field--" <> (show ftField)

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
        code = R2.stringify (JSON.writeImpl j) 2

    cpt {field: Field {typ: typ@(Markdown {text})}, onChange} _ = do
      pure $ CE.codeEditor {code: text, defaultCodeType: CE.Markdown, onChange: changeCode onChange typ}

-- Performs the matrix of code type changes
-- (FieldType -> Effect Unit) is the callback function for fields array
-- FieldType is the current element that we will modify
-- CE.CodeType is the editor code type (might have been the cause of the trigger)
-- CE.Code is the editor code (might have been the cause of the trigger)
changeCode :: (FieldType -> Effect Unit) -> FieldType -> CE.CodeType -> CE.Code -> Effect Unit
changeCode onc (Haskell hs)        CE.Haskell  c = onc $ Haskell $ hs { haskell = c }
changeCode onc (Haskell _)         CE.Python   c = onc $ Python   $ defaultPython'   { python  = c }
changeCode onc (Haskell {haskell}) CE.JSON     _ = onc $ JSON     $ defaultJSON'     { desc = haskell }
changeCode onc (Haskell {haskell}) CE.Markdown _ = onc $ Markdown $ defaultMarkdown' { text = haskell }

changeCode onc (Python hs)       CE.Python   c = onc $ Python  $ hs { python  = c }
changeCode onc (Python _)        CE.Haskell  c = onc $ Haskell $ defaultHaskell' { haskell = c }
changeCode onc (Python {python}) CE.JSON     _ = onc $ JSON     $ defaultJSON' { desc = python }
changeCode onc (Python {python}) CE.Markdown _ = onc $ Markdown $ defaultMarkdown' { text = python }

changeCode onc (Markdown _)  CE.Haskell  c = onc $ Haskell  $ defaultHaskell'  { haskell = c }
changeCode onc (Markdown _)  CE.Python   c = onc $ Python   $ defaultPython'   { python  = c }
changeCode onc (Markdown _)  CE.JSON     c = onc $ Markdown $ defaultMarkdown' { text    = c }
changeCode onc (Markdown md) CE.Markdown c = onc $ Markdown $ md               { text    = c }

changeCode onc (JSON j) CE.Haskell _ = onc $ Haskell $ defaultHaskell' { haskell = haskell }
  where
    haskell = R2.stringify (JSON.writeImpl j) 2
changeCode onc (JSON j) CE.Python _ = onc $ Python $ defaultPython' { python = toCode }
  where
    toCode = R2.stringify (JSON.writeImpl j) 2
changeCode onc _ CE.JSON c = do
  case JSON.readJSON c of
    Left err -> here.log2 "[fieldCodeEditor'] cannot parse json" c  -- TODO Refactor?
    Right j' -> onc $ JSON j'
  -- case jsonParser c of
  --   Left err -> here.log2 "[fieldCodeEditor'] cannot parse json" c
  --   Right j' -> case decodeJson j' of
  --     Left err -> here.log2 "[fieldCodeEditor'] cannot decode json" j'
  --     Right j'' -> onc $ JSON j''
changeCode onc (JSON j) CE.Markdown _ = onc $ Markdown $ defaultMarkdown' { text = text }
  where
    text = R2.stringify (JSON.writeImpl j) 2


type LoadProps =
  ( nodeId  :: Int
  , session :: Session
  )

loadCorpus' :: Record LoadProps -> Aff (Either RESTError (NodePoly Hyperdata))
loadCorpus' {nodeId, session} = get session $ NodeAPI Corpus (Just nodeId) ""

-- Just to make reloading effective
loadCorpusWithReload :: { reload :: T2.Reload  | LoadProps } -> Aff (Either RESTError (NodePoly Hyperdata))
loadCorpusWithReload {nodeId, session} = loadCorpus' {nodeId, session}

type SaveProps = (
  hyperdata :: Hyperdata
  | LoadProps
  )

saveCorpus :: Record SaveProps -> Aff (Either RESTError Int)
saveCorpus {hyperdata, nodeId, session} = do
  put session (NodeAPI Corpus (Just nodeId) "") hyperdata

loadCorpus :: Record LoadProps -> Aff (Either RESTError CorpusData)
loadCorpus {nodeId, session} = do
  -- fetch corpus via lists parentId
  res <- get session nodePolyRoute
  case res of
    Left err -> pure $ Left err
    Right (NodePoly {parentId: corpusId} :: NodePoly {}) -> do
      eCorpusNode     <-  get session $ corpusNodeRoute     corpusId ""
      eDefaultListIds <- (get session $ defaultListIdsRoute corpusId)
                      :: forall a. JSON.ReadForeign a => AffETableResult (NodePoly a)
      case eCorpusNode of
        Left err -> pure $ Left err
        Right corpusNode -> do
          case eDefaultListIds of
            Left err -> pure $ Left err
            Right defaultListIds -> do
              case (A.head defaultListIds.docs :: Maybe (NodePoly HyperdataList)) of
                Just (NodePoly { id: defaultListId }) ->
                  pure $ Right { corpusId, corpusNode, defaultListId }
                Nothing ->
                  pure $ Left $ CustomError "Missing default list"

--  (NodePoly {parentId: corpusId} :: NodePoly {}) <- get session nodePolyRoute
--  corpusNode     <-  get session $ corpusNodeRoute     corpusId ""
--  defaultListIds <- (get session $ defaultListIdsRoute corpusId)
--                    :: forall a. JSON.ReadForeign a => AffTableResult (NodePoly a)
--  case (A.head defaultListIds.docs :: Maybe (NodePoly HyperdataList)) of
--    Just (NodePoly { id: defaultListId }) ->
--      pure {corpusId, corpusNode, defaultListId}
--    Nothing ->
--      throwError $ error "Missing default list"
  where
    nodePolyRoute       = NodeAPI Corpus (Just nodeId) ""
    corpusNodeRoute     = NodeAPI Corpus <<< Just
    defaultListIdsRoute = Children NodeList 0 1 Nothing <<< Just


loadCorpusWithChild :: Record LoadProps -> Aff (Either RESTError CorpusData)
loadCorpusWithChild { nodeId: childId, session } = do
  -- fetch corpus via lists parentId
  eListNode <- get session $ listNodeRoute childId ""
  case eListNode of
    Left err -> pure $ Left err
    Right listNode -> do
      let (NodePoly {parentId: corpusId} :: NodePoly {}) = listNode
      eCorpusNode     <-  get session $ corpusNodeRoute     corpusId ""
      case eCorpusNode of
        Left err -> pure $ Left err
        Right corpusNode -> do
          eDefaultListIds <- (get session $ defaultListIdsRoute corpusId)
                             :: forall a. JSON.ReadForeign a => AffETableResult (NodePoly a)
          case eDefaultListIds of
            Left err -> pure $ Left err
            Right defaultListIds -> do
              case (A.head defaultListIds.docs :: Maybe (NodePoly HyperdataList)) of
                Just (NodePoly { id: defaultListId }) ->
                  pure $ Right { corpusId, corpusNode, defaultListId }
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
loadCorpusWithChildAndReload :: Record LoadWithReloadProps -> Aff (Either RESTError CorpusData)
loadCorpusWithChildAndReload {nodeId, session} = loadCorpusWithChild {nodeId, session}

data ViewType = Code | Folders
derive instance Generic ViewType _
instance Eq ViewType where
  eq = genericEq
instance Show ViewType where
  show = genericShow

type ViewTypeSelectorProps =
  (
    state :: T.Box ViewType
  )

viewTypeSelector :: Record ViewTypeSelectorProps -> R.Element
viewTypeSelector p = R.createElement viewTypeSelectorCpt p []
viewTypeSelectorCpt :: R.Component ViewTypeSelectorProps
viewTypeSelectorCpt = here.component "viewTypeSelector" cpt
  where
    cpt {state} _ = do
      state' <- T.useLive T.unequal state

      pure $ H.div { className: "btn-group"
                   , role: "group" } [
          viewTypeButton Folders state' state
        , viewTypeButton Code state' state
        ]

    viewTypeButton viewType state' state =
      H.button { className: "btn btn-primary" <> active
               , on: { click: \_ -> T.write viewType state }
               , type: "button"
               } [
        H.i { className: "fa " <> (icon viewType) } []
      ]
      where
        active = if viewType == state' then " active" else ""

    icon Folders = "fa-folder"
    icon Code = "fa-code"
