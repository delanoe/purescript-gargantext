module Gargantext.Components.Nodes.Corpus where

import Data.Argonaut (class DecodeJson, decodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as A
import Data.Either (Either(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.Components.CodeEditor as CE
import Gargantext.Components.Node (NodePoly(..), HyperdataList)
import Gargantext.Components.Nodes.Corpus.Types (CorpusData, FTField, Field(..), FieldType(..), Hash, Hyperdata(..), defaultField, defaultHaskell', defaultJSON', defaultMarkdown')
import Gargantext.Data.Array as GDA
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(NodeAPI, Children))
import Gargantext.Sessions (Session, get, put)
import Gargantext.Types (NodeType(..), AffTableResult)
import Gargantext.Utils.Crypto as GUC
import Gargantext.Utils.Reactix as R2

type Props = (
    nodeId  :: Int
  , session :: Session
  )

type Reload = R.State Int

corpusLayout :: Record Props -> R.Element
corpusLayout props = R.createElement corpusLayoutCpt props []

corpusLayoutCpt :: R.Component Props
corpusLayoutCpt = R.hooksComponent "G.C.N.C.corpusLayout" cpt
  where
    cpt {nodeId, session} _ = do
      reload <- R.useState' 0

      useLoader {nodeId, reload: fst reload, session} loadCorpusWithReload $
        \corpus -> corpusLayoutView {corpus, nodeId, reload, session}

type ViewProps = (
    corpus  :: NodePoly Hyperdata
  , reload  :: Reload
  | Props
  )

-- We need FTFields with indices because it's the only way to identify the
-- FTField element inside a component (there are no UUIDs and such)
type Index = Int
type FTFieldWithIndex = Tuple Index FTField
type FTFieldsWithIndex = List.List FTFieldWithIndex

corpusLayoutView :: Record ViewProps -> R.Element
corpusLayoutView props = R.createElement corpusLayoutViewCpt props []

corpusLayoutViewCpt :: R.Component ViewProps
corpusLayoutViewCpt = R.hooksComponent "G.C.N.C.corpusLayoutView" cpt
  where
    cpt {corpus: (NodePoly {hyperdata: Hyperdata {fields}}), nodeId, reload, session} _ = do
      let fieldsWithIndex = List.mapWithIndex (\idx -> \t -> Tuple idx t) fields
      fieldsS <- R.useState' fieldsWithIndex

      pure $ H.div {} [
        H.div { className: "row" } [
           H.div { className: "btn btn-default " <> (saveEnabled fieldsWithIndex fieldsS)
                 , on: { click: onClickSave {fields: fieldsS, nodeId, reload, session} }
                 } [
              H.span { className: "glyphicon glyphicon-floppy-open" } [  ]
              ]
           ]
        , H.div {} [ fieldsCodeEditor { fields: fieldsS
                                      , nodeId
                                      , session } ]
        , H.div { className: "row" } [
           H.div { className: "btn btn-default"
                 , on: { click: onClickAdd fieldsS }
                 } [
              H.span { className: "glyphicon glyphicon-plus" } [  ]
              ]
           ]
        ]

    saveEnabled :: FTFieldsWithIndex -> R.State FTFieldsWithIndex -> String
    saveEnabled fs (fsS /\ _) = if fs == fsS then "disabled" else "enabled"

    onClickSave :: forall e. { fields :: R.State FTFieldsWithIndex
                             , nodeId :: Int
                             , reload :: R.State Int
                             , session :: Session } -> e -> Effect Unit
    onClickSave {fields: (fieldsS /\ _), nodeId, reload: (_ /\ setReload), session} _ = do
      log2 "[corpusLayoutViewCpt] onClickSave fieldsS" fieldsS
      launchAff_ do
        saveCorpus $ { hyperdata: Hyperdata {fields: (\(Tuple _ f) -> f) <$> fieldsS}
                     , nodeId
                     , session }
        liftEffect $ setReload $ (+) 1

    onClickAdd :: forall e. R.State FTFieldsWithIndex -> e -> Effect Unit
    onClickAdd (_ /\ setFieldsS) _ = do
      setFieldsS $ \fieldsS -> List.snoc fieldsS $ Tuple (List.length fieldsS) defaultField

type FieldsCodeEditorProps =
  (
    fields :: R.State FTFieldsWithIndex
    | LoadProps
  )

fieldsCodeEditor :: Record FieldsCodeEditorProps -> R.Element
fieldsCodeEditor props = R.createElement fieldsCodeEditorCpt props []

fieldsCodeEditorCpt :: R.Component FieldsCodeEditorProps
fieldsCodeEditorCpt = R.hooksComponent "G.C.N.C.fieldsCodeEditorCpt" cpt
  where
    cpt {nodeId, fields: fS@(fields /\ _), session} _ = do
      pure $ H.div {} $ List.toUnfoldable editors
      where
        editors = (\idxField@(Tuple idx field) ->
                    fieldCodeEditorWrapper { canMoveDown: idx < (List.length fields - 1)
                                           , canMoveUp: idx > 0
                                           , field
                                           , hash: hash idxField
                                           , onChange: onChange fS idx
                                           , onMoveDown: onMoveDown fS idx
                                           , onMoveUp: onMoveUp fS idx
                                           , onRemove: onRemove fS idx
                                           , onRename: onRename fS idx
                                           }) <$> fields

    onChange :: R.State FTFieldsWithIndex -> Index -> FieldType -> Effect Unit
    onChange (_ /\ setFields) idx typ = do
      setFields $ \fields ->
        case List.modifyAt idx (\(Tuple _ (Field f)) -> Tuple idx (Field $ f { typ = typ })) fields of
          Nothing -> fields
          Just newFields -> newFields

    onMoveDown :: R.State FTFieldsWithIndex -> Index -> Unit -> Effect Unit
    onMoveDown (fs /\ setFields) idx _ = do
      setFields $ recomputeIndices <<< (GDA.swapList idx (idx + 1))

    onMoveUp :: R.State FTFieldsWithIndex -> Index -> Unit -> Effect Unit
    onMoveUp (_ /\ setFields) idx _ = do
      setFields $ recomputeIndices <<< (GDA.swapList idx (idx - 1))

    onRemove :: R.State FTFieldsWithIndex -> Index -> Unit -> Effect Unit
    onRemove (_ /\ setFields) idx _ = do
      setFields $ \fields ->
        case List.deleteAt idx fields of
          Nothing -> fields
          Just newFields -> recomputeIndices newFields

    onRename :: R.State FTFieldsWithIndex -> Index -> String -> Effect Unit
    onRename (_ /\ setFields) idx newName = do
      setFields $ \fields ->
        case List.modifyAt idx (\(Tuple _ (Field f)) -> Tuple idx (Field $ f { name = newName })) fields of
          Nothing -> fields
          Just newFields -> newFields

    recomputeIndices :: FTFieldsWithIndex -> FTFieldsWithIndex
    recomputeIndices = List.mapWithIndex $ \idx -> \(Tuple _ t) -> Tuple idx t

hash :: FTFieldWithIndex -> Hash
hash (Tuple idx f) = GUC.md5 $ "--idx--" <> (show idx) <> "--field--" <> (show f)

type FieldCodeEditorProps =
  (
    canMoveDown :: Boolean
  , canMoveUp :: Boolean
  , field :: FTField
  , hash :: Hash
  , onChange :: FieldType -> Effect Unit
  , onMoveDown :: Unit -> Effect Unit
  , onMoveUp :: Unit -> Effect Unit
  , onRemove :: Unit -> Effect Unit
  , onRename :: String -> Effect Unit
  )

fieldCodeEditorWrapper :: Record FieldCodeEditorProps -> R.Element
fieldCodeEditorWrapper props = R.createElement fieldCodeEditorWrapperCpt props []

fieldCodeEditorWrapperCpt :: R.Component FieldCodeEditorProps
fieldCodeEditorWrapperCpt = R.hooksComponent "G.C.N.C.fieldCodeEditorWrapperCpt" cpt
  where
    cpt props@{canMoveDown, canMoveUp, field: Field {name, typ}, hash, onMoveDown, onMoveUp, onRemove, onRename} _ = do
      pure $ H.div { className: "row panel panel-default hash-" <> hash } [
        H.div { className: "panel-heading" } [
          H.div { className: "code-editor-heading" } [
              renameable {onRename, text: name}
            , H.div { className: "buttons-right" } [
                H.div { className: "btn btn-danger"
                      , on: { click: \_ -> onRemove unit }
                      } [
                  H.span { className: "glyphicon glyphicon-trash" } [  ]
                  ]
                ]
              , moveDownButton canMoveDown
              , moveUpButton canMoveUp
            ]
         ]
        , H.div { className: "panel-body" } [
           fieldCodeEditor props
           ]
        ]
      where
        moveDownButton false = H.div {} []
        moveDownButton true =
          H.div { className: "btn btn-default"
                , on: { click: \_ -> onMoveDown unit }
                } [
            H.span { className: "glyphicon glyphicon-arrow-down" } [  ]
            ]
        moveUpButton false = H.div {} []
        moveUpButton true =
          H.div { className: "btn btn-default"
                , on: { click: \_ -> onMoveUp unit }
                } [
            H.span { className: "glyphicon glyphicon-arrow-up" } [  ]
            ]

type RenameableProps =
  (
    onRename :: String -> Effect Unit
  , text :: String
  )

renameable :: Record RenameableProps -> R.Element
renameable props = R.createElement renameableCpt props []

renameableCpt :: R.Component RenameableProps
renameableCpt = R.hooksComponent "G.C.N.C.renameableCpt" cpt
  where
    cpt {onRename, text} _ = do
      isEditing <- R.useState' false
      state <- R.useState' text

      pure $ H.div { className: "renameable" } [
        textCpt isEditing state
      ]
      where
        textCpt :: R.State Boolean -> R.State String -> R.Element
        textCpt (false /\ setIsEditing) (text /\ _) = H.div {} [
            H.span { className: "text" } [ H.text text ]
          , H.span { className: "btn btn-default"
                  , on: { click: \_ -> setIsEditing $ const true } } [
              H.span { className: "glyphicon glyphicon-pencil" } []
            ]
        ]
        textCpt (true /\ setIsEditing) (text /\ setText) = H.div {} [
            H.input { defaultValue: text
                    , className: "form-control text"
                    , on: { change: \e -> setText $ const $ R2.unsafeEventValue e } }
          , H.span { className: "btn btn-default"
                  , on: { click: \_ -> do
                          setIsEditing $ const false
                          onRename text
                        } } [
              H.span { className: "glyphicon glyphicon-floppy-open" } []
            ]
        ]

fieldCodeEditor :: Record FieldCodeEditorProps -> R.Element
fieldCodeEditor props = R.createElement fieldCodeEditorCpt props []

fieldCodeEditorCpt :: R.Component FieldCodeEditorProps
fieldCodeEditorCpt = R.hooksComponent "G.C.N.C.fieldCodeEditorCpt" cpt
  where
    cpt {field: Field {typ: typ@(Haskell {haskell})}, onChange} _ = do
      pure $ CE.codeEditor {code: haskell, defaultCodeType: CE.Haskell, onChange: changeCode onChange typ}
    cpt {field: Field {typ: typ@(JSON j)}, onChange} _ = do
      pure $ CE.codeEditor {code, defaultCodeType: CE.JSON, onChange: changeCode onChange typ}
      where
        code = R2.stringify (encodeJson j) 2
    cpt {field: Field {typ: typ@(Markdown {text})}, onChange} _ = do
      pure $ CE.codeEditor {code: text, defaultCodeType: CE.Markdown, onChange: changeCode onChange typ}

-- Perofrms the matrix of code type changes
-- (FieldType -> Effect Unit) is the callback function for fields array
-- FieldType is the current element that we will modify
-- CE.CodeType is the editor code type (might have been the cause of the trigger)
-- CE.Code is the editor code (might have been the cause of the trigger)
changeCode :: (FieldType -> Effect Unit) -> FieldType -> CE.CodeType -> CE.Code -> Effect Unit
changeCode onc (Haskell hs) CE.Haskell c = onc $ Haskell $ hs { haskell = c }
changeCode onc (Haskell {haskell}) CE.JSON c = onc $ JSON $ defaultJSON' { desc = haskell }
changeCode onc (Haskell {haskell}) CE.Markdown c = onc $ Markdown $ defaultMarkdown' { text = haskell }
changeCode onc (JSON j@{desc}) CE.Haskell c = onc $ Haskell $ defaultHaskell' { haskell = haskell }
  where
    haskell = R2.stringify (encodeJson j) 2
changeCode onc (JSON j) CE.JSON c = do
  case jsonParser c of
    Left err -> log2 "[fieldCodeEditor'] cannot parse json" c
    Right j' -> case decodeJson j' of
      Left err -> log2 "[fieldCodeEditor'] cannot decode json" j'
      Right j'' -> onc $ JSON j''
changeCode onc (JSON j) CE.Markdown c = onc $ Markdown $ defaultMarkdown' { text = text }
  where
    text = R2.stringify (encodeJson j) 2
changeCode onc (Markdown md) CE.Haskell c = onc $ Haskell $ defaultHaskell' { haskell = c }
changeCode onc (Markdown md) CE.JSON c = onc $ Markdown $ defaultMarkdown' { text = c }
changeCode onc (Markdown md) CE.Markdown c = onc $ Markdown $ md { text = c }

type LoadProps = (
    nodeId  :: Int
  , session :: Session
  )

loadCorpus' :: Record LoadProps -> Aff (NodePoly Hyperdata)
loadCorpus' {nodeId, session} = get session $ NodeAPI Corpus (Just nodeId) ""

-- Just to make reloading effective
loadCorpusWithReload :: {reload :: Int  | LoadProps} -> Aff (NodePoly Hyperdata)
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
loadCorpusWithChild {nodeId:childId, session} = do
  -- fetch corpus via lists parentId
  (NodePoly {parentId: corpusId} :: NodePoly {}) <- get session $ listNodeRoute childId ""
  corpusNode     <-  get session $ corpusNodeRoute     corpusId ""
  defaultListIds <- (get session $ defaultListIdsRoute corpusId)
                    :: forall a. DecodeJson a => AffTableResult (NodePoly a)
  case (A.head defaultListIds.docs :: Maybe (NodePoly HyperdataList)) of
    Just (NodePoly { id: defaultListId }) ->
      pure {corpusId, corpusNode, defaultListId}
    Nothing ->
      throwError $ error "Missing default list"
  where
    corpusNodeRoute     = NodeAPI Corpus <<< Just
    listNodeRoute       = NodeAPI Node <<< Just
    defaultListIdsRoute = Children NodeList 0 1 Nothing <<< Just

