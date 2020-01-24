module Gargantext.Components.Nodes.Corpus where

import Data.Argonaut (class DecodeJson, decodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as A
import Data.Either (Either(..))
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
import Gargantext.Components.Nodes.Corpus.Types
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(NodeAPI, Children))
import Gargantext.Sessions (Session, get, put)
import Gargantext.Types (NodeType(..), AffTableResult)
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

type Index = Int
type FTFieldWithIndex = Tuple Index FTField

corpusLayoutView :: Record ViewProps -> R.Element
corpusLayoutView props = R.createElement corpusLayoutViewCpt props []

corpusLayoutViewCpt :: R.Component ViewProps
corpusLayoutViewCpt = R.hooksComponent "G.C.N.C.corpusLayoutView" cpt
  where
    cpt {corpus: (NodePoly {hyperdata: Hyperdata {fields}}), nodeId, reload, session} _ = do
      let fieldsWithIndex = A.mapWithIndex (\idx -> \t -> Tuple idx t) fields
      fieldsS <- R.useState' fieldsWithIndex

      pure $ H.div {} [
          H.div { className: "row" } [
            H.div { className: "btn btn-default " <> (saveEnabled fieldsWithIndex fieldsS)
                  , on: { click: onClickSave {fields: fieldsS, nodeId, reload, session} }
                  } [
               H.span { className: "glyphicon glyphicon-save" } [  ]
            ]
          ]
        , H.div {} [ fieldsCodeEditor {nodeId, session, fields: fieldsS} ]
        , H.div { className: "row" } [
            H.div { className: "btn btn-default"
                  , on: { click: onClickAdd fieldsS }
                  } [
              H.span { className: "glyphicon glyphicon-plus" } [  ]
            ]
          ]
        ]

    saveEnabled :: Array FTFieldWithIndex -> R.State (Array FTFieldWithIndex) -> String
    saveEnabled fs (fsS /\ _) = if fs == fsS then "disabled" else "enabled"

    onClickSave :: forall e. { fields :: R.State (Array FTFieldWithIndex)
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

    onClickAdd :: forall e. R.State (Array FTFieldWithIndex) -> e -> Effect Unit
    onClickAdd (_ /\ setFieldsS) _ = do
      setFieldsS $ \fieldsS -> A.snoc fieldsS $ Tuple (A.length fieldsS) defaultField

type FieldsCodeEditorProps =
  (
    fields :: R.State (Array FTFieldWithIndex)
    | LoadProps
  )

fieldsCodeEditor :: Record FieldsCodeEditorProps -> R.Element
fieldsCodeEditor props = R.createElement fieldsCodeEditorCpt props []

fieldsCodeEditorCpt :: R.Component FieldsCodeEditorProps
fieldsCodeEditorCpt = R.hooksComponent "G.C.N.C.fieldsCodeEditorCpt" cpt
  where
    cpt {nodeId, fields: fS@(fields /\ setFields), session} _ = do
      pure $ H.div {} $
        (\(Tuple idx field) ->
          fieldCodeEditorWrapper { field
                                 , onChange: onChange fS idx
                                 }) <$> fields

    onChange :: R.State (Array FTFieldWithIndex) -> Index -> FieldType -> Effect Unit
    onChange (_ /\ setFields) idx typ = do
      setFields $ \fields ->
        case A.modifyAt idx (\(Tuple _ (Field f)) -> Tuple idx (Field $ f { typ = typ })) fields of
          Nothing -> fields
          Just newFields -> newFields

type FieldCodeEditorProps =
  (
    field :: FTField
  , onChange :: FieldType -> Effect Unit
  )

fieldCodeEditorWrapper :: Record FieldCodeEditorProps -> R.Element
fieldCodeEditorWrapper props = R.createElement fieldCodeEditorWrapperCpt props []

fieldCodeEditorWrapperCpt :: R.Component FieldCodeEditorProps
fieldCodeEditorWrapperCpt = R.hooksComponent "G.C.N.C.fieldCodeEditorWrapperCpt" cpt
  where
    cpt props@{field: Field {name, typ}} _ = do
      pure $ H.div { className: "row panel panel-default" } [
        H.div { className: "panel-heading" } [ H.text name ]
        , H.div { className: "panel-body" } [
           fieldCodeEditor props
           ]
        ]

fieldCodeEditor :: Record FieldCodeEditorProps -> R.Element
fieldCodeEditor props = R.createElement fieldCodeEditorCpt props []

fieldCodeEditorCpt :: R.Component FieldCodeEditorProps
fieldCodeEditorCpt = R.hooksComponent "G.C.N.C.fieldCodeEditorCpt" cpt
  where
    cpt {field: Field {typ: Haskell hs@{code}}, onChange} _ = do
      pure $ CE.codeEditor {code, defaultCodeType: CE.Haskell, onChange: onChange'}
      where
        onChange' :: CE.Code -> Effect Unit
        onChange' c = do
          onChange $ Haskell $ hs { code = c }
    cpt {field: Field {typ: JSON j}, onChange} _ = do
      pure $ CE.codeEditor {code, defaultCodeType: CE.JSON, onChange: onChange'}
      where
        code = R2.stringify (encodeJson j) 2

        onChange' :: CE.Code -> Effect Unit
        onChange' c = do
          case jsonParser c of
            Left err -> log2 "[fieldCodeEditor'] cannot parse json" c
            Right j' -> case decodeJson j' of
              Left err -> log2 "[fieldCodeEditor'] cannot decode json" j'
              Right j'' -> onChange $ JSON j''
    cpt {field: Field {typ: Markdown md@{text}}, onChange} _ = do
      pure $ CE.codeEditor {code: text, defaultCodeType: CE.Markdown, onChange: onChange'}
      where
        onChange' :: CE.Code -> Effect Unit
        onChange' c = do
          onChange $ Markdown $ md { text = c }

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
  corpusNode     <- get session $ corpusNodeRoute     corpusId ""
  defaultListIds <- (get session $ defaultListIdsRoute corpusId) :: forall a. DecodeJson a => AffTableResult (NodePoly a)
  case (A.head defaultListIds.docs :: Maybe (NodePoly HyperdataList)) of
    Just (NodePoly { id: defaultListId }) ->
      pure {corpusId, corpusNode, defaultListId}
    Nothing ->
      throwError $ error "Missing default list"
  where
    nodePolyRoute       = NodeAPI Corpus (Just nodeId) ""
    corpusNodeRoute     = NodeAPI Corpus <<< Just
    defaultListIdsRoute = Children NodeList 0 1 Nothing <<< Just
