module Gargantext.Components.Nodes.Corpus where

import Data.Argonaut (class DecodeJson, decodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as A
import Data.Either (Either(..))
import Data.List as List
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T
import Gargantext.Prelude

import Gargantext.Components.CodeEditor as CE
import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Components.Node (NodePoly(..), HyperdataList)
import Gargantext.Components.Nodes.Types
import Gargantext.Components.Nodes.Corpus.Types (CorpusData, Hyperdata(..))
import Gargantext.Data.Array as GDA
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(NodeAPI, Children))
import Gargantext.Sessions (Session, get, put, sessionId)
import Gargantext.Types (NodeType(..), AffTableResult)
import Gargantext.Utils.Crypto as Crypto
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Reload as GUR
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus"

type Props = ( nodeId  :: Int, session :: R.Context Session )

corpusLayout :: R2.Leaf Props
corpusLayout props = R.createElement corpusLayoutCpt props []

corpusLayoutCpt :: R.Component Props
corpusLayoutCpt = here.component "corpusLayout" cpt where
  cpt { nodeId, session } _ = cp <$> R.useContext session where
    cp s = corpusLayoutWithKey { key, nodeId, session: s } where
      key = show (sessionId s) <> "-" <> show nodeId

type KeyProps =
  ( nodeId  :: Int
  , key     :: String
  , session :: Session
  )

corpusLayoutWithKey :: R2.Leaf KeyProps
corpusLayoutWithKey props = R.createElement corpusLayoutWithKeyCpt props []

corpusLayoutWithKeyCpt :: R.Component KeyProps
corpusLayoutWithKeyCpt = here.component "corpusLayoutWithKey" cpt where
  cpt { nodeId, session } _ = do
    reload <- GUR.new
    let reload' = GUR.value reload
    useLoader { nodeId, reload: reload', session } loadCorpusWithReload $
      \corpus -> corpusLayoutView { corpus, nodeId, reload, session }

type ViewProps =
  ( corpus  :: NodePoly Hyperdata
  , reload  :: GUR.ReloadS
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
      fieldsS <- R.useState' fieldsWithIndex
      fieldsRef <- R.useRef fields

      -- handle props change of fields
      R.useEffect1' fields $ do
        if R.readRef fieldsRef == fields then
          pure unit
        else do
          R.setRef fieldsRef fields
          snd fieldsS $ const fieldsWithIndex

      pure $ H.div {}
        [ H.div { className: "row" }
          [ H.div { className: "btn btn-primary " <> (saveEnabled fieldsWithIndex fieldsS)
                  , on: { click: onClickSave {fields: fieldsS, nodeId, reload, session} }
                  }
            [ H.span { className: "fa fa-floppy-o" } [  ]
            ]
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

    saveEnabled :: FTFieldsWithIndex -> R.State FTFieldsWithIndex -> String
    saveEnabled fs (fsS /\ _) = if fs == fsS then "disabled" else "enabled"

    onClickSave :: forall e. { fields :: R.State FTFieldsWithIndex
                       , nodeId :: Int
                       , reload :: GUR.ReloadS
                       , session :: Session } -> e -> Effect Unit
    onClickSave {fields: (fieldsS /\ _), nodeId, reload, session} _ = do
      launchAff_ do
        saveCorpus $ { hyperdata: Hyperdata {fields: (\(Tuple _ f) -> f) <$> fieldsS}
                     , nodeId
                     , session }
        liftEffect $ GUR.bump reload

    onClickAdd :: forall e. R.State FTFieldsWithIndex -> e -> Effect Unit
    onClickAdd (_ /\ setFieldsS) _ = do
      setFieldsS $ \fieldsS -> List.snoc fieldsS $ Tuple (List.length fieldsS) defaultField

type FieldsCodeEditorProps =
  (
    fields :: R.State FTFieldsWithIndex
    | LoadProps
  )

fieldsCodeEditor :: R2.Component FieldsCodeEditorProps
fieldsCodeEditor = R.createElement fieldsCodeEditorCpt

fieldsCodeEditorCpt :: R.Component FieldsCodeEditorProps
fieldsCodeEditorCpt = here.component "fieldsCodeEditorCpt" cpt
  where
    cpt {nodeId, fields: fS@(fields /\ _), session} _ = do
      masterKey <- GUR.new

      pure $ H.div {} $ List.toUnfoldable (editors masterKey)
      where
        editors masterKey =
          (\(Tuple idx field) ->
            fieldCodeEditorWrapper { canMoveDown: idx < (List.length fields - 1)
                                   , canMoveUp: idx > 0
                                   , field
                                   , key: (show $ fst masterKey) <> "-" <> (show idx)
                                   , onChange: onChange fS idx
                                   , onMoveDown: onMoveDown masterKey fS idx
                                   , onMoveUp: onMoveUp masterKey fS idx
                                   , onRemove: onRemove fS idx
                                   , onRename: onRename fS idx
                                   }) <$> fields

    onChange :: R.State FTFieldsWithIndex -> Index -> FieldType -> Effect Unit
    onChange (_ /\ setFields) idx typ = do
      setFields $ \fields ->
        fromMaybe fields $
          List.modifyAt idx (\(Tuple _ (Field f)) -> Tuple idx (Field $ f { typ = typ })) fields

    onMoveDown :: GUR.ReloadS -> R.State FTFieldsWithIndex -> Index -> Unit -> Effect Unit
    onMoveDown masterKey (_ /\ setFields) idx _ = do
      GUR.bump masterKey
      setFields $ recomputeIndices <<< (GDA.swapList idx (idx + 1))

    onMoveUp :: GUR.ReloadS -> R.State FTFieldsWithIndex -> Index -> Unit -> Effect Unit
    onMoveUp masterKey (_ /\ setFields) idx _ = do
      GUR.bump masterKey
      setFields $ recomputeIndices <<< (GDA.swapList idx (idx - 1))

    onRemove :: R.State FTFieldsWithIndex -> Index -> Unit -> Effect Unit
    onRemove (_ /\ setFields) idx _ = do
      setFields $ \fields ->
        fromMaybe fields $ List.deleteAt idx fields

    onRename :: R.State FTFieldsWithIndex -> Index -> String -> Effect Unit
    onRename (_ /\ setFields) idx newName = do
      setFields $ \fields ->
        fromMaybe fields $ List.modifyAt idx (\(Tuple _ (Field f)) -> Tuple idx (Field $ f { name = newName })) fields

    recomputeIndices :: FTFieldsWithIndex -> FTFieldsWithIndex
    recomputeIndices = List.mapWithIndex $ \idx -> \(Tuple _ t) -> Tuple idx t

hash :: FTFieldWithIndex -> Hash
hash (Tuple idx f) = Crypto.hash $ "--idx--" <> (show idx) <> "--field--" <> (show f)

type FieldCodeEditorProps =
  (
    canMoveDown :: Boolean
  , canMoveUp :: Boolean
  , field :: FTField
  , key :: String
  , onChange :: FieldType -> Effect Unit
  , onMoveDown :: Unit -> Effect Unit
  , onMoveUp :: Unit -> Effect Unit
  , onRemove :: Unit -> Effect Unit
  , onRename :: String -> Effect Unit
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
                renameable {onRename, text: name}
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
      isEditing <- R.useState' false
      state <- R.useState' text
      textRef <- R.useRef text

      -- handle props change of text
      R.useEffect1' text $ do
        if R.readRef textRef == text then
          pure unit
        else do
          R.setRef textRef text
          snd state $ const text

      pure $ H.div { className: "renameable" } [
        renameableText { isEditing, onRename, state }
      ]

type RenameableTextProps =
  (
    isEditing :: R.State Boolean
  , onRename :: String -> Effect Unit
  , state :: R.State String
  )

renameableText :: Record RenameableTextProps -> R.Element
renameableText props = R.createElement renameableTextCpt props []

renameableTextCpt :: R.Component RenameableTextProps
renameableTextCpt = here.component "renameableTextCpt" cpt
  where
    cpt {isEditing: (false /\ setIsEditing), state: (text /\ _)} _ = do
      pure $ H.div { className: "input-group" }
        [ H.input { className: "form-control"
                  , defaultValue: text
                  , disabled: 1
                  , type: "text" }
        , H.div { className: "btn input-group-append"
                , on: { click: \_ -> setIsEditing $ const true } }
          [ H.span { className: "fa fa-pencil" } []
          ]
        ]
    cpt {isEditing: (true /\ setIsEditing), onRename, state: (text /\ setText)} _ = do
      pure $ H.div { className: "input-group" }
        [ inputWithEnter {
            autoFocus: false
          , className: "form-control text"
          , defaultValue: text
          , onEnter: submit
          , onValueChanged: setText <<< const
          , placeholder: ""
          , type: "text"
          }
        , H.div { className: "btn input-group-append"
                , on: { click: submit } }
          [ H.span { className: "fa fa-floppy-o" } []
          ]
        ]
      where
        submit _ = do
          setIsEditing $ const false
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

-- Just to make reloading effective
loadCorpusWithReload :: {reload :: GUR.Reload  | LoadProps} -> Aff (NodePoly Hyperdata)
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
    reload :: GUR.Reload
  | LoadProps
  )


-- Just to make reloading effective
loadCorpusWithChildAndReload :: Record LoadWithReloadProps -> Aff CorpusData
loadCorpusWithChildAndReload {nodeId, reload, session} = loadCorpusWithChild {nodeId, session}
