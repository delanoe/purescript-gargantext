module Gargantext.Components.CodeEditor where

import Data.Argonaut.Parser (jsonParser)
import Data.Either (either, Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null, toMaybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log, log2)
import DOM.Simple.Types (Element)
import Effect (Effect)
import FFI.Simple ((.=), delay)
import Reactix as R
import Reactix.DOM.HTML as H
import Text.Markdown.SlamDown.Parser (parseMd)
import Text.Markdown.SlamDown.Smolder as MD
import Text.Markdown.SlamDown.Syntax (SlamDownP)
import Text.Smolder.Renderer.String (render)

import Gargantext.Prelude
import Gargantext.Utils.HighlightJS as HLJS
import Gargantext.Utils.Reactix as R2

type Code = String
type Html = String
type Error = String

data CodeType = JSON | Markdown
derive instance genericCodeType :: Generic CodeType _
instance eqCodeType :: Eq CodeType where
  eq = genericEq
instance showCodeType :: Show CodeType where
  show = genericShow

data ViewType = Code | Preview | Both
derive instance genericViewType :: Generic ViewType _
instance eqViewType :: Eq ViewType where
  eq = genericEq
instance showViewType :: Show ViewType where
  show = genericShow

type Props =
  ( code :: String
  , defaultCodeType :: CodeType
  , onChange :: String -> Effect Unit
  )

compile :: CodeType -> Code -> Either Error Html
compile JSON code = result
  where
    parsedE = jsonParser code
    result = case parsedE of
      Left err -> Left err
      Right parsed -> Right $ "<pre>" <> (R2.stringify parsed 2) <> "</pre>"
compile Markdown code = Right $ compileMd code

-- TODO Replace with markdown-it?
-- https://pursuit.purescript.org/packages/purescript-markdown-it
compileMd' :: forall e. MD.ToMarkupOptions e -> String -> String
compileMd' options input =
  either identity (MD.toMarkup' options >>> render)
  (parseMd input :: Either String (SlamDownP String))

compileMd :: String -> String
compileMd = compileMd' MD.defaultToMarkupOptions

codeEditor :: Record Props -> R.Element
codeEditor p = R.createElement codeEditorCpt p []

codeEditorCpt :: R.Component Props
codeEditorCpt = R.hooksComponent "G.C.CodeEditor" cpt
  where
    cpt {code, defaultCodeType, onChange} _ = do
      controls <- initControls code defaultCodeType

      -- Initial rendering of elements with given data

      -- Note: delay is necessary here, otherwise initially the HTML won't get
      -- rendered (mDiv is still null)
      R.useEffectOnce $ delay unit $ \_ -> do
        _ <- renderHtml code controls
        pure $ pure unit

      R.useEffectOnce $ delay unit $ \_ -> do
        let mCodeOverlayEl = toMaybe $ R.readRef controls.codeOverlayElRef
        case mCodeOverlayEl of
          Nothing -> pure $ pure unit
          Just codeOverlayEl -> do
            _ <- pure $ (codeOverlayEl .= "innerText") code
            HLJS.highlightBlock codeOverlayEl
            pure $ pure unit

      pure $ H.div { className: "code-editor" } [
        H.div { className: "row toolbar" } [
             codeTypeSelector {
                  codeType: controls.codeType
                , onChange: onChangeCodeType controls
                }
           , viewTypeSelector {state: controls.viewType}
           ]
        , H.div { className: "row error" } [
           errorComponent {error: controls.error}
        ]
        , H.div { className: "row editor" } [
           H.div { className: "code " <> (codeHidden $ fst controls.viewType) } [
              H.textarea { defaultValue: code
                         , on: { change: onEditChange controls }
                         , ref: controls.codeElRef } [ ]
            , H.code { className: ""
                     -- , contentEditable: "true"
                     , ref: controls.codeOverlayElRef
                     , rows: 30
                     --, on: { input: onEditChange (fst codeType) codeElRef htmlRef editorCodeRef error }
                     } []
              ]
           , H.div { className: "v-divider " <> (dividerHidden $ fst controls.viewType) } [ H.text " " ]
           , H.div { className: "html " <> (previewHidden $ fst controls.viewType)
                   , ref: controls.htmlElRef
                   } []
           ]
        ]

    codeHidden :: ViewType -> String
    codeHidden Code = ""
    codeHidden Both = ""
    codeHidden _ = "hidden"

    dividerHidden :: ViewType -> String
    dividerHidden Both = ""
    dividerHidden _ = "hidden"

    previewHidden :: ViewType -> String
    previewHidden Preview = ""
    previewHidden Both = ""
    previewHidden _ = "hidden"

    -- Handle rerendering of preview when viewType changed
    onChangeCodeType :: forall e. Record Controls -> e -> Effect Unit
    onChangeCodeType controls _ = do
        _ <- renderHtml (R.readRef controls.editorCodeRef) controls
        pure unit

    onEditChange :: forall e. Record Controls -> e -> Effect Unit
    onEditChange controls@{codeElRef, codeOverlayElRef, editorCodeRef} e = do
      log2 "[onChange] e" e
      let mCodeOverlayEl = toMaybe $ R.readRef codeOverlayElRef
      case mCodeOverlayEl of
        Nothing -> log "[onChange] mCodeOverlayEl = Nothing"
        Just codeOverlayEl -> do
          --R.setRef editorCodeRef $ R2.innerText codeOverlayEl
          let code = R2.unsafeEventValue e
          R.setRef editorCodeRef code
          _ <- case (toMaybe $ R.readRef codeElRef) of
            Nothing -> pure unit
            Just codeEl -> do
              _ <- pure $ (codeOverlayEl .= "innerText") code
              HLJS.highlightBlock codeOverlayEl
              pure unit
          pure unit
      renderHtml (R.readRef controls.editorCodeRef) controls

renderHtml :: Code -> Record Controls -> Effect Unit
renderHtml code {codeType: (codeType /\ _), htmlElRef, error: (_ /\ setError)} =
  case (toMaybe $ R.readRef htmlElRef) of
    Nothing -> pure unit
    Just htmlEl -> do
      case compile codeType code of
        Left err -> do
          setError $ const $ Just err
        Right compiled -> do
          setError $ const Nothing
          _ <- pure $ (htmlEl .= "innerHTML") compiled
          pure unit


type ErrorComponentProps =
  (
    error :: R.State (Maybe Error)
  )

errorComponent :: Record ErrorComponentProps -> R.Element
errorComponent p = R.createElement errorComponentCpt p []

errorComponentCpt :: R.Component ErrorComponentProps
errorComponentCpt = R.hooksComponent "G.C.ErrorComponent" cpt
  where
    cpt {error: (Nothing /\ _)} _ = pure $ H.div {} []
    cpt {error: ((Just error) /\ _)} _ = do
      pure $ H.div { className: "text-danger" } [ H.text error ]


type CodeTypeSelectorProps =
  (
    codeType :: R.State CodeType
  , onChange :: CodeType -> Effect Unit
  )

codeTypeSelector :: Record CodeTypeSelectorProps -> R.Element
codeTypeSelector p = R.createElement codeTypeSelectorCpt p []

codeTypeSelectorCpt :: R.Component CodeTypeSelectorProps
codeTypeSelectorCpt = R.hooksComponent "G.C.CodeTypeSelector" cpt
  where
    cpt {codeType, onChange} _ = do
      pure $ R2.select { className: "form-control"
                , on: { change: onSelectChange codeType onChange }
                , style: { width: "150px" }
                , value: show $ fst codeType }
        (option <$> [JSON, Markdown])

    option :: CodeType -> R.Element
    option value = H.option { value: show value } [ H.text $ show value ]

    onSelectChange :: forall e. R.State CodeType -> (CodeType -> Effect Unit) -> e -> Effect Unit
    onSelectChange (_ /\ setCodeType) onChange e = do
      let codeType = case value of
            "JSON"     -> JSON
            "Markdown" -> Markdown
            _          -> Markdown
      setCodeType $ const codeType
      onChange codeType
      where
        value = R2.unsafeEventValue e


type ViewTypeSelectorProps =
  (
    state :: R.State ViewType
  )

viewTypeSelector :: Record ViewTypeSelectorProps -> R.Element
viewTypeSelector p = R.createElement viewTypeSelectorCpt p []

viewTypeSelectorCpt :: R.Component ViewTypeSelectorProps
viewTypeSelectorCpt = R.hooksComponent "G.C.ViewTypeSelector" cpt
  where
    cpt {state} _ =
      pure $ H.div { className: "btn-group" } [
          viewTypeButton Code state
        , viewTypeButton Both state
        , viewTypeButton Preview state
        ]

    viewTypeButton viewType (state /\ setState) =
      H.label {
        className: "btn btn-default" <> active
        , on: { click: onClick }
      } [
        H.i { className: "glyphicon " <> (icon viewType) } []
      ]
      where
        active = if viewType == state then " active" else ""

        onClick _ = do
          setState $ const viewType

    icon Preview = "glyphicon-eye-open"
    icon Both = "glyphicon-transfer"
    icon Code = "glyphicon-pencil"

type Controls =
  (
      codeElRef :: R.Ref (Nullable Element)
    , codeType :: R.State CodeType
    , codeOverlayElRef :: R.Ref (Nullable Element)
    , editorCodeRef :: R.Ref Code
    , error :: R.State (Maybe Error)
    , htmlElRef :: R.Ref (Nullable Element)
    , viewType :: R.State ViewType
  )

initControls :: Code -> CodeType -> R.Hooks (Record Controls)
initControls code defaultCodeType = do
  htmlElRef <- R.useRef null
  codeElRef <- R.useRef null
  codeOverlayElRef <- R.useRef null
  codeType <- R.useState' defaultCodeType
  editorCodeRef <- R.useRef code
  error <- R.useState' Nothing
  viewType <- R.useState' Both

  pure $ {
      codeElRef
    , codeType
    , codeOverlayElRef
    , editorCodeRef
    , error
    , htmlElRef
    , viewType
    }
