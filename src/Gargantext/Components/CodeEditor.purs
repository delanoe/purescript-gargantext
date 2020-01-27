module Gargantext.Components.CodeEditor where

import Data.Argonaut.Parser (jsonParser)
import Data.Either (either, Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null, toMaybe)
import Data.String.Utils (endsWith)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Types (Element)
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H
import Text.Markdown.SlamDown.Parser (parseMd)
import Text.Markdown.SlamDown.Smolder as MD
import Text.Markdown.SlamDown.Syntax (SlamDownP)
import Text.Smolder.Renderer.String as Smolder

import Gargantext.Prelude
import Gargantext.Utils.HighlightJS as HLJS
import Gargantext.Utils.Reactix as R2

type Code = String
type Html = String
type Error = String

data CodeType = Haskell | JSON | Markdown
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
  ( code :: Code
  , defaultCodeType :: CodeType
  , onChange :: CodeType -> Code -> Effect Unit
  )

-- Fixes newlines in code
-- This is useful eg for proper rendering of the textarea overlay
codeNlFix :: CodeType -> Code -> Code
codeNlFix _ "" = " "
codeNlFix _ c = if endsWith "\n" c then (c <> " ") else c

render :: CodeType -> Code -> Either Error Html
render Haskell code = Right $ renderHaskell $ codeNlFix Haskell code
render JSON code = result
  where
    parsedE = jsonParser code
    result = case parsedE of
      Left err -> Left err
      Right parsed -> Right $ R2.stringify parsed 2
render Markdown code = Right $ renderMd $ codeNlFix Markdown code

previewPostProcess :: CodeType -> Element -> Effect Unit
previewPostProcess Haskell htmlEl = do
  HLJS.highlightBlock htmlEl
previewPostProcess JSON htmlEl = do
  HLJS.highlightBlock htmlEl
previewPostProcess Markdown _ = pure unit

-- TODO Replace with markdown-it?
-- https://pursuit.purescript.org/packages/purescript-markdown-it
renderMd' :: forall e. MD.ToMarkupOptions e -> String -> String
renderMd' options input =
  either identity (MD.toMarkup' options >>> Smolder.render)
  (parseMd input :: Either String (SlamDownP String))

renderMd :: String -> String
renderMd = renderMd' MD.defaultToMarkupOptions

renderHaskell :: String -> String
renderHaskell s = s

codeEditor :: Record Props -> R.Element
codeEditor p = R.createElement codeEditorCpt p []

-- The code editor contains 3 components:
-- - a hidden textarea
-- - textarea code overlay
-- - html preview
-- The overlay is to provide seamless syntax highlighting on top of the textarea.
-- I took the idea from: https://github.com/satya164/react-simple-code-editor
codeEditorCpt :: R.Component Props
codeEditorCpt = R.hooksComponent "G.C.CE.CodeEditor" cpt
  where
    cpt {code, defaultCodeType, onChange} _ = do
      controls <- initControls code defaultCodeType

      R.useEffect2' (fst controls.codeS) (fst controls.codeType) $ do
        let code' = fst controls.codeS
        setCodeOverlay controls code'
        renderHtml code' controls

      pure $ H.div { className: "code-editor" } [
          toolbar {controls, onChange}
        , H.div { className: "row error" } [
           errorComponent {error: controls.error}
        ]
        , H.div { className: "row editor" } [
           H.div { className: "code-area " <> (codeHidden $ fst controls.viewType) } [
             H.div { className: "code-container" } [
               H.textarea { defaultValue: code
                          , on: { change: onEditChange controls onChange }
                          , placeholder: "Type some code..."
                          , ref: controls.codeElRef } [ ]
               , H.pre  { className: (langClass $ fst controls.codeType)
                          -- , contentEditable: "true"
                        , ref: controls.codeOverlayElRef
                        , rows: 30
                          --, on: { input: onEditChange (fst codeType) codeElRef htmlRef codeRef error }
                        } []
               ]
             ]
           , H.div { className: "v-divider " <> (dividerHidden $ fst controls.viewType) } [ H.text " " ]
           , H.div { className: "html " <> (langClass $ fst controls.codeType) <> (previewHidden $ fst controls.viewType)
                   , ref: controls.htmlElRef
                   } []
           ]
        ]

    codeHidden :: ViewType -> String
    codeHidden Code = ""
    codeHidden Both = ""
    codeHidden _ = " hidden"

    dividerHidden :: ViewType -> String
    dividerHidden Both = ""
    dividerHidden _ = " hidden"

    langClass :: CodeType -> String
    langClass Haskell = " language-haskell"
    langClass JSON = " language-json"
    langClass Markdown = " language-md"

    previewHidden :: ViewType -> String
    previewHidden Preview = ""
    previewHidden Both = ""
    previewHidden _ = " hidden"

    onEditChange :: forall e. Record Controls -> (CodeType -> Code -> Effect Unit) -> e -> Effect Unit
    onEditChange controls@{codeElRef, codeOverlayElRef, codeType: (codeType /\ _), codeS} onChange e = do
      let code = R2.unsafeEventValue e
      snd codeS $ const code
      onChange codeType code

setCodeOverlay :: Record Controls -> Code -> Effect Unit
setCodeOverlay {codeOverlayElRef, codeType: (codeType /\ _)} code = do
  let mCodeOverlayEl = toMaybe $ R.readRef codeOverlayElRef
  _ <- case mCodeOverlayEl of
    Nothing -> pure unit
    Just codeOverlayEl -> do
      _ <- pure $ (codeOverlayEl .= "innerText") $ codeNlFix codeType code
      HLJS.highlightBlock codeOverlayEl
      pure unit
  pure unit

renderHtml :: Code -> Record Controls -> Effect Unit
renderHtml code {codeType: (codeType /\ _), htmlElRef, error: (_ /\ setError)} =
  case (toMaybe $ R.readRef htmlElRef) of
    Nothing -> pure unit
    Just htmlEl -> do
      case render codeType code of
        Left err -> do
          setError $ const $ Just err
        Right rendered -> do
          setError $ const Nothing
          _ <- pure $ (htmlEl .= "innerHTML") rendered
          previewPostProcess codeType htmlEl
          pure unit

type ToolbarProps = (
    controls :: Record Controls
  , onChange :: CodeType -> Code -> Effect Unit
  )

toolbar :: Record ToolbarProps -> R.Element
toolbar p = R.createElement toolbarCpt p []

toolbarCpt :: R.Component ToolbarProps
toolbarCpt = R.hooksComponent "G.C.CE.toolbar" cpt
  where
    cpt props@{controls: {codeType, error, viewType}} _ = do
      pure $
        H.div { className: "row toolbar" } [
             codeTypeSelector {
                  codeType
                , onChange: onChangeCodeType props
                }
           , viewTypeSelector {state: viewType}
           ]

    -- Handle rerendering of preview when viewType changed
    onChangeCodeType :: forall e. Record ToolbarProps -> e -> Effect Unit
    onChangeCodeType {controls, onChange} _ = do
      onChange (fst controls.codeType) code
      where
        code = fst controls.codeS


type ErrorComponentProps =
  (
    error :: R.State (Maybe Error)
  )

errorComponent :: Record ErrorComponentProps -> R.Element
errorComponent p = R.createElement errorComponentCpt p []

errorComponentCpt :: R.Component ErrorComponentProps
errorComponentCpt = R.hooksComponent "G.C.CE.ErrorComponent" cpt
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
codeTypeSelectorCpt = R.hooksComponent "G.C.CE.CodeTypeSelector" cpt
  where
    cpt {codeType, onChange} _ = do
      pure $ R2.select { className: "form-control"
                , on: { change: onSelectChange codeType onChange }
                , style: { width: "150px" }
                , value: show $ fst codeType }
        (option <$> [Haskell, JSON, Markdown])

    option :: CodeType -> R.Element
    option value = H.option { value: show value } [ H.text $ show value ]

    onSelectChange :: forall e. R.State CodeType -> (CodeType -> Effect Unit) -> e -> Effect Unit
    onSelectChange (_ /\ setCodeType) onChange e = do
      let codeType = case value of
            "Haskell"  -> Haskell
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
viewTypeSelectorCpt = R.hooksComponent "G.C.CE.ViewTypeSelector" cpt
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
    , codeS :: R.State Code
    , codeType :: R.State CodeType
    , codeOverlayElRef :: R.Ref (Nullable Element)
    , error :: R.State (Maybe Error)
    , htmlElRef :: R.Ref (Nullable Element)
    , viewType :: R.State ViewType
  )

initControls :: Code -> CodeType -> R.Hooks (Record Controls)
initControls code defaultCodeType = do
  htmlElRef <- R.useRef null
  codeS <- R.useState' code
  codeElRef <- R.useRef null
  codeOverlayElRef <- R.useRef null
  codeType <- R.useState' defaultCodeType
  error <- R.useState' Nothing
  viewType <- R.useState' Both

  pure $ {
      codeElRef
    , codeS
    , codeType
    , codeOverlayElRef
    , error
    , htmlElRef
    , viewType
    }

reinitControls :: Record Controls -> Code -> CodeType -> Effect Unit
reinitControls c@{codeType, codeS, error} code defaultCodeType = do
  snd codeType $ const defaultCodeType
  snd codeS $ const code
  snd error $ const Nothing
