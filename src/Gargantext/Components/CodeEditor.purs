module Gargantext.Components.CodeEditor where

import DOM.Simple.Types (Element)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (either, Either(..))
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null, toMaybe)
import Data.String.Utils (endsWith)
import Effect (Effect)
import FFI.Simple ((.=))
import Reactix as R
import Reactix.DOM.HTML as H
import Text.Markdown.SlamDown.Parser (parseMd)
import Text.Markdown.SlamDown.Smolder as MD
import Text.Markdown.SlamDown.Syntax (SlamDownP)
import Text.Smolder.Renderer.String as Smolder
import Toestand as T

import Gargantext.Prelude
import Gargantext.Utils.HighlightJS as HLJS
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.CodeEditor"

type Code = String
type Html = String
type Error = String
type ElRef = R.Ref (Nullable Element)

data CodeType = Haskell | JSON | Markdown | Python

derive instance Generic CodeType _
instance Eq CodeType where
  eq = genericEq
instance Show CodeType where
  show = genericShow

data ViewType = Code | Preview | Both
derive instance Generic ViewType _
instance Eq ViewType where
  eq = genericEq
instance Show ViewType where
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
render Python  code = Right $ renderPython  $ codeNlFix Python code
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

previewPostProcess Python htmlEl = do
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

renderPython :: String -> String
renderPython s = s


codeEditor :: Record Props -> R.Element
codeEditor p = R.createElement codeEditorCpt p []

-- The code editor contains 3 components:
-- - a hidden textarea
-- - textarea code overlay
-- - html preview
-- The overlay is to provide seamless syntax highlighting on top of the textarea.
-- I took the idea from: https://github.com/satya164/react-simple-code-editor
codeEditorCpt :: R.Component Props
codeEditorCpt = here.component "codeEditor" cpt
  where
    cpt {code, defaultCodeType, onChange} _ = do
      controls <- initControls code defaultCodeType

      codeS' <- T.useLive T.unequal controls.codeS
      codeType' <- T.useLive T.unequal controls.codeType
      viewType' <- T.useLive T.unequal controls.viewType

      R.useEffect2' codeS' codeType' $ do
        setCodeOverlay controls.codeOverlayElRef codeType' codeS'
        renderHtml codeS' codeType' controls.htmlElRef controls.error

      pure $ H.div { className: "code-editor" }
        [ toolbar { controls, onChange }
        , H.div { className: "row no-gutters error" }
          [ errorComponent {error: controls.error} ]
        , H.div { className: "row no-gutters editor" }
          [ H.div { className: "code-area " <> (codeHidden viewType') }
            [ H.div { className: "code-container" }
              [ H.textarea { defaultValue: codeS'
                           , on: { change: onEditChange controls.codeS codeType' onChange }
                           , placeholder: "Type some code..."
                           , ref: controls.codeElRef } [ ]
              , H.pre  { className: (langClass codeType')
                         -- , contentEditable: "true"
                       , ref: controls.codeOverlayElRef
                       , rows: 30
                         --, on: { input: onEditChange (fst codeType) codeElRef htmlRef codeRef error }
                       } []
              ]
             ]
           , H.div { className: "v-divider " <> (dividerHidden viewType') } [ H.text " " ]
           , H.div { className: "html " <> (langClass codeType') <> (previewHidden viewType')
                   , ref: controls.htmlElRef
                   } []
           ]
        ]

    codeHidden :: ViewType -> String
    codeHidden Code = ""
    codeHidden Both = ""
    codeHidden _ = " d-none"

    dividerHidden :: ViewType -> String
    dividerHidden Both = ""
    dividerHidden _ = " d-none"

    langClass :: CodeType -> String
    langClass Haskell  = " language-haskell"
    langClass JSON     = " language-json"
    langClass Markdown = " language-md"
    langClass Python = " language-python"

    previewHidden :: ViewType -> String
    previewHidden Preview = ""
    previewHidden Both = ""
    previewHidden _ = " d-none"

    onEditChange :: forall e. T.Box Code -> CodeType -> OnChangeCodeType -> e -> Effect Unit
    onEditChange codeS codeType onChange e = do
      let code = R.unsafeEventValue e
      T.write_ code codeS
      onChange codeType code

setCodeOverlay :: ElRef -> CodeType -> Code -> Effect Unit
setCodeOverlay codeOverlayElRef codeType code = do
  let mCodeOverlayEl = toMaybe $ R.readRef codeOverlayElRef
  _ <- case mCodeOverlayEl of
    Nothing -> pure unit
    Just codeOverlayEl -> do
      _ <- pure $ (codeOverlayEl .= "innerText") $ codeNlFix codeType code
      HLJS.highlightBlock codeOverlayEl
      pure unit
  pure unit

renderHtml :: Code -> CodeType -> ElRef -> T.Box (Maybe Error) -> Effect Unit
renderHtml code codeType htmlElRef error =
  case (toMaybe $ R.readRef htmlElRef) of
    Nothing -> pure unit
    Just htmlEl -> do
      case render codeType code of
        Left err -> do
          T.write_ (Just err) error
        Right rendered -> do
          T.write_ Nothing error
          _ <- pure $ (htmlEl .= "innerHTML") rendered
          previewPostProcess codeType htmlEl
          pure unit

type OnChangeCodeType = CodeType -> Code -> Effect Unit

type ToolbarProps = (
    controls :: Record Controls
  , onChange :: OnChangeCodeType
  )

toolbar :: Record ToolbarProps -> R.Element
toolbar p = R.createElement toolbarCpt p []

toolbarCpt :: R.Component ToolbarProps
toolbarCpt = here.component "toolbar" cpt
  where
    cpt { controls: { codeS, codeType, viewType }
        , onChange } _ = do
      codeS' <- T.useLive T.unequal codeS
      codeType' <- T.useLive T.unequal codeType

      pure $
        H.div { className: "row no-gutters align-items-center mb-3 code-editor__toolbar" }
          [ H.div { className: "code-editor__toolbar__type" }
               [ codeTypeSelector {
                   codeType
                  -- Handle rerendering of preview when viewType changed
                 , onChange: \ct -> onChange ct codeS'
                 }
               ]
          , H.div {}
             [ viewTypeSelector {state: viewType} [] ]
          ]


type ErrorComponentProps =
  (
    error :: T.Box (Maybe Error)
  )

errorComponent :: Record ErrorComponentProps -> R.Element
errorComponent p = R.createElement errorComponentCpt p []

errorComponentCpt :: R.Component ErrorComponentProps
errorComponentCpt = here.component "errorComponent" cpt
  where
    cpt { error } _ = do
      error' <- T.useLive T.unequal error

      pure $ case error' of
        Nothing -> H.div {} []
        Just err -> H.div { className: "text-danger mb-3" } [ H.text err ]


type CodeTypeSelectorProps =
  (
    codeType :: T.Box CodeType
  , onChange :: CodeType -> Effect Unit
  )

codeTypeSelector :: Record CodeTypeSelectorProps -> R.Element
codeTypeSelector p = R.createElement codeTypeSelectorCpt p []

codeTypeSelectorCpt :: R.Component CodeTypeSelectorProps
codeTypeSelectorCpt = here.component "codeTypeSelector" cpt
  where
    cpt { codeType, onChange } _ = do
      codeType' <- T.useLive T.unequal codeType

      pure $ R2.select { className: "form-control"
                       , defaultValue: show codeType'
                       , on: { change: onSelectChange codeType onChange }
                       , style: { width: "150px" }
                       }
        (option <$> [JSON, Markdown, Haskell, Python])

    option :: CodeType -> R.Element
    option value = H.option { value: show value } [ H.text $ show value ]

    onSelectChange :: forall e. T.Box CodeType -> (CodeType -> Effect Unit) -> e -> Effect Unit
    onSelectChange codeType onChange e = do
      let ct = case value of
            "Haskell"  -> Haskell
            "JSON"     -> JSON
            "Markdown" -> Markdown
            "Python"   -> Python
            _          -> Markdown
      T.write_ ct codeType
      onChange ct
      where
        value = R.unsafeEventValue e


type ViewTypeSelectorProps =
  (
    state :: T.Box ViewType
  )

viewTypeSelector :: R2.Component ViewTypeSelectorProps
viewTypeSelector = R.createElement viewTypeSelectorCpt

viewTypeSelectorCpt :: R.Component ViewTypeSelectorProps
viewTypeSelectorCpt = here.component "viewTypeSelector" cpt
  where
    cpt { state } _ = do
      state' <- T.useLive T.unequal state

      pure $ H.div { className: "btn-group"
                   , role: "group" } [
          viewTypeButton Code state' state
        , viewTypeButton Both state' state
        , viewTypeButton Preview state' state
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

    icon Preview = "fa-eye"
    icon Both = "fa-columns"
    icon Code = "fa-pencil"

type Controls =
  (
      codeElRef :: R.Ref (Nullable Element)
    , codeS :: T.Box Code
    , codeType :: T.Box CodeType
    , codeOverlayElRef :: R.Ref (Nullable Element)
    , error :: T.Box (Maybe Error)
    , htmlElRef :: R.Ref (Nullable Element)
    , viewType :: T.Box ViewType
  )

initControls :: Code -> CodeType -> R.Hooks (Record Controls)
initControls code defaultCodeType = do
  htmlElRef <- R.useRef null
  codeS <- T.useBox code
  codeElRef <- R.useRef null
  codeOverlayElRef <- R.useRef null
  codeType <- T.useBox defaultCodeType
  error <- T.useBox Nothing
  viewType <- T.useBox Preview

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
reinitControls c@{ codeType, codeS, error } code defaultCodeType = do
  T.write_ defaultCodeType codeType
  T.write_ code codeS
  T.write_ Nothing error
