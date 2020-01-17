module Gargantext.Components.CodeEditor where

import Data.Either (either, Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null, toMaybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log, log2)
import DOM.Simple.Types (Element)
import Effect (Effect)
import FFI.Simple ((.=), delay)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Reactix as R
import Reactix.DOM.HTML as H
import Text.Markdown.SlamDown.Parser (parseMd)
import Text.Markdown.SlamDown.Smolder as MD
import Text.Markdown.SlamDown.Syntax (SlamDownP(..))
import Text.Smolder.Renderer.String (render)

import Gargantext.Prelude
import Gargantext.Utils.Reactix as R2

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
  , codeType :: CodeType
  , onChange :: String -> Effect Unit
  )

compile :: CodeType -> String -> String
compile JSON code = code
compile Markdown code = compileMd code

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
    cpt {code, codeType, onChange} _ = do
      htmlRef <- R.useRef null
      codeRef <- R.useRef null
      editorCodeRef <- R.useRef code
      codeTypeS <- R.useState' codeType
      viewType <- R.useState' Both

      -- Note: delay is necessary here, otherwise initially the HTML won't get
      -- rendered (mDiv is still null)
      R.useEffect $ delay unit $ \_ -> do
        let mHtmlEl = toMaybe $ R.readRef htmlRef
        case mHtmlEl of
          Nothing -> pure $ pure unit
          Just htmlEl -> do
            _ <- pure $ (htmlEl .= "innerHTML") $ compile codeType code
            pure $ pure unit

      R.useEffectOnce $ delay unit $ \_ -> do
        let mCodeEl = toMaybe $ R.readRef codeRef
        case mCodeEl of
          Nothing -> pure $ pure unit
          Just codeEl -> do
            _ <- pure $ (codeEl .= "innerText") code
            pure $ pure unit

      pure $ H.div { className: "code-editor" } [
        H.div { className: "row toolbar" } [
           codeTypeSelector {codeType: codeTypeS}
           , H.div { className: "btn-group" } [
               viewTypeButton {viewType: Code, state: viewType}
             , viewTypeButton {viewType: Both, state: viewType}
             , viewTypeButton {viewType: Preview, state: viewType}
             ]
           ]
        , H.div { className: "row editor" } [
           H.div { className: "code " <> (codeHidden $ fst viewType) } [
              H.code { className: ""
                     , contentEditable: "true"
                     , ref: codeRef
                     , rows: 30
                     , on: { input: onEditChange codeType codeRef htmlRef editorCodeRef }
                     } []
              ]
           , H.div { ref: htmlRef, className: "html " <> (previewHidden $ fst viewType) } []
           ]
        ]

    codeHidden :: ViewType -> String
    codeHidden Code = ""
    codeHidden Both = ""
    codeHidden _ = "hidden"

    previewHidden :: ViewType -> String
    previewHidden Preview = ""
    previewHidden Both = ""
    previewHidden _ = "hidden"

    onEditChange :: forall e. CodeType -> R.Ref (Nullable Element) -> R.Ref (Nullable Element) -> R.Ref String -> e -> Effect Unit
    onEditChange codeType codeRef htmlRef editorCodeRef e = do
      log2 "[onChange] e" e
      let mCode = toMaybe $ R.readRef codeRef
      case mCode of
        Nothing -> log "[onChange] mCode = Nothing"
        Just code -> do
          R.setRef editorCodeRef $ R2.innerText code
          pure unit
      let mHtml = toMaybe $ R.readRef htmlRef
      case mHtml of
        Nothing -> pure unit
        Just html -> do
          _ <- pure $ (html .= "innerHTML") $ compile codeType $ R.readRef editorCodeRef
          pure unit
      pure unit


type CodeTypeSelectorProps =
  (
    codeType :: R.State CodeType
  )

codeTypeSelector :: Record CodeTypeSelectorProps -> R.Element
codeTypeSelector p = R.createElement codeTypeSelectorCpt p []

codeTypeSelectorCpt :: R.Component CodeTypeSelectorProps
codeTypeSelectorCpt = R.hooksComponent "G.C.CodeTypeSelector" cpt
  where
    cpt {codeType} _ = do
      pure $ R2.select { className: "form-control"
                , on: { change: onSelectChange codeType }
                , style: { width: "150px" }
                , value: show $ fst codeType }
        (option <$> [JSON, Markdown])

    option :: CodeType -> R.Element
    option value = H.option { value: show value } [ H.text $ show value ]
    onSelectChange (_ /\ setCodeType) e = do
      let codeType = case value of
            "JSON"     -> JSON
            "Markdown" -> Markdown
            _          -> Markdown
      setCodeType $ const codeType
      where
        value = R2.unsafeEventValue e


type ViewTypeProps =
  (
    viewType :: ViewType
  , state :: R.State ViewType
  )

viewTypeButton :: Record ViewTypeProps -> R.Element
viewTypeButton p = R.createElement viewTypeButtonCpt p []

viewTypeButtonCpt :: R.Component ViewTypeProps
viewTypeButtonCpt = R.hooksComponent "G.C.ViewTypeButton" cpt
  where
    cpt {viewType, state: (state /\ setState)} _ =
      pure $ H.label {
                className: "btn btn-default" <> (active viewType state)
              , on: { click: onClick viewType setState }
              } [
                H.i { className: "glyphicon " <> (icon viewType) } []
              ]

    active viewType state = if viewType == state then " active" else ""

    icon Preview = "glyphicon-eye-open"
    icon Both = "glyphicon-transfer"
    icon Code = "glyphicon-pencil"

    onClick viewType setState _ = setState $ const viewType
