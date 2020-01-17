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
import Prelude (($), (>>>), Unit, bind, const, discard, identity, pure, unit)
import Reactix as R
import Reactix.DOM.HTML as H
import Text.Markdown.SlamDown.Parser (parseMd)
import Text.Markdown.SlamDown.Smolder as MD
import Text.Markdown.SlamDown.Syntax (SlamDownP(..))
import Text.Smolder.Renderer.String (render)

import Gargantext.Utils.Reactix as R2

data CodeType = Markdown

type Props =
  ( code :: String
  , codeType :: CodeType
  , onChange :: String -> Effect Unit
  )

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

      -- Note: delay is necessary here, otherwise initially the HTML won't get
      -- rendered (mDiv is still null)
      R.useEffect $ delay unit $ \_ -> do
        let mHtmlEl = toMaybe $ R.readRef htmlRef
        case mHtmlEl of
          Nothing -> pure $ pure unit
          Just htmlEl -> do
            _ <- pure $ (htmlEl .= "innerHTML") $ compileMd code
            pure $ pure unit

      R.useEffectOnce $ delay unit $ \_ -> do
        let mCodeEl = toMaybe $ R.readRef codeRef
        case mCodeEl of
          Nothing -> pure $ pure unit
          Just codeEl -> do
            _ <- pure $ (codeEl .= "innerText") code
            pure $ pure unit

      pure $ H.div { className: "code-editor" } [
        H.div { className: "code" } [
          H.code { className: ""
                 , contentEditable: "true"
                 , ref: codeRef
                 , rows: 30
                 , on: { change: onEditChange codeRef htmlRef editorCodeRef
                       , input: onEditChange codeRef htmlRef editorCodeRef }
                 } []
        ]
      , H.div { ref: htmlRef, className: "html" } []
      ]

    onEditChange :: forall e. R.Ref (Nullable Element) -> R.Ref (Nullable Element) -> R.Ref String -> e -> Effect Unit
    onEditChange codeRef htmlRef editorCodeRef e = do
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
          _ <- pure $ (html .= "innerHTML") $ compileMd $ R.readRef editorCodeRef
          pure unit
      pure unit
