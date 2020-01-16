module Gargantext.Components.MarkdownEditor where

import Data.Either (either, Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null, toMaybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
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

type Props =
  ( md :: String
  , nodeId :: Int
  )

compileMd' :: forall e. MD.ToMarkupOptions e -> String -> String
compileMd' options input =
  either identity (MD.toMarkup' options >>> render)
  (parseMd input :: Either String (SlamDownP String))

compileMd :: String -> String
compileMd = compileMd' MD.defaultToMarkupOptions

markdownEditor :: Record Props -> R.Element
markdownEditor p = R.createElement markdownEditorCpt p []

markdownEditorCpt :: R.Component Props
markdownEditorCpt = R.hooksComponent "G.C.MarkdownEditor" cpt
  where
    cpt {md, nodeId} _ = do
      ref <- R.useRef null
      editedMd <- R.useState' md

      -- Note: delay is necessary here, otherwise initially the HTML won't get
      -- rendered (mDiv is still null)
      R.useEffect $ delay unit $ \_ -> do
        let mDiv = toMaybe $ R.readRef ref
        case mDiv of
          Nothing -> pure $ pure unit
          Just d -> do
            _ <- pure $ (d .= "innerHTML") $ compileMd $ fst editedMd
            pure $ pure unit

      pure $ H.div { className: "markdown-editor" } [
        H.div { className: "md" } [
          H.textarea { className: "form-control"
                     , rows: 30
                     , on: {change: onChange ref editedMd} } [ H.text $ fst editedMd ]
        ]
      , H.div { ref, className: "html" } []
      ]

    onChange :: forall e. R.Ref (Nullable Element) -> R.State String -> e -> Effect Unit
    onChange ref (_ /\ setEditedMd) e = do
      setEditedMd $ const value
      where
        value = R2.unsafeEventValue e
