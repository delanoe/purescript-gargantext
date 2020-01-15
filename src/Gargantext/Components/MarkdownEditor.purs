module Gargantext.Components.MarkdownEditor where

import Data.Either (either, Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null, toMaybe)
import FFI.Simple ((.=))
import Prelude (($), (>>>), bind, discard, identity, pure, unit)
import Reactix as R
import Reactix.DOM.HTML as H
import Text.Markdown.SlamDown.Parser (parseMd)
import Text.Markdown.SlamDown.Smolder as MD
import Text.Markdown.SlamDown.Syntax (SlamDownP(..))
import Text.Smolder.Renderer.String (render)

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

      R.useEffect1' md $ do
        let mDiv = toMaybe $ R.readRef ref
        case mDiv of
          Nothing -> pure unit
          Just d -> do
            _ <- pure $ ("innerHTML" .= d) $ compileMd md
            pure unit

      pure $ H.div { ref } []
