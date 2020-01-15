module Gargantext.Components.MarkdownEditor where

import Data.Either (either, Either(..))
import Prelude (($), (>>>), identity, pure)
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
    cpt {md, nodeId} _ =
      pure $
      H.div {} [ H.text $ compileMd md ]
