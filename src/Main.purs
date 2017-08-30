module Main where

import Login (initialState, loginSpec)
import Prelude

import Control.Monad.Eff.Unsafe (unsafePerformEff)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (QuerySelector(..))
import DOM.Node.ParentNode (querySelector) as DOM
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import React as R
import ReactDOM as RDOM
import Thermite as T

main :: Unit
main = unsafePerformEff $ do
  case T.createReactSpec loginSpec initialState  of
    { spec, dispatcher } -> void $ do
      let spec' = spec
      document <- DOM.window >>= DOM.document
      container <- unsafePartial (fromJust <$> DOM.querySelector (QuerySelector "#app") (DOM.htmlDocumentToParentNode document))
      RDOM.render (R.createFactory (R.createClass spec') {}) container
