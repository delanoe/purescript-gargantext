module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (QuerySelector(..))
import DOM.Node.ParentNode (querySelector) as DOM
import Data.Maybe (fromJust)

import Gargantext.Pages.Layout.Specs (layoutSpec)
import Gargantext.Layout.Dispatcher   (dispatchAction)
import Gargantext.Pages.Layout.States (initAppState)

import Gargantext.Router (routeHandler, routing)
import Network.HTTP.Affjax (AJAX)
import Partial.Unsafe (unsafePartial)
import React as R
import ReactDOM as RDOM
import Routing (matches)
import Routing.Hash (getHash, setHash)
import Thermite as T

main :: forall e. Eff (dom:: DOM, console :: CONSOLE, ajax :: AJAX | e ) Unit
main = do
 case T.createReactSpec layoutSpec initAppState of
    { spec, dispatcher } -> void $ do
      let setRouting this = void $ do
            matches routing (routeHandler (dispatchAction (dispatcher this)))
          spec' = spec { componentWillMount = setRouting }
      document <- DOM.window >>= DOM.document
      container <- unsafePartial (fromJust  <$> DOM.querySelector (QuerySelector "#app") (DOM.htmlDocumentToParentNode document))
      h <- getHash
      case h of
        "" -> setHash "/"
        _ -> do
          setHash "/"
          setHash h
      RDOM.render (R.createFactory (R.createClass spec') {}) container
