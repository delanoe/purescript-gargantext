module Main where

import Prelude

import Effect (Effect)
import Data.Maybe (fromJust)

import Gargantext.Pages.Layout        (dispatchAction)
import Gargantext.Pages.Layout.Specs  (layoutSpec)
import Gargantext.Pages.Layout.States (initAppState)

import Gargantext.Router (routeHandler, routing)
import Partial.Unsafe (unsafePartial)
import React as R
import ReactDOM as RDOM
import Routing (matches)
import Routing.Hash (getHash, setHash)
import Thermite as T

main :: Effect Unit
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
