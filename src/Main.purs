module Main where

import Prelude

import Data.Maybe (fromJust)
import Effect (Effect)
import Gargantext.Pages.Layout (dispatchAction)
import Gargantext.Pages.Layout.Specs (layoutSpec)
import Gargantext.Pages.Layout.States (initAppState)
import Gargantext.Router (routeHandler, routing)
import Partial.Unsafe (unsafePartial)
import React (ReactThis)
import React as R
import ReactDOM as RDOM
import Routing.Hash (getHash, matches, setHash)
import Thermite (createClass)
import Thermite as T
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
 case T.createReactSpec layoutSpec initAppState of
    { spec, dispatcher } -> void $ do
      let setRouting this = void $ do
            matches routing (routeHandler (dispatchAction (dispatcher this)))
          spec' = spec { componentWillMount = setRouting }
      document <- window >>= document
      container <- unsafePartial (fromJust  <$> querySelector (QuerySelector "#app") (document))
      h <- getHash
      case h of
        "" -> setHash "/"
        _ -> do
          setHash "/"
          setHash h
      RDOM.render (R.unsafeCreateElement (createClass spec') {}) container
