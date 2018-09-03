module Main where

import Prelude

import Data.Maybe (fromJust)
import Effect (Effect)
import Gargantext.Pages.Layout (dispatchAction)
import Gargantext.Pages.Layout.Specs (layoutSpec)
import Gargantext.Pages.Layout.States (initAppState)
import Gargantext.Router (routeHandler, routing)
import Partial.Unsafe (unsafePartial)
import React as R
import ReactDOM as RDOM
import Record.Unsafe (unsafeSet)
import Routing.Hash (getHash, matches, setHash)
import Thermite as T
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toParentNode)

setComponentWillMount :: forall s. Effect Unit -> Record s -> Record (componentWillMount :: Effect Unit | s)
setComponentWillMount = unsafeSet "componentWillMount"

main :: Effect Unit
main = do
 case T.createReactSpec layoutSpec initAppState of
    { spec, dispatcher } -> void $ do
      let setRouting this = void $ do
            matches routing (routeHandler (dispatchAction (dispatcher this)))
          spec' this = setComponentWillMount (setRouting this) <$> (spec this)
      document <- window >>= document
      container <- unsafePartial (fromJust  <$> querySelector (QuerySelector "#app") (toParentNode document))
      h <- getHash
      case h of
        "" -> setHash "/"
        _ -> do
          setHash "/"
          setHash h
      let e = R.unsafeCreateElement (R.component "GargantextMain" spec) {} []
      RDOM.render e container
