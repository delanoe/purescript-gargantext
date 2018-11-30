module Main where

import Prelude

import Data.Maybe (fromJust)
import Effect (Effect)
import Gargantext.Pages.Layout (dispatchAction)
import Gargantext.Pages.Layout.Specs (layoutSpec)
import Gargantext.Pages.Layout.States (initAppState)
import Gargantext.Router (routing)
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

setUnsafeComponentWillMount :: forall s. Effect Unit -> Record s -> Record (unsafeComponentWillMount :: Effect Unit | s)
setUnsafeComponentWillMount = unsafeSet "unsafeComponentWillMount"

main :: Effect Unit
main = do
  state <- initAppState
  case T.createReactSpec layoutSpec (const state) of
    { spec, dispatcher } -> void $ do
      let setRouting this = void $ do
            matches routing (dispatchAction (dispatcher this))
          spec' this = setUnsafeComponentWillMount (setRouting this) <$> (spec this)
      document <- window >>= document
      container <- unsafePartial (fromJust  <$> querySelector (QuerySelector "#app") (toParentNode document))
      h <- getHash
      case h of
        "" -> setHash "/"
        _ -> do
          setHash "/"
          setHash h
      let e = R.unsafeCreateElement (R.component "GargantextMain" spec') {} []
      RDOM.render e container
