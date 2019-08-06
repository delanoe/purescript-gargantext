module Gargantext.Pages.Corpus.Chart.Utils where

import Data.Tuple.Nested ((/\))
import Effect.Uncurried (mkEffectFn1)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

reloadButtonWrap :: R.State Int -> R.Element -> R.Element
reloadButtonWrap setReload el = H.div {} [
    reloadButton setReload
  , el
  ]

reloadButton :: R.State Int -> R.Element
reloadButton (_ /\ setReload) = H.a {className, onClick, title: "Reload"} []
  where
    className = "reload-btn glyphicon glyphicon-refresh"
    onClick = mkEffectFn1 $ \_ -> setReload $ \r -> r + 1
