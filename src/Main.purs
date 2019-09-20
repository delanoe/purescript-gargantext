module Main where

import Prelude (Unit, ($))

import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import DOM.Simple.Document (document)
import DOM.Simple.Console (log)
import Effect (Effect)
import FFI.Simple ((...))
import Reactix as R
import Gargantext.Components.Layout (layout)

main :: Effect Unit
main = paint $ toMaybe $ document ... "getElementById" $ [ "app" ]
  where
    paint Nothing = log "[main] Container not found"
    paint (Just c) = R.render (layout {}) c
