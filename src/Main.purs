module Main (main) where

import Prelude (Unit, ($))
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import DOM.Simple (Element)
import DOM.Simple.Document (document)
import DOM.Simple.Console (log)
import Effect (Effect)
import FFI.Simple ((...))
import Reactix as R
import Gargantext.Components.App (app)

main :: Effect Unit
main = paint $ toMaybe (document ... "getElementById" $ [ "app" ])

paint :: Maybe Element -> Effect Unit
paint Nothing = log "[main] Container not found"
paint (Just c) = R.render (app {}) c
