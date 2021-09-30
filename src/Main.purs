module Main (main) where

import DOM.Simple (Element)
import DOM.Simple.Console (log)
import DOM.Simple.Document (document)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Effect (Effect)
import FFI.Simple ((...))
import Gargantext.Components.App (app)
import Gargantext.Utils.Reactix as R2
import Prelude (Unit, ($))

main :: Effect Unit
main = paint $ toMaybe (document ... "getElementById" $ [ "app" ])

paint :: Maybe Element -> Effect Unit
paint Nothing = log "[main] Container not found"
paint (Just c) = R2.render (app {} []) c
