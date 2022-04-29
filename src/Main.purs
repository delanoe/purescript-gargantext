module Main (main) where

import DOM.Simple (Element)
import DOM.Simple.Document (document)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Effect (Effect)
import FFI.Simple ((...))
import Gargantext.Components.App (app)
import Gargantext.Components.App.Store as AppStore
import Gargantext.Utils.Reactix as R2
import Prelude (Unit, ($))

here :: R2.Here
here = R2.here "Gargantext.Main"

main :: Effect Unit
main = paint $ toMaybe (document ... "getElementById" $ [ "app" ])

paint :: Maybe Element -> Effect Unit
paint Nothing  = here.error "[main] Container not found"
paint (Just c) = R2.render app' c
  where
    state = AppStore.options
    app' =
      AppStore.provide
      state
      [
        app {} []
      ]
