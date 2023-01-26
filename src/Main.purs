module Main (main) where

import DOM.Simple (Element)
import DOM.Simple.Document (document)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Effect (Effect)
import FFI.Simple ((...))
import Gargantext.Components.App as App
import Gargantext.Utils.Reactix as R2
import Prelude (Unit, ($), bind)
import Reactix as R

here :: R2.Here
here = R2.here "Gargantext.Main"

main :: Effect Unit
main = paint $ toMaybe (document ... "getElementById" $ [ "app" ])

paint :: Maybe Element -> Effect Unit
paint Nothing  = here.error "[main] Container not found"
paint (Just c) = do
  R.render app c
  where
    app = App.app {}
