module Gargantext.Global
  ( Global, defaultGlobal )
  where

import Prelude (pure)
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Gargantext.Config (Ends, defaultEnds)
import Gargantext.Components.Login.Types (AuthData(..))

type Global =
  { ends :: Ends
  , authData :: Maybe AuthData }

defaultGlobal :: Effect Global
defaultGlobal = pure { ends: defaultEnds, authData: Nothing }
