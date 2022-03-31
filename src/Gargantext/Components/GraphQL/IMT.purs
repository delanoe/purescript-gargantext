module Gargantext.Components.GraphQL.IMT where

import Gargantext.Prelude

import Affjax.RequestBody (RequestBody(..))
import Data.Array as A
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import GraphQL.Client.Args (NotNull, (=>>))
import GraphQL.Client.Variable (Var(..))


type School
  = { school_id        :: String
    , school_longName  :: String
    , school_shortName :: String
    }

schoolsQuery = { imt_schools:
                  { school_id: unit
                  , school_longName: unit
                  , school_shortName: unit}
                }

