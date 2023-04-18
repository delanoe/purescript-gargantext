module Gargantext.Components.GraphQL.IMT where

import Gargantext.Prelude

import Affjax.RequestBody (RequestBody(..))
import Data.Array as A
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Gargantext.Utils.GraphQL as GGQL
import Type.Proxy (Proxy(..))


type School
  = { school_id        :: String
    , school_longName  :: String
    , school_shortName :: String
    }

type SchoolsQuery
  = { imt_schools ::
         { school_id :: Unit
         , school_longName :: Unit
         , school_shortName :: Unit } }

schoolsQuery :: SchoolsQuery
schoolsQuery = { imt_schools:
                 GGQL.getFieldsStandard (Proxy :: _ School)
                }
