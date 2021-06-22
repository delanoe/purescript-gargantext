module Gargantext.Components.Nodes.Lists.Types where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)

import Gargantext.Prelude
import Gargantext.Types (ListId, NodeID)
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Lists.Types"

data CacheState = CacheOn | CacheOff

derive instance genericCacheState :: Generic CacheState _
instance eqCacheState :: Eq CacheState where
  eq = genericEq
instance decodeJsonCacheState :: DecodeJson CacheState where
  decodeJson json = do
    obj <- decodeJson json
    case obj of
      "CacheOn"  -> pure CacheOn
      "CacheOff" -> pure CacheOff
      s          -> Left $ AtKey s $ TypeMismatch $ "Unknown cache value"
instance encodeJsonCacheState :: EncodeJson CacheState where
  encodeJson CacheOn  = encodeJson "CacheOn"
  encodeJson CacheOff = encodeJson "CacheOff"
instance showCacheState :: Show CacheState where
  show = genericShow

type SidePanel = ()

initialSidePanel :: Maybe (Record SidePanel)
initialSidePanel = Nothing
