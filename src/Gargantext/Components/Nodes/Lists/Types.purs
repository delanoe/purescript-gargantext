module Gargantext.Components.Nodes.Lists.Types where

import Data.Argonaut (class DecodeJson, decodeJson, class EncodeJson, encodeJson, (~>), (:=))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)

import Gargantext.Prelude

thisModule :: String
thisModule = "Gargantext.Components.Nodes.Lists.Types"

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
