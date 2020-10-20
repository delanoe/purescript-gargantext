module Gargantext.Components.Nodes.Lists.Types where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)

import Gargantext.Prelude

thisModule = "Gargantext.Components.Nodes.Lists.Types"

data CacheState = CacheOn | CacheOff

derive instance genericCacheState :: Generic CacheState _
instance eqCacheState :: Eq CacheState where
  eq = genericEq
instance showCacheState :: Show CacheState where
  show = genericShow
