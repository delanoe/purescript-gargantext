module Gargantext.Components.Nodes.Lists.Types where

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Simple.JSON as JSON
import Simple.JSON.Generics as JSONG

import Gargantext.Prelude
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Lists.Types"

data CacheState = CacheOn | CacheOff

derive instance Generic CacheState _
instance JSON.ReadForeign CacheState where
  readImpl = JSONG.enumSumRep
instance JSON.WriteForeign CacheState where
  writeImpl = JSON.writeImpl <<< show
instance Eq CacheState where
  eq = genericEq
instance Show CacheState where
  show = genericShow

type SidePanel :: forall k. Row k
type SidePanel = ()

initialSidePanel :: Maybe (Record SidePanel)
initialSidePanel = Nothing
