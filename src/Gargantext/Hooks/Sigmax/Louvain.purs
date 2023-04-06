module Gargantext.Hooks.Sigmax.Louvain where

-- FFI for communities-louvain: https://graphology.github.io/standard-library/communities-louvain

import Prelude

import Data.Array as A
import Data.Map as Map
import Data.Sequence as Seq
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Timer (setTimeout)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import FFI.Simple ((..), (...), (.=))
import Gargantext.Data.Louvain as DLouvain
import Gargantext.Hooks.Sigmax.Graphology as Graphology
import Gargantext.Hooks.Sigmax.Operators as Operators
import Gargantext.Hooks.Sigmax.Types as Types
import Record as Record

-- | Type representing the web worker.
foreign import data LouvainLayout :: Type

graph :: LouvainLayout -> Graphology.Graph
graph s = s .. "graph" :: Graphology.Graph

foreign import _assign :: forall settings. EffectFn2 Graphology.Graph settings Graphology.Graph

assign :: forall settings. Graphology.Graph -> settings -> Effect Graphology.Graph
assign = runEffectFn2 _assign

assignVisible :: forall settings. Graphology.Graph -> settings -> Effect Graphology.Graph
assignVisible g s = do
  n <- Graphology.copy g
  Graphology.updateGraphOnlyVisible n
  assign (Operators.toUndirected n) s

-- \[{ id, community }] -> { id: community }
cluster :: Graphology.Graph -> DLouvain.LouvainCluster
cluster g = Map.fromFoldable $ (\{ id, community } -> Tuple id community) <$> (Graphology.nodes g)
