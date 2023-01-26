module Gargantext.Hooks.Sigmax.Operators where

-- FFI for operators: https://graphology.github.io/standard-library/operators

import Prelude

import Data.Array as A
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Map as Map
import Data.Sequence as Seq
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Gargantext.Data.Louvain as DLouvain
import Gargantext.Hooks.Sigmax.Graphology as Graphology
import Gargantext.Hooks.Sigmax.Types as Types
import Record as Record

foreign import _toUndirected :: Fn1 Graphology.Graph Graphology.Graph

toUndirected :: Graphology.Graph -> Graphology.Graph
toUndirected = runFn1 _toUndirected
