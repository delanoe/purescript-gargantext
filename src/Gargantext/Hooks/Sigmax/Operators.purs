module Gargantext.Hooks.Sigmax.Operators where

-- FFI for operators: https://graphology.github.io/standard-library/operators

import Data.Function.Uncurried (Fn1, runFn1)
import Gargantext.Hooks.Sigmax.Graphology as Graphology


foreign import _toUndirected :: Fn1 Graphology.Graph Graphology.Graph

toUndirected :: Graphology.Graph -> Graphology.Graph
toUndirected = runFn1 _toUndirected
