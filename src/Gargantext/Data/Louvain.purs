module Gargantext.Data.Louvain where

import Prelude (Unit, unit, ($), (<$>))
import Data.Function.Uncurried (Fn1, runFn1, Fn3, runFn3)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Tuple.Native (T2, prj)
import Data.Typelevel.Num (d0, d1)

foreign import data Louvain :: Type

type Node = String

type Edge =
  (
    source :: Node
  , target :: Node
  , weight :: Number
  )

type Cluster = Int
type LouvainCluster_ = T2 Node Cluster
type LouvainCluster = Map.Map Node Cluster

foreign import _jLouvain :: Fn1 Unit Louvain

louvain :: Unit -> Louvain
louvain unit = runFn1 _jLouvain unit

foreign import _init :: Fn3 Louvain (Array Node) (Array (Record Edge)) (Array LouvainCluster_)

init :: Louvain -> Array Node -> Array (Record Edge) -> LouvainCluster
init l nds edgs = Map.fromFoldable clusterTuples
  where
    clusterArr = runFn3 _init l nds edgs
    clusterTuples = (\t2 -> Tuple (prj d0 t2) (prj d1 t2)) <$> clusterArr
