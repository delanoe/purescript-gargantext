module Gargantext.Components.GraphExplorer.Utils
  ( stEdgeToGET, stNodeToGET
  , normalizeNodes
  , takeGreatestNodeByCluster, countNodeByCluster
  ) where

import Gargantext.Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Hooks.Sigmax.Types as ST
import Gargantext.Utils (getter)
import Gargantext.Utils.Array as GUA

stEdgeToGET :: Record ST.Edge -> GET.Edge
stEdgeToGET { _original } = _original

stNodeToGET :: Record ST.Node -> GET.Node
stNodeToGET { id, label, x, y, _original: GET.Node { attributes, size, type_ } } = GET.Node {
    attributes
  , children: []
  , id_: id
  , label
  , size
  , type_
  , x
  , y
  }

-----------------------------------------------------------------------

normalizeNodes :: Array GET.Node -> Array GET.Node
normalizeNodes ns = map normalizeNode ns
  where
    xs = map (\(GET.Node { x }) -> x) ns
    ys = map (\(GET.Node { y }) -> y) ns
    mMinx = GUA.min xs
    mMaxx = GUA.max xs
    mMiny = GUA.min ys
    mMaxy = GUA.max ys
    mXrange = do
      minx <- mMinx
      maxx <- mMaxx
      pure $ maxx - minx
    mYrange = do
      miny <- mMiny
      maxy <- mMaxy
      pure $ maxy - miny
    xdivisor = case mXrange of
      Nothing -> 1.0
      Just xdiv -> 1.0 / xdiv
    ydivisor = case mYrange of
      Nothing -> 1.0
      Just ydiv -> 1.0 / ydiv
    normalizeNode (GET.Node n@{ x, y }) = GET.Node $ n { x = x * xdivisor
                                                       , y = y * ydivisor }

------------------------------------------------------------------------

takeGreatestNodeByCluster :: GET.HyperdataGraph -> Int -> Int -> Array GET.Node
takeGreatestNodeByCluster graphData take clusterId
  =   graphData
  #   getter _.graph
  >>> getter _.nodes
  >>> A.filter
      (   getter _.attributes
      >>> getter _.clustDefault
      >>> eq clusterId
      )
  >>> A.sortWith
      ( getter _.size
      )
  >>> A.takeEnd take
  >>> A.reverse

countNodeByCluster :: GET.HyperdataGraph -> Int -> GET.ClusterCount
countNodeByCluster graphData clusterId
  =   graphData
  #   getter _.graph
  >>> getter _.nodes
  >>> A.filter
      (   getter _.attributes
      >>> getter _.clustDefault
      >>> eq clusterId
      )
  >>> A.length
  >>> { id:  clusterId
      , count: _
      }
  >>> wrap
