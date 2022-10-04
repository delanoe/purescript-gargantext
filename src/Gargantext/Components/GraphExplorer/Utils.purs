module Gargantext.Components.GraphExplorer.Utils
  ( stEdgeToGET, stNodeToGET
  , normalizeNodes
  , takeGreatestNodeByCluster, countNodeByCluster
  ) where

import Gargantext.Prelude

import Data.Array as A
import Data.Foldable (maximum, minimum)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Sequence as Seq
import Gargantext.Components.GraphExplorer.GraphTypes as GEGT
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Hooks.Sigmax.Types as ST
import Gargantext.Utils (getter)
import Gargantext.Utils.Seq as GUS

stEdgeToGET :: Record ST.Edge -> GEGT.Edge
stEdgeToGET { _original } = _original

stNodeToGET :: Record ST.Node -> GEGT.Node
stNodeToGET { id, label, x, y, _original: GEGT.Node { attributes, size, type_ } } = GEGT.Node {
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

normalizeNodes :: Seq.Seq GEGT.Node -> Seq.Seq GEGT.Node
normalizeNodes ns = Seq.map normalizeNode ns
  where
    xs = map (\(GEGT.Node { x }) -> x) ns
    ys = map (\(GEGT.Node { y }) -> y) ns
    mMinx = minimum xs
    mMaxx = maximum xs
    mMiny = minimum ys
    mMaxy = maximum ys
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
    normalizeNode (GEGT.Node n@{ x, y }) = GEGT.Node $ n { x = x * xdivisor
                                                         , y = y * ydivisor }

------------------------------------------------------------------------

takeGreatestNodeByCluster :: GET.HyperdataGraph -> Int -> Int -> Array GEGT.Node
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

countNodeByCluster :: GET.HyperdataGraph -> Int -> GEGT.ClusterCount
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
