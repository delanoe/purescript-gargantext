module Gargantext.Components.GraphExplorer.Utils
  ( stEdgeToGET, stNodeToGET
  , normalizeNodes
  , normalizeNodeSizeDefault
  , normalizeNodeSize
  , takeGreatestNodeByCluster, countNodeByCluster
  ) where

import Gargantext.Prelude

import Data.Array as A
import Data.Foldable (maximum, minimum)
import Data.Lens (Lens', lens, over, traversed, (^.))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.Number as DN
import Data.Sequence as Seq
import Data.Traversable (class Traversable)
import Gargantext.Components.GraphExplorer.GraphTypes as GEGT
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Hooks.Sigmax.Types as ST
import Gargantext.Utils (getter)
import Gargantext.Utils.Lens as GUL
import Gargantext.Utils.Seq as GUS

stEdgeToGET :: Record ST.Edge -> GEGT.Edge
stEdgeToGET { _original: GEGT.Edge original, hidden } = GEGT.Edge $ original { hidden = Just hidden }

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

-- | Normalize nodes, i.e. set their {x, y} values so that they are in
-- | range [0, 1].
normalizeNodes :: forall t. Traversable t => t GEGT.Node -> t GEGT.Node
normalizeNodes ns = GUL.normalizeLens xLens $ GUL.normalizeLens yLens ns
  where
    xLens :: Lens' GEGT.Node Number
    xLens = lens (\(GEGT.Node { x }) -> x) $ (\(GEGT.Node n) val -> GEGT.Node (n { x = val }))
    yLens :: Lens' GEGT.Node Number
    yLens = lens (\(GEGT.Node { y }) -> y) $ (\(GEGT.Node n) val -> GEGT.Node (n { y = val }))

type NodeSize r = { size :: Number | r }

normalizeNodeSizeDefault :: forall t r. Traversable t => t (NodeSize r) -> t (NodeSize r)
normalizeNodeSizeDefault ns = logSize <$> normalizeNodeSize 50.0 100000.0 ns
  where
    logSize (n@{ size }) = n { size = DN.log (size + 1.0) }

normalizeNodeSize :: forall t r. Traversable t =>
                     Number -> Number -> t (NodeSize r) -> t (NodeSize r)
normalizeNodeSize minSize maxSize ns = over traversed (over sizeLens (\s -> minSize + (s - sizeMin') * quotient)) ns
  where
    sizes = over traversed (_ ^. sizeLens) ns
    sizeMin = minimum sizes
    sizeMax = maximum sizes
    range = do
      sMin <- sizeMin
      sMax <- sizeMax
      pure $ sMax - sMin
    sizeMin' = fromMaybe 0.0 sizeMin
    divisor = maybe 1.0 (\r -> 1.0 / r) range
    quotient :: Number
    quotient = (maxSize - minSize) * divisor
    --quotient = (toNumber $ maxSize - minSize) * divisor
    --sizeLens :: Lens' GEGT.Node Number
    --sizeLens = lens (\(GEGT.Node { size }) -> size) $ (\(GEGT.Node n) val -> GEGT.Node (n { size = val }))
    sizeLens :: Lens' { size :: Number | r } Number
    sizeLens = lens (\{ size } -> size) $ \n val -> (n { size = val })

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
