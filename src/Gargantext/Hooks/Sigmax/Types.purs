module Gargantext.Hooks.Sigmax.Types where

import Prelude (map, ($), (&&), (==), class Eq, class Ord, class Show, Ordering, compare)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Data.Sequence (Seq)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import DOM.Simple.Types (Element)

newtype Graph n e = Graph { nodes :: Seq {|n}, edges :: Seq {|e} }

--derive instance eqGraph :: Eq Graph

--instance eqGraph :: Eq Graph where
--  eq (Graph {nodes: n1, edges: e1}) (Graph {nodes: n2, edges: e2}) = n1 == n2 && e1 == e2


type Renderer = { "type" :: String, container :: Element }


type Node =
  ( borderColor :: String
  , color :: String
  , hidden :: Boolean
  , id    :: String
  , label :: String
  , size  :: Number
  , type  :: String  -- available types: circle, cross, def, diamond, equilateral, pacman, square, star
  , x     :: Number
  , y     :: Number
  )

type Edge =
  ( color :: String
  , confluence :: Number
  , id :: String
  , hidden :: Boolean
  , size :: Number
  , source :: String
  , target :: String
  , weight :: Number )

type SelectedNodeIds = Set.Set String
type SelectedEdgeIds = Set.Set String
type EdgesMap = Map.Map String (Record Edge)
type NodesMap = Map.Map String (Record Node)

type SGraph = Graph Node Edge

graphEdges :: SGraph -> Seq (Record Edge)
graphEdges (Graph {edges}) = edges

graphNodes :: SGraph -> Seq (Record Node)
graphNodes (Graph {nodes}) = nodes

edgesGraphMap :: Graph Node Edge -> EdgesMap
edgesGraphMap graph =
  Map.fromFoldable $ map (\e -> Tuple e.id e) $ graphEdges graph

nodesMap :: Seq (Record Node) -> NodesMap
nodesMap nodes = Map.fromFoldable $ map (\n -> Tuple n.id n) nodes

nodesGraphMap :: Graph Node Edge -> NodesMap
nodesGraphMap graph =
  nodesMap $ graphNodes graph

eqGraph :: (Graph Node Edge) -> (Graph Node Edge) -> Boolean
eqGraph (Graph {nodes: n1, edges: e1}) (Graph {nodes: n2, edges: e2}) = (n1 == n2) && (e1 == e2)


-- | Custom state for force atlas. Basically, it can be "Running" or "Paused"
-- however when graph is loaded initially, forceAtlas is running for a couple of
-- seconds and then stops (unless the user alters this by clicking the toggle
-- button).
data ForceAtlasState = InitialRunning | Running | Paused

derive instance genericForceAtlasState :: Generic ForceAtlasState _
instance eqForceAtlasState :: Eq ForceAtlasState where
  eq = genericEq

toggleForceAtlasState :: ForceAtlasState -> ForceAtlasState
toggleForceAtlasState InitialRunning = Paused
toggleForceAtlasState Running = Paused
toggleForceAtlasState Paused = Running

-- | Custom state for show edges. Normally it is EShow or EHide (show/hide
-- | edges). However, edges are temporarily turned off when forceAtlas is
-- | running.
data ShowEdgesState = EShow | EHide | ETempHiddenThenShow

derive instance genericShowEdgesState :: Generic ShowEdgesState _
instance eqShowEdgesState :: Eq ShowEdgesState where
  eq = genericEq
instance showShowEdgesState :: Show ShowEdgesState where
  show = genericShow

-- | Whether the edges are hidden now (temp or "stable").
edgeStateHidden :: ShowEdgesState -> Boolean
edgeStateHidden EHide = true
edgeStateHidden ETempHiddenThenShow = true
edgeStateHidden _ = false

-- | Switch from hidden to shown, handling the temp state as well.
toggleShowEdgesState :: ShowEdgesState -> ShowEdgesState
toggleShowEdgesState s =
  if edgeStateHidden s then
    EShow
  else
    EHide

-- | Return the temporary hidden state, if applicable.
edgeStateTempHide :: ShowEdgesState -> ShowEdgesState
edgeStateTempHide EHide = EHide
edgeStateTempHide _ = ETempHiddenThenShow

-- | Whether, after disabling the temp state, edges will be shown or hidden.
edgeStateWillBeHidden :: ShowEdgesState -> Boolean
edgeStateWillBeHidden EHide = true
edgeStateWillBeHidden _ = false

-- | Get rid of the temporary transition
edgeStateStabilize :: ShowEdgesState -> ShowEdgesState
edgeStateStabilize ETempHiddenThenShow = EShow
edgeStateStabilize s = s

-- | Return state in which showEdges should be depending on forceAtlasState
forceAtlasEdgeState :: ForceAtlasState -> ShowEdgesState -> ShowEdgesState
forceAtlasEdgeState InitialRunning EShow = ETempHiddenThenShow
forceAtlasEdgeState InitialRunning es = es
forceAtlasEdgeState Running EShow = ETempHiddenThenShow
forceAtlasEdgeState Running es = es
forceAtlasEdgeState Paused ETempHiddenThenShow = EShow
forceAtlasEdgeState Paused es = es
