module Gargantext.Hooks.Sigmax
  where

import Prelude
  ( Unit, bind, discard, flip, map, not, pure, unit
  , ($), (&&), (*>), (<<<), (<>), (>>=))
import Data.Array as A
import Data.Either (either)
import Data.Foldable (sequence_, foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested((/\))
import DOM.Simple.Console (log, log2)
import DOM.Simple.Types (Element)
import Effect (Effect)
import Effect.Class.Console (error)
import Effect.Timer (TimeoutId, clearTimeout)
import FFI.Simple ((.=))
import Reactix as R
import Toestand as T

import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Hooks.Sigmax.Types as ST
import Gargantext.Utils.Reactix as R2

type Sigma =
  { sigma :: R.Ref (Maybe Sigma.Sigma)
    -- TODO is Seq in cleanup really necessary?
  , cleanup :: R.Ref (Seq (Effect Unit))
  }

type Data n e = { graph :: R.Ref (ST.Graph n e) }

initSigma :: R.Hooks Sigma
initSigma = do
    s <- R2.nothingRef
    c <- R.useRef Seq.empty
    pure { sigma: s, cleanup: c }

readSigma :: Sigma -> Maybe Sigma.Sigma
readSigma sigma = R.readRef sigma.sigma

writeSigma :: Sigma -> Maybe Sigma.Sigma -> Effect Unit
writeSigma sigma = R.setRef sigma.sigma

-- | Pushes to the back of the cleanup sequence. Cleanup happens
-- | *before* sigma is destroyed
cleanupLast :: Sigma -> Effect Unit -> Effect Unit
cleanupLast sigma = R.setRef sigma.cleanup <<< Seq.snoc existing
  where existing = R.readRef sigma.cleanup

-- | Pushes to the front of the cleanup sequence. Cleanup happens
-- | *before* sigma is destroyed
cleanupFirst :: Sigma -> Effect Unit -> Effect Unit
cleanupFirst sigma =
  R.setRef sigma.cleanup <<< (flip Seq.cons) (R.readRef sigma.cleanup)

cleanupSigma :: Sigma -> String -> Effect Unit
cleanupSigma sigma context = traverse_ kill (readSigma sigma)
  where
    kill sig = runCleanups *> killSigma *> emptyOut
      where -- close over sig
        killSigma = Sigma.killSigma sig >>= report
    runCleanups = sequence_ (R.readRef sigma.cleanup)
    emptyOut = writeSigma sigma Nothing *> R.setRef sigma.cleanup Seq.empty
    report = either (log2 errorMsg) (\_ -> log successMsg)
    prefix = "[" <> context <> "] "
    errorMsg = prefix <> "Error killing sigma:"
    successMsg = prefix <> "Killed sigma"

refreshData :: forall n e. Sigma.Sigma -> Sigma.Graph n e -> Effect Unit
refreshData sigma graph
  =   log clearingMsg
  *>  Sigma.clear sigmaGraph
  *>  log readingMsg
  *>  Sigma.graphRead sigmaGraph graph
  >>= either (log2 errorMsg) refresh
  where
    sigmaGraph = Sigma.graph sigma
    refresh _ = log refreshingMsg *> Sigma.refresh sigma
    clearingMsg = "[refreshData] Clearing existing graph data"
    readingMsg = "[refreshData] Reading graph data"
    refreshingMsg = "[refreshData] Refreshing graph"
    errorMsg = "[refreshData] Error reading graph data:"

sigmafy :: forall n e. ST.Graph n e -> Sigma.Graph n e
sigmafy (ST.Graph g) = {nodes,edges}
  where
    nodes = A.fromFoldable g.nodes
    edges = A.fromFoldable g.edges

dependOnSigma :: Sigma -> String -> (Sigma.Sigma -> Effect Unit) -> Effect Unit
dependOnSigma sigma notFoundMsg f = do
  case readSigma sigma of
    Nothing -> log notFoundMsg
    Just sig -> f sig

dependOnContainer :: R.Ref (Nullable Element) -> String -> (Element -> Effect Unit) -> Effect Unit
dependOnContainer container notFoundMsg f = do
  case R.readNullableRef container of
    Nothing -> log notFoundMsg
    Just c -> f c


-- Effectful versions of the above code

-- | Effect for handling pausing FA via state changes.  We need this because
-- | pausing can be done not only via buttons but also from the initial
-- | setTimer.
handleForceAtlas2Pause :: forall settings. R.Ref Sigma -> T.Box ST.ForceAtlasState -> R.Ref (Maybe TimeoutId) -> settings -> Effect Unit
handleForceAtlas2Pause sigmaRef forceAtlasState mFAPauseRef settings = do
  let sigma = R.readRef sigmaRef
  toggled <- T.read forceAtlasState
  dependOnSigma sigma "[handleForceAtlas2Pause] sigma: Nothing" $ \s -> do
    let isFARunning = Sigma.isForceAtlas2Running s
    case Tuple toggled isFARunning of
      Tuple ST.InitialRunning false -> do
        Sigma.restartForceAtlas2 s settings
      Tuple ST.Running false -> do
        Sigma.restartForceAtlas2 s settings
        case R.readRef mFAPauseRef of
          Nothing -> pure unit
          Just timeoutId -> clearTimeout timeoutId
      Tuple ST.Paused true -> do
        Sigma.stopForceAtlas2 s
      _ -> pure unit

setEdges :: Sigma.Sigma -> Boolean -> Effect Unit
setEdges sigma val = do
  let settings = {
        drawEdges: val
      , drawEdgeLabels: val
      , hideEdgesOnMove: not val
    }
  Sigma.setSettings sigma settings


updateEdges :: Sigma.Sigma -> ST.EdgesMap -> Effect Unit
updateEdges sigma edgesMap = do
  Sigma.forEachEdge (Sigma.graph sigma) \e -> do
    let mTEdge = Map.lookup e.id edgesMap
    case mTEdge of
      Nothing -> error $ "Edge id " <> e.id <> " not found in edgesMap"
      (Just {color: tColor, hidden: tHidden}) -> do
        _ <- pure $ (e .= "color") tColor
        _ <- pure $ (e .= "hidden") tHidden
        pure unit
  --Sigma.refresh sigma


updateNodes :: Sigma.Sigma -> ST.NodesMap -> Effect Unit
updateNodes sigma nodesMap = do
  Sigma.forEachNode (Sigma.graph sigma) \n -> do
    let mTNode = Map.lookup n.id nodesMap
    case mTNode of
      Nothing -> error $ "Node id " <> n.id <> " not found in nodesMap"
      (Just { borderColor: tBorderColor
             , color: tColor
             , equilateral: tEquilateral
             , hidden: tHidden
             , type: tType }) -> do
        _ <- pure $ (n .= "borderColor") tBorderColor
        _ <- pure $ (n .= "color") tColor
        _ <- pure $ (n .= "equilateral") tEquilateral
        _ <- pure $ (n .= "hidden") tHidden
        _ <- pure $ (n .= "type") tType
        pure unit
  --Sigma.refresh sigma


-- | Toggles item visibility in the selected set
multiSelectUpdate :: ST.NodeIds -> ST.NodeIds -> ST.NodeIds
multiSelectUpdate new selected = foldl fld selected new
  where
    fld selectedAcc item =
      if Set.member item selectedAcc then
        Set.delete item selectedAcc
      else
        Set.insert item selectedAcc


bindSelectedNodesClick :: Sigma.Sigma -> T.Box ST.NodeIds -> R.Ref Boolean -> Effect Unit
bindSelectedNodesClick sigma selectedNodeIds multiSelectEnabledRef =
  Sigma.bindClickNodes sigma $ \nodes -> do
    let multiSelectEnabled = R.readRef multiSelectEnabledRef
    let nodeIds = Set.fromFoldable $ map _.id nodes
    if multiSelectEnabled then
      T.modify_ (multiSelectUpdate nodeIds) selectedNodeIds
    else
      T.write_ nodeIds selectedNodeIds

bindSelectedEdgesClick :: R.Ref Sigma -> R.State ST.EdgeIds -> Effect Unit
bindSelectedEdgesClick sigmaRef (_ /\ setEdgeIds) =
  dependOnSigma (R.readRef sigmaRef) "[graphCpt] no sigma" $ \sigma -> do
    Sigma.bindClickEdge sigma $ \edge -> do
      setEdgeIds \eids ->
        if Set.member edge.id eids then
          Set.delete edge.id eids
        else
          Set.insert edge.id eids

selectorWithSize :: Sigma.Sigma -> Int -> Effect Unit
selectorWithSize sigma size = do
  pure unit

performDiff :: Sigma.Sigma -> ST.SGraph -> Effect Unit
performDiff sigma g = do
  if (Seq.null addEdges) && (Seq.null addNodes) && (Set.isEmpty removeEdges) && (Set.isEmpty removeNodes) then
    pure unit
  else do
    traverse_ (Sigma.addNode sigmaGraph) addNodes
    traverse_ (Sigma.addEdge sigmaGraph) addEdges
    traverse_ (Sigma.removeEdge sigmaGraph) removeEdges
    traverse_ (Sigma.removeNode sigmaGraph) removeNodes
    Sigma.refresh sigma
    Sigma.killForceAtlas2 sigma
  where
    sigmaGraph = Sigma.graph sigma
    sigmaEdgeIds = Sigma.sigmaEdgeIds sigmaGraph
    sigmaNodeIds = Sigma.sigmaNodeIds sigmaGraph
    {add: Tuple addEdges addNodes, remove: Tuple removeEdges removeNodes} = ST.sigmaDiff sigmaEdgeIds sigmaNodeIds g
-- DEPRECATED

markSelectedEdges :: Sigma.Sigma -> ST.EdgeIds -> ST.EdgesMap -> Effect Unit
markSelectedEdges sigma selectedEdgeIds graphEdges = do
  Sigma.forEachEdge (Sigma.graph sigma) \e -> do
    case Map.lookup e.id graphEdges of
      Nothing -> error $ "Edge id " <> e.id <> " not found in graphEdges map"
      Just {color} -> do
        let newColor =
              if Set.member e.id selectedEdgeIds then
                "#ff0000"
              else
                color
        _ <- pure $ (e .= "color") newColor
        pure unit
  Sigma.refresh sigma

markSelectedNodes :: Sigma.Sigma -> ST.NodeIds -> ST.NodesMap -> Effect Unit
markSelectedNodes sigma selectedNodeIds graphNodes = do
  Sigma.forEachNode (Sigma.graph sigma) \n -> do
    case Map.lookup n.id graphNodes of
      Nothing -> error $ "Node id " <> n.id <> " not found in graphNodes map"
      Just {color} -> do
        let newColor =
              if Set.member n.id selectedNodeIds then
                "#ff0000"
              else
                color
        _ <- pure $ (n .= "color") newColor
        pure unit
  Sigma.refresh sigma

getEdges :: Sigma.Sigma -> Effect (Array (Record ST.Edge))
getEdges sigma = Sigma.getEdges sigma

getNodes :: Sigma.Sigma -> Effect (Array (Record ST.Node))
getNodes sigma = Sigma.getNodes sigma
