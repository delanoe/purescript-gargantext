module Gargantext.Hooks.Sigmax
  where

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
import Data.Tuple.Nested ((/\))
import DOM.Simple.Types (Element)
import Effect (Effect)
import Effect.Class.Console (error)
import Effect.Timer (TimeoutId, clearTimeout)
import FFI.Simple ((.=))
import Gargantext.Hooks.Sigmax.ForceAtlas2 as ForceAtlas
import Gargantext.Hooks.Sigmax.Graphology as Graphology
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Hooks.Sigmax.Types as ST
import Gargantext.Utils.Console as C
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Set as GSet
import Prelude (Unit, bind, discard, flip, map, not, pure, unit, ($), (&&), (*>), (<<<), (<>), (>>=), (+), (>), negate, (/=), (==), (<$>))
import Reactix as R
import Toestand as T

type Sigma =
  { sigma :: R.Ref (Maybe Sigma.Sigma)
    -- TODO is Seq in cleanup really necessary?
  , cleanup :: R.Ref (Seq (Effect Unit))
  }

type Data n e = { graph :: R.Ref (ST.Graph n e) }

moduleName :: R2.Module
moduleName = "Gargantext.Hooks.Sigmax"

console :: C.Console
console = C.encloseContext C.Main moduleName

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
    report = either (console.log2 errorMsg) (\_ -> console.log successMsg)
    prefix = "[" <> context <> "] "
    errorMsg = prefix <> "Error killing sigma:"
    successMsg = prefix <> "Killed sigma"

-- refreshData :: Sigma.Sigma -> Graphology.Graph -> Effect Unit
-- refreshData sigma graph = do
--   console.log clearingMsg
--   Graphology.clear sigmaGraph
--   console.log readingMsg
--   _ <- Graphology.updateWithGraph sigmaGraph graph

--   -- refresh
--   console.log refreshingMsg
--   Sigma.refresh sigma
--   --pure $ either (console.log2 errorMsg) refresh
--   where
--     sigmaGraph = Sigma.graph sigma
--     refresh _ = console.log refreshingMsg *> Sigma.refresh sigma
--     clearingMsg = "[refreshData] Clearing existing graph data"
--     readingMsg = "[refreshData] Reading graph data"
--     refreshingMsg = "[refreshData] Refreshing graph"
--     errorMsg = "[refreshData] Error reading graph data:"

dependOnSigma :: Sigma -> String -> (Sigma.Sigma -> Effect Unit) -> Effect Unit
dependOnSigma sigma notFoundMsg f = do
  case readSigma sigma of
    Nothing -> console.warn notFoundMsg
    Just sig -> f sig

dependOnContainer :: R.Ref (Nullable Element) -> String -> (Element -> Effect Unit) -> Effect Unit
dependOnContainer container notFoundMsg f = do
  case R.readNullableRef container of
    Nothing -> console.warn notFoundMsg
    Just c -> f c


-- Effectful versions of the above code

-- | Effect for handling pausing FA via state changes.  We need this because
-- | pausing can be done not only via buttons but also from the initial
-- | setTimer.
handleForceAtlas2Pause :: forall settings. R.Ref (Maybe ForceAtlas.FA2Layout)
                          -> T.Box ST.ForceAtlasState
                          -> R.Ref (Maybe TimeoutId)
                          -> settings
                          -> Effect Unit
handleForceAtlas2Pause fa2Ref forceAtlasState mFAPauseRef settings = do
  let fa2_ = R.readRef fa2Ref
  toggled <- T.read forceAtlasState
  case fa2_ of
    Nothing -> pure unit
    Just fa2 -> do
      isFARunning <- ForceAtlas.isRunning fa2
      case Tuple toggled isFARunning of
        Tuple ST.InitialRunning false -> do
          -- console.log "[handleForceAtlas2Paue)] restarting FA (InitialRunning)"
          ForceAtlas.start fa2
        Tuple ST.Running false -> do
          -- console.log2 "[handleForceAtlas2Pause] restarting FA (Running)" fa2
          Graphology.updateGraphOnlyVisible (ForceAtlas.graph fa2)
          ForceAtlas.start fa2
          case R.readRef mFAPauseRef of
            Nothing -> pure unit
            Just timeoutId -> clearTimeout timeoutId
        Tuple ST.Paused true -> do
          -- console.log "[handleForceAtlas2Pause] stopping FA (Paused)"
          ForceAtlas.stop fa2
        _ -> pure unit

setSigmaEdgesVisibility :: Sigma.Sigma -> Record ST.EdgeVisibilityProps -> Effect Unit
setSigmaEdgesVisibility sigma ev = do
  let settings = {
      hideEdgesOnMove: ST.edgeStateHidden ev.showEdges
    }
  Sigma.setSettings sigma settings
  Graphology.updateEachEdgeAttributes (Sigma.graph sigma) $ ST.setEdgeVisibility ev


-- updateEdges :: Sigma.Sigma -> ST.EdgesMap -> Effect Unit
-- updateEdges sigma edgesMap = do
--   Graphology.forEachEdge (Sigma.graph sigma) \e -> do
--     let mTEdge = Map.lookup e.id edgesMap
--     case mTEdge of
--       Nothing -> error $ "Edge id " <> e.id <> " not found in edgesMap"
--       (Just {color: tColor, hidden: tHidden}) -> do
--         _ <- pure $ (e .= "color") tColor
--         _ <- pure $ (e .= "hidden") tHidden
--         pure unit
--   --Sigma.refresh sigma


-- updateNodes :: Sigma.Sigma -> ST.NodesMap -> Effect Unit
-- updateNodes sigma nodesMap = do
--   Graphology.forEachNode (Sigma.graph sigma) \n -> do
--     let mTNode = Map.lookup n.id nodesMap
--     case mTNode of
--       Nothing -> error $ "Node id " <> n.id <> " not found in nodesMap"
--       (Just { borderColor: tBorderColor
--              , color: tColor
--              , equilateral: tEquilateral
--              , hidden: tHidden
--              , type: tType }) -> do
--         _ <- pure $ (n .= "borderColor") tBorderColor
--         _ <- pure $ (n .= "color") tColor
--         _ <- pure $ (n .= "equilateral") tEquilateral
--         _ <- pure $ (n .= "hidden") tHidden
--         _ <- pure $ (n .= "type") tType
--         pure unit
--   --Sigma.refresh sigma


-- | Toggles item visibility in the selected set
--   Basically: add items that are NOT in `selected` and remove items
--   that are in `selected`.
multiSelectUpdate :: ST.NodeIds -> ST.NodeIds -> ST.NodeIds
multiSelectUpdate new selected = foldl GSet.toggle selected new


bindSelectedNodesClick :: Sigma.Sigma -> T.Box ST.NodeIds -> T.Box Boolean -> Effect Unit
bindSelectedNodesClick sigma selectedNodeIds multiSelectEnabled =
  Sigma.bindClickNodes sigma $ \nodeIds' -> do
    let nodeIds = Set.fromFoldable nodeIds'
    multiSelectEnabled' <- T.read multiSelectEnabled
    if multiSelectEnabled' then
      T.modify_ (multiSelectUpdate nodeIds) selectedNodeIds
    else
      T.write_ nodeIds selectedNodeIds

bindShiftWheel :: Sigma.Sigma -> T.Box Number -> Effect Unit
bindShiftWheel sigma mouseSelectorSize =
  Sigma.bindShiftWheel sigma $ \delta -> do
    let step = if delta > 0.0 then 5.0 else -5.0
    val <- T.read mouseSelectorSize
    let newVal = val + step
    Sigma.setSettings sigma {
      mouseSelectorSize: newVal
      }
    T.write_ newVal mouseSelectorSize

selectorWithSize :: Sigma.Sigma -> Int -> Effect Unit
selectorWithSize _ _ = do
  pure unit

performDiff :: Sigma.Sigma -> ST.SGraph -> Effect Unit
performDiff sigma g = do
  -- if (Seq.null addEdges) && (Seq.null addNodes) && (Set.isEmpty removeEdges) && (Set.isEmpty removeNodes) then
  --   pure unit
  -- else do
  -- console.log2 "[performDiff] addNodes" addNodes
  -- console.log2 "[performDiff] addEdges" $ A.fromFoldable addEdges
  -- console.log2 "[performDiff] removeNodes" removeNodes
  -- console.log2 "[performDiff] removeEdges" removeEdges
  traverse_ (Graphology.addNode sigmaGraph) addNodes
  traverse_ (Graphology.addEdge sigmaGraph) addEdges
  traverse_ (Graphology.removeEdge sigmaGraph) removeEdges
  traverse_ (Graphology.removeNode sigmaGraph) removeNodes
  traverse_ (Graphology.updateEdge sigmaGraph) updateEdges
  --traverse_ (Graphology.updateNode sigmaGraph) updateNodes
  traverse_ (\n -> Graphology.mergeNodeAttributes sigmaGraph n.id { borderColor: n.borderColor
                                                                  , color: n.color
                                                                  , equilateral: n.equilateral
                                                                  , hidden: n.hidden
                                                                  , highlighted: n.highlighted }) updateNodes
  --Sigma.refresh sigma
  -- TODO Use FA2Layout here
  --Sigma.killForceAtlas2 sigma
  where
    sigmaGraph = Sigma.graph sigma
    { add: Tuple addEdges addNodes
    , remove: Tuple removeEdges removeNodes
    , update: Tuple updateEdges updateNodes } = sigmaDiff sigmaGraph g


-- | Compute a diff between current sigma graph and whatever is set via custom controls
sigmaDiff :: Graphology.Graph -> ST.SGraph -> Record ST.SigmaDiff
sigmaDiff sigmaGraph gControls = { add, remove, update }
  where
    add = Tuple addEdges addNodes
    remove = Tuple removeEdges removeNodes
    update = Tuple updateEdges updateNodes

    sigmaNodes = Graphology.nodes sigmaGraph
    sigmaEdges = Graphology.edges sigmaGraph
    sigmaNodeIds = Set.fromFoldable $ Seq.map _.id sigmaNodes
    sigmaEdgeIds = Set.fromFoldable $ Seq.map _.id sigmaEdges

    gcNodes = ST.graphNodes gControls
    gcEdges = ST.graphEdges gControls
    gcNodeIds = Seq.map _.id gcNodes
    gcEdgeIds = Seq.map _.id gcEdges


    -- Add nodes/edges which aren't present in `sigmaGraph`, but are
    -- in `gControls`
    addGC = ST.edgesFilter (\e -> not (Set.member e.id sigmaEdgeIds)) $
            ST.nodesFilter (\n -> not (Set.member n.id sigmaNodeIds)) gControls
    addEdges = ST.graphEdges addGC
    addNodes = ST.graphNodes addGC
    -- addEdges = Seq.empty
    -- addNodes = ST.graphNodes addGC

    -- Remove nodes/edges from `sigmaGraph` which aren't in
    -- `gControls`
    removeEdges = Set.difference sigmaEdgeIds (Set.fromFoldable gcEdgeIds)
    removeNodes = Set.difference sigmaNodeIds (Set.fromFoldable gcNodeIds)

    commonNodeIds = Set.intersection sigmaNodeIds $ Set.fromFoldable gcNodeIds
    commonEdgeIds = Set.intersection sigmaEdgeIds $ Set.fromFoldable gcEdgeIds
    sigmaNodeIdsMap = Map.fromFoldable $ Seq.map (\n -> Tuple n.id n) sigmaNodes
    sigmaEdgeIdsMap = Map.fromFoldable $ Seq.map (\e -> Tuple e.id e) sigmaEdges
    updateEdges = Seq.filter (\e -> Just e /= Map.lookup e.id sigmaEdgeIdsMap) gcEdges
    -- Find nodes for which `ST.compareNodes` returns `false`, i.e. nodes differ
    updateNodes = Seq.filter (\n -> (ST.compareNodes n <$> (Map.lookup n.id sigmaNodeIdsMap)) == Just false) gcNodes
    -- updateEdges = Seq.empty
    -- updateNodes = Seq.empty


-- DEPRECATED

-- markSelectedEdges :: Sigma.Sigma -> ST.EdgeIds -> ST.EdgesMap -> Effect Unit
-- markSelectedEdges sigma selectedEdgeIds graphEdges = do
--   Graphology.forEachEdge (Sigma.graph sigma) \e -> do
--     case Map.lookup e.id graphEdges of
--       Nothing -> error $ "Edge id " <> e.id <> " not found in graphEdges map"
--       Just {color} -> do
--         let newColor =
--               if Set.member e.id selectedEdgeIds then
--                 "#ff0000"
--               else
--                 color
--         _ <- pure $ (e .= "color") newColor
--         pure unit
--   Sigma.refresh sigma

-- markSelectedNodes :: Sigma.Sigma -> ST.NodeIds -> ST.NodesMap -> Effect Unit
-- markSelectedNodes sigma selectedNodeIds graphNodes = do
--   Graphology.forEachNode (Sigma.graph sigma) \n -> do
--     case Map.lookup n.id graphNodes of
--       Nothing -> error $ "Node id " <> n.id <> " not found in graphNodes map"
--       Just {color} -> do
--         let newColor =
--               if Set.member n.id selectedNodeIds then
--                 "#ff0000"
--               else
--                 color
--         _ <- pure $ (n .= "color") newColor
--         pure unit
--   Sigma.refresh sigma
