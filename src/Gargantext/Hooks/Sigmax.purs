module Gargantext.Hooks.Sigmax
  -- (
  -- )
  where

import DOM.Simple.Console (log, log2)
import DOM.Simple.Types (Element)
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Foldable (sequence_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested((/\))
import Effect (Effect)
import Effect.Class.Console (error)
import Effect.Timer (TimeoutId, clearTimeout)
import FFI.Simple (delay, (.=))
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Hooks.Sigmax.Types (Graph(..), Node(..), NodesMap, SelectedNodeIds)
import Gargantext.Utils.Reactix as R2
import Prelude (Unit, bind, const, discard, flip, pure, unit, ($), (*>), (<$), (<$>), (<<<), (<>), (>>=), not)
import Reactix as R

type Sigma =
  { sigma :: R.Ref (Maybe Sigma.Sigma)
    -- TODO is Seq in cleanup really necessary?
  , cleanup :: R.Ref (Seq (Effect Unit))
  }

type Data n e = { graph :: R.Ref (Graph n e) }

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
  *>  Sigma.clear sigma
  *>  log readingMsg
  *>  Sigma.graphRead sigma graph
  >>= either (log2 errorMsg) refresh
  where
    refresh _ = log refreshingMsg *> Sigma.refresh sigma
    clearingMsg = "[refreshData] Clearing existing graph data"
    readingMsg = "[refreshData] Reading graph data"
    refreshingMsg = "[refreshData] Refreshing graph"
    errorMsg = "[refreshData] Error reading graph data:"

sigmafy :: forall n e. Graph n e -> Sigma.Graph n e
sigmafy (Graph g) = {nodes,edges}
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
--handleForceAtlasPause sigmaRef (toggled /\ setToggled) mFAPauseRef = do
handleForceAtlas2Pause :: R.Ref Sigma -> R.State Boolean -> Boolean -> R.Ref (Maybe TimeoutId) -> Effect Unit
handleForceAtlas2Pause sigmaRef (toggled /\ setToggled) showEdges mFAPauseRef = do
  let sigma = R.readRef sigmaRef
  dependOnSigma sigma "[handleForceAtlas2Pause] sigma: Nothing" $ \s -> do
    --log2 "[handleForceAtlas2Pause] mSigma: Just " s
    --log2 "[handleForceAtlas2Pause] toggled: " toggled
    isFARunning <- Sigma.isForceAtlas2Running s
    --log2 "[handleForceAtlas2Pause] isFARunning: " isFARunning
    case Tuple toggled isFARunning of
      Tuple true false -> do
        -- hide edges during forceAtlas rendering, this prevents flickering
        Sigma.restartForceAtlas2 s
        setEdges s false
        case R.readRef mFAPauseRef of
          Nothing -> pure unit
          Just timeoutId -> clearTimeout timeoutId
      Tuple false true -> do
        -- restore edges state
        Sigma.stopForceAtlas2 s
        setEdges s showEdges
      _ -> pure unit
    -- handle case when user pressed pause/start fa button before timeout fired
    --case R.readRef mFAPauseRef of
    --  Nothing -> pure unit
    --  Just timeoutId -> do
    --    R.setRef mFAPauseRef Nothing
    --    clearTimeout timeoutId

setEdges :: Sigma.Sigma -> Boolean -> Effect Unit
setEdges sigma val = do
  let settings = {
        drawEdges: val
      , drawEdgeLabels: val
      , hideEdgesOnMove: not val
    }
  -- prevent showing edges (via show edges button) when FA is running (flickering)
  isFARunning <- Sigma.isForceAtlas2Running sigma
  case Tuple val isFARunning of
    Tuple false _ ->
      Sigma.setSettings sigma settings
    Tuple true false ->
      Sigma.setSettings sigma settings
    _ -> pure unit

markSelectedNodes :: Sigma.Sigma -> SelectedNodeIds -> NodesMap -> Effect Unit
markSelectedNodes sigma selectedNodeIds graphNodes = do
  Sigma.forEachNode sigma \n -> do
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


bindSelectedNodesClick :: R.Ref Sigma -> R.State SelectedNodeIds -> Effect Unit
bindSelectedNodesClick sigmaRef (_ /\ setSelectedNodeIds) =
  dependOnSigma (R.readRef sigmaRef) "[graphCpt] no sigma" $ \sigma ->
    Sigma.bindClickNode sigma $ \node -> do
      setSelectedNodeIds \nids ->
        if Set.member node.id nids then
          Set.delete node.id nids
        else
          Set.insert node.id nids
