module Gargantext.Hooks.Sigmax
  -- (
  -- )
  where

import Prelude
import Data.Array as A
import Data.Bitraversable (bitraverse)
import Data.Either (Either(..), either)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, null)
import Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Traversable (for, for_, traverse, traverse_)
import DOM.Simple.Types (Element)
import DOM.Simple.Console (log, log2)
import Effect (Effect)
import FFI.Simple (delay)
import Reactix as R
import Reactix.DOM.HTML as RH
import Gargantext.Utils.Reactix as R2
import Gargantext.Types (class Optional)
import Gargantext.Hooks.Sigmax.Sigma (SigmaOpts)
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Hooks.Sigmax.Types

type Sigma =
  { sigma :: R.Ref (Maybe Sigma.Sigma)
  , cleanup :: R.Ref (Seq (Effect Unit))
  }

type Data n e = { graph :: R.Ref (Graph n e) }

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

-- | Manages a sigma with the given settings
useSigma :: forall settings. R.Ref (Nullable Element) -> settings -> R.Hooks Sigma
useSigma container settings = do
  sigma <- newSigma <$> R2.nothingRef <*> R.useRef Seq.empty
  R.useEffect2 container sigma.sigma $
    delay unit $ handleSigma sigma (readSigma sigma)
  pure sigma
  where
    newSigma sigma cleanup = { sigma, cleanup }
    handleSigma sigma (Just _) _ = pure R.nothing
    handleSigma sigma Nothing _ = do
      ret <- createSigma settings
      traverse_ (writeSigma sigma <<< Just) ret
      pure (cleanupSigma sigma "useSigma")

-- | Manages a renderer for the sigma
useCanvasRenderer :: R.Ref (Nullable Element) -> Sigma -> R.Hooks Unit
useCanvasRenderer container sigma =
  R.useEffect2' container sigma.sigma $
    delay unit $ \_ ->
      dependOnContainer container containerNotFoundMsg withContainer
  where
    withContainer c = dependOnSigma sigma sigmaNotFoundMsg withSigma
      where -- close over c
        withSigma sig = addRenderer sig renderer >>= handle
          where -- close over sig
            renderer = { "type": "canvas", container: c }
            handle (Right _) = cleanupFirst sigma (Sigma.killRenderer sig renderer >>= logCleanup)
            handle (Left e) =
              log2 errorAddingMsg e *> cleanupSigma sigma "useCanvasRenderer"
    logCleanup (Left e) = log2 errorKillingMsg e
    logCleanup _ = log killedMsg
    containerNotFoundMsg = "[useCanvasRenderer] Container not found, not adding renderer"
    sigmaNotFoundMsg     = "[useCanvasRenderer] Sigma not found, not adding renderer"
    errorAddingMsg       = "[useCanvasRenderer] Error adding canvas renderer: "
    errorKillingMsg      = "[useCanvasRenderer] Error killing renderer:"
    killedMsg            = "[useCanvasRenderer] Killed renderer"

createSigma :: forall settings err. settings -> Effect (Either err Sigma.Sigma)
createSigma settings = do
  ret <- Sigma.sigma {settings}
  ret <$ logStatus ret
  where
    logStatus (Left err) = log2 "[useSigma] Error during sigma creation:" err
    logStatus (Right x) = log2 "[useSigma] Initialised sigma successfully:" x

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

addRenderer :: forall r err. Sigma.Sigma -> r -> Effect (Either err Unit)
addRenderer sigma renderer = do
  ret <- Sigma.addRenderer sigma renderer
  (const unit <$> ret) <$ report ret
  where
    report = either (log2 errorMsg) (\_ -> log successMsg)
    errorMsg = "[useRenderer] Error adding renderer:"
    successMsg = "[useRenderer] Added renderer successfully"

useData :: forall n e. Sigma -> Graph n e -> R.Hooks Unit
useData sigma graph =
  R.useEffect2' sigma.sigma graph $
    delay unit $ \_ -> dependOnSigma sigma sigmaNotFoundMsg withSigma
  where
    withSigma sig = refreshData sig (sigmafy graph)
    sigmaNotFoundMsg = "[useData] Sigma not found, not adding data"

refreshData :: forall n e. Sigma.Sigma -> Sigma.Graph n e -> Effect Unit
refreshData sigma graph
  =   log clearingMsg
  *>  Sigma.clear sigma
  *>  log readingMsg
  *>  Sigma.graphRead sigma graph
  >>= either (log2 errorMsg) refresh
  where
    refresh _ = log refreshingMsg *> Sigma.refresh sigma
    clearingMsg = "[useData] Clearing existing graph data"
    readingMsg = "[useData] Reading graph data"
    refreshingMsg = "[useData] Refreshing graph"
    errorMsg = "[useData] Error reading graph data:"

sigmafy :: forall n e. Graph n e -> Sigma.Graph n e
sigmafy (Graph g) = {nodes,edges}
  where
    nodes = A.fromFoldable g.nodes
    edges = A.fromFoldable g.edges

useForceAtlas2 :: forall settings. Sigma -> settings -> R.Hooks Unit
useForceAtlas2 sigma settings =
  R.useEffect1' sigma.sigma (delay unit effect)
  where
    effect _ = dependOnSigma sigma sigmaNotFoundMsg withSigma
    withSigma sig = do
      log startingMsg
      log sigma
      Sigma.startForceAtlas2 sig settings
      cleanupFirst sigma (Sigma.killForceAtlas2 sig)
    startingMsg = "[Graph] Starting ForceAtlas2"
    sigmaNotFoundMsg = "[Graph] Sigma not found, not initialising"

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

