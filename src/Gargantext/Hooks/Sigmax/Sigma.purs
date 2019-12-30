module Gargantext.Hooks.Sigmax.Sigma where

import Prelude
import Data.Either (Either(..))
import Data.Nullable (notNull, null, Nullable)
import Data.Set as Set
import DOM.Simple.Console (log2)
import DOM.Simple.Types (Element)
import FFI.Simple ((..))
import Effect (Effect, foreachE)
import Effect.Timer (setTimeout)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1, EffectFn2, runEffectFn2, EffectFn3, runEffectFn3, EffectFn4, runEffectFn4)
import Type.Row (class Union)
import Reactix as R

import Gargantext.Hooks.Sigmax.Types as Types

foreign import data Sigma :: Type

type NodeRequiredProps = ( id :: String )
type EdgeRequiredProps = ( id :: String, source :: String, target :: String )

class NodeProps (all :: #Type) (extra :: #Type) | all -> extra
class EdgeProps (all :: #Type) (extra :: #Type) | all -> extra

instance nodeProps
  :: Union NodeRequiredProps extra all
  => NodeProps all extra

instance edgeProps
  :: Union EdgeRequiredProps extra all
  => EdgeProps all extra
  
type Graph n e = { nodes :: Array {|n}, edges :: Array {|e} }
type SigmaOpts s = { settings :: s }

foreign import _sigma ::
  forall a b opts err.
  EffectFn3 (a -> Either a b)
            (b -> Either a b)
            (SigmaOpts opts)
            (Either err Sigma)
sigma :: forall opts err. SigmaOpts opts -> Effect (Either err Sigma)
sigma = runEffectFn3 _sigma Left Right

foreign import _graphRead ::
  forall a b data_ err.
  EffectFn4 (a -> Either a b)
            (b -> Either a b)
            Sigma
            data_
            (Either err Unit)
graphRead :: forall node edge err. Sigma -> Graph node edge -> Effect (Either err Unit)
graphRead = runEffectFn4 _graphRead Left Right

foreign import _refresh :: EffectFn1 Sigma Unit
refresh :: Sigma -> Effect Unit
refresh = runEffectFn1 _refresh

foreign import _addRenderer
  :: forall a b r err.
  EffectFn4 (a -> Either a b)
            (b -> Either a b)
            Sigma
            r
            (Either err Unit)
addRenderer :: forall r err. Sigma -> r -> Effect (Either err Unit)
addRenderer = runEffectFn4 _addRenderer Left Right

foreign import _bindMouseSelectorPlugin
  :: forall a b err.
  EffectFn3 (a -> Either a b)
            (b -> Either a b)
            Sigma
            (Either err Unit)
bindMouseSelectorPlugin :: forall err. Sigma -> Effect (Either err Unit)
bindMouseSelectorPlugin = runEffectFn3 _bindMouseSelectorPlugin Left Right

foreign import _killRenderer
  :: forall a b r err.
  EffectFn4 (a -> Either a b)
            (b -> Either a b)
            Sigma
            r
            (Either err Unit)
killRenderer :: forall r err. Sigma -> r -> Effect (Either err Unit)
killRenderer = runEffectFn4 _killRenderer Left Right

foreign import _getRendererContainer
  :: EffectFn1 Sigma Element
getRendererContainer :: Sigma -> Effect Element
getRendererContainer = runEffectFn1 _getRendererContainer

swapRendererContainer :: R.Ref (Nullable Element) -> Sigma -> Effect Unit
swapRendererContainer ref s = do
  el <- getRendererContainer s
  log2 "[swapRendererContainer] el" el
  R.setRef ref $ notNull el

foreign import _setRendererContainer
  :: EffectFn2 Sigma Element Unit
setRendererContainer :: Sigma -> Element -> Effect Unit
setRendererContainer = runEffectFn2 _setRendererContainer

foreign import _killSigma
  :: forall a b err.
  EffectFn3 (a -> Either a b)
            (b -> Either a b)
            Sigma
            (Either err Unit)
killSigma :: forall err. Sigma -> Effect (Either err Unit)
killSigma = runEffectFn3 _killSigma Left Right

foreign import _clear :: EffectFn1 Sigma Unit
clear :: Sigma -> Effect Unit
clear = runEffectFn1 _clear

foreign import _bind :: forall e. EffectFn3 Sigma String (EffectFn1 e Unit) Unit
bind_ :: forall e. Sigma -> String -> (e -> Effect Unit) -> Effect Unit
bind_ s e h = runEffectFn3 _bind s e (mkEffectFn1 h)

foreign import _unbind :: EffectFn2 Sigma String Unit
unbind_ :: Sigma -> String -> Effect Unit
unbind_ s e = runEffectFn2 _unbind s e

foreign import _edgeIds :: EffectFn1 Sigma (Array String)
sigmaEdgeIds :: Sigma -> Effect Types.SelectedEdgeIds
sigmaEdgeIds s =  do
  edgeIds <- runEffectFn1 _edgeIds s
  pure $ Set.fromFoldable edgeIds

foreign import _nodeIds :: EffectFn1 Sigma (Array String)
sigmaNodeIds :: Sigma -> Effect Types.SelectedNodeIds
sigmaNodeIds s = do
  nodeIds <- runEffectFn1 _nodeIds s
  pure $ Set.fromFoldable nodeIds

foreign import _addEdge :: EffectFn2 Sigma (Record Types.Edge) Unit
addEdge :: Sigma -> Record Types.Edge -> Effect Unit
addEdge s e = runEffectFn2 _addEdge s e
foreign import _removeEdge :: EffectFn2 Sigma String Unit
removeEdge :: Sigma -> String -> Effect Unit
removeEdge s eId = runEffectFn2 _removeEdge s eId

foreign import _addNode :: EffectFn2 Sigma (Record Types.Node) Unit
addNode :: Sigma -> Record Types.Node -> Effect Unit
addNode s n = runEffectFn2 _addNode s n
foreign import _removeNode :: EffectFn2 Sigma String Unit
removeNode :: Sigma -> String -> Effect Unit
removeNode s nId = runEffectFn2 _removeNode s nId

foreign import _forEachNode :: EffectFn2 Sigma (EffectFn1 (Record Types.Node) Unit) Unit
forEachNode :: Sigma -> (Record Types.Node -> Effect Unit) -> Effect Unit
forEachNode s f = runEffectFn2 _forEachNode s (mkEffectFn1 f)

foreign import _forEachEdge :: EffectFn2 Sigma (EffectFn1 (Record Types.Edge) Unit) Unit
forEachEdge :: Sigma -> (Record Types.Edge -> Effect Unit) -> Effect Unit
forEachEdge s f = runEffectFn2 _forEachEdge s (mkEffectFn1 f)

bindClickNode :: Sigma -> (Record Types.Node -> Effect Unit) -> Effect Unit
bindClickNode s f = bind_ s "clickNode" $ \e -> do
  let node = e .. "data" .. "node" :: Record Types.Node
  f node

unbindClickNode :: Sigma -> Effect Unit
unbindClickNode s = unbind_ s "clickNode"

bindClickNodes :: Sigma -> (Array (Record Types.Node) -> Effect Unit) -> Effect Unit
bindClickNodes s f = bind_ s "clickNodes" $ \e -> do
  let nodes = e .. "data" .. "node" :: Array (Record Types.Node)
  f nodes

unbindClickNodes :: Sigma -> Effect Unit
unbindClickNodes s = unbind_ s "clickNodes"

bindOverNode :: Sigma -> (Record Types.Node -> Effect Unit) -> Effect Unit
bindOverNode s f = bind_ s "overNode" $ \e -> do
  let node = e .. "data" .. "node" :: Record Types.Node
  f node

bindClickEdge :: Sigma -> (Record Types.Edge -> Effect Unit) -> Effect Unit
bindClickEdge s f = bind_ s "clickEdge" $ \e -> do
  let edge = e .. "data" .. "edge" :: Record Types.Edge
  f edge

unbindClickEdge :: Sigma -> Effect Unit
unbindClickEdge s = unbind_ s "clickEdge"

bindOverEdge :: Sigma -> (Record Types.Edge -> Effect Unit) -> Effect Unit
bindOverEdge s f = bind_ s "overEdge" $ \e -> do
  let edge = e .. "data" .. "edge" :: Record Types.Edge
  f edge

foreign import _setSettings :: forall settings. EffectFn2 Sigma settings Unit
setSettings :: forall settings. Sigma -> settings -> Effect Unit
setSettings s settings = do
  runEffectFn2 _setSettings s settings
  refresh s

startForceAtlas2 :: forall settings. Sigma -> settings -> Effect Unit
startForceAtlas2 = runEffectFn2 _startForceAtlas2

restartForceAtlas2 :: Sigma -> Effect Unit
restartForceAtlas2 s = runEffectFn2 _startForceAtlas2 s null

stopForceAtlas2 :: Sigma -> Effect Unit
stopForceAtlas2 = runEffectFn1 _stopForceAtlas2

killForceAtlas2 :: Sigma -> Effect Unit
killForceAtlas2 = runEffectFn1 _killForceAtlas2

isForceAtlas2Running :: Sigma -> Effect Boolean
isForceAtlas2Running = runEffectFn1 _isForceAtlas2Running

foreign import _startForceAtlas2 :: forall s. EffectFn2 Sigma s Unit
foreign import _stopForceAtlas2 :: EffectFn1 Sigma Unit
foreign import _killForceAtlas2 :: EffectFn1 Sigma Unit
foreign import _isForceAtlas2Running :: EffectFn1 Sigma Boolean

refreshForceAtlas :: Sigma -> Effect Unit
refreshForceAtlas s = do
  isRunning <- isForceAtlas2Running s
  if isRunning then
    pure unit
  else do
    _ <- setTimeout 100 $ do
      restartForceAtlas2 s
      _ <- setTimeout 100 $
        stopForceAtlas2 s
      pure unit
    pure unit

newtype SigmaEasing = SigmaEasing String

sigmaEasing :: { linear :: SigmaEasing
, quadraticIn :: SigmaEasing
, quadraticOut :: SigmaEasing
, quadraticInOut :: SigmaEasing
, cubicIn :: SigmaEasing
, cubicOut :: SigmaEasing
, cubicInOut :: SigmaEasing
}
sigmaEasing =
  { linear : SigmaEasing "linear"
  , quadraticIn : SigmaEasing "quadraticIn"
  , quadraticOut : SigmaEasing "quadraticOut"
  , quadraticInOut : SigmaEasing "quadraticInOut"
  , cubicIn : SigmaEasing "cubicIn"
  , cubicOut : SigmaEasing "cubicOut"
  , cubicInOut : SigmaEasing "cubicInOut"
  }

type CameraProps =
  ( x :: Number
  , y :: Number
  , ratio :: Number
  , angle :: Number
  )

foreign import data CameraInstance' :: # Type
type CameraInstance = { | CameraInstance' }

cameras :: Sigma -> Effect (Array CameraInstance)
cameras = runEffectFn1 _getCameras

foreign import _getCameras :: EffectFn1 Sigma (Array CameraInstance)

goTo :: Record CameraProps -> CameraInstance -> Effect Unit
goTo props cam = do
  runEffectFn2 _goTo cam props

foreign import _goTo :: EffectFn2 CameraInstance (Record CameraProps) Unit

goToAllCameras :: Sigma -> Record CameraProps -> Effect Unit
goToAllCameras s props = do
  cs <- cameras s
  foreachE cs (goTo props)
