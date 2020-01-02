module Gargantext.Hooks.Sigmax.Sigma where

import Prelude

import DOM.Simple.Types (Element)
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Nullable (null)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Exception as EEx
import Effect.Timer (setTimeout)
import Effect.Uncurried (EffectFn1, EffectFn3, EffectFn4, mkEffectFn1, runEffectFn3, runEffectFn4)
import FFI.Simple ((..), (...), (.=))
import Foreign.Object as Object
import Gargantext.Hooks.Sigmax.Types as Types
import Type.Row (class Union)

-- | Type representing a sigmajs instance
foreign import data Sigma :: Type
-- | Type representing `sigma.graph`
foreign import data SigmaGraph :: Type

type NodeRequiredProps = ( id :: Types.NodeId )
type EdgeRequiredProps = ( id :: Types.EdgeId, source :: Types.NodeId, target :: Types.NodeId )

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

-- | Initialize sigmajs.
sigma :: forall opts err. SigmaOpts opts -> Effect (Either err Sigma)
sigma = runEffectFn3 _sigma Left Right

-- | Call the `refresh()` method on a sigmajs instance.
refresh :: Sigma -> Effect Unit
refresh s = pure $ s ... "refresh" $ []

-- | Type representing a sigmajs renderer.
foreign import data Renderer :: Type
type RendererType = String

--makeRenderer :: forall props. RendererType -> Element -> props -> Renderer
--makeRenderer type_ container props =
--  {
--    "type": type_
--  , container
--  | props
--  }

-- | Call the `addRenderer` method on a sigmajs instance.
--addRenderer :: forall err. Sigma -> Renderer -> Effect (Either err Unit)
addRenderer :: forall r err. Sigma -> r -> Effect (Either err Unit)
addRenderer = runEffectFn4 _addRenderer Left Right

-- | Initialize the mouse selector plugin. This allows for custom bindings to mouse events.
bindMouseSelectorPlugin :: forall err. Sigma -> Effect (Either err Unit)
bindMouseSelectorPlugin = runEffectFn3 _bindMouseSelectorPlugin Left Right

-- | Call `killRenderer` on a sigmajs instance.
killRenderer :: forall r. Sigma -> r -> Effect (Either EEx.Error Unit)
killRenderer s r = EEx.try $ pure $ s ... "killRenderer" $ [ r ]

-- | Get `renderers` of a sigmajs instance.
renderers :: Sigma -> Array Renderer
renderers s = s .. "renderers" :: Array Renderer

-- | Get the `container` of a sigmajs renderer.
rendererContainer :: Renderer -> Element
rendererContainer r = r .. "container"

-- | Return the container of first renderer in sigmajs instance's `renderers` list.
getRendererContainer :: Sigma -> Maybe Element
getRendererContainer s = rendererContainer <$> mContainer
  where
    mContainer = A.head $ renderers s

-- | Set the container of first renderer in sigmajs instance's `renderers` list.
setRendererContainer :: Renderer -> Element -> Effect Unit
setRendererContainer r el = do
  _ <- pure $ (r .= "container") el
  pure unit

-- | Call the `kill()` method on a sigmajs instance.
killSigma :: Sigma -> Effect (Either EEx.Error Unit)
killSigma s = EEx.try $ pure $ s ... "kill" $ []

-- | Get the `.graph` object from a sigmajs instance.
graph :: Sigma -> SigmaGraph
graph s = s .. "graph" :: SigmaGraph

-- | Read graph into a sigmajs instance.
graphRead :: forall nodeExtra node edgeExtra edge. NodeProps nodeExtra node => EdgeProps edgeExtra edge => SigmaGraph -> Graph node edge -> Effect (Either EEx.Error Unit)
graphRead sg g = EEx.try $ pure $ sg ... "read" $ [ g ]

-- | Clear a sigmajs graph.
clear :: SigmaGraph -> Effect Unit
clear sg = pure $ sg ... "clear" $ []

-- | Call `sigma.bind(event, handler)` on a sigmajs instance.
bind_ :: forall e. Sigma -> String -> (e -> Effect Unit) -> Effect Unit
bind_ s e h = runEffectFn3 _bind s e (mkEffectFn1 h)

-- | Generic function to bind a sigmajs event for edges.
bindEdgeEvent :: Sigma -> String -> (Record Types.Edge -> Effect Unit) -> Effect Unit
bindEdgeEvent s ev f = bind_ s ev $ \e -> do
  let edge = e .. "data" .. "edge" :: Record Types.Edge
  f edge
-- | Generic function to bind a sigmajs event for nodes.
bindNodeEvent :: Sigma -> String -> (Record Types.Node -> Effect Unit) -> Effect Unit
bindNodeEvent s ev f = bind_ s ev $ \e -> do
  let node = e .. "data" .. "node" :: Record Types.Node
  f node

-- | Call `sigma.unbind(event)` on a sigmajs instance.
unbind_ :: Sigma -> String -> Effect Unit
unbind_ s e = pure $ s ... "unbind" $ [e]

edges_ :: SigmaGraph -> Array (Record Types.Edge)
edges_ sg = sg ... "edges" $ [] :: Array (Record Types.Edge)
nodes_ :: SigmaGraph -> Array (Record Types.Node)
nodes_ sg = sg ... "nodes" $ [] :: Array (Record Types.Node)

-- | Call `sigmaGraph.edges()` on a sigmajs graph instance.
edges :: SigmaGraph -> Seq.Seq (Record Types.Edge)
edges = Seq.fromFoldable <<< edges_
-- | Call `sigmaGraph.nodes()` on a sigmajs graph instance.
nodes :: SigmaGraph -> Seq.Seq (Record Types.Node)
nodes = Seq.fromFoldable <<< nodes_

-- | Fetch ids of graph edges in a sigmajs instance.
sigmaEdgeIds :: SigmaGraph -> Types.SelectedEdgeIds
sigmaEdgeIds sg =  Set.fromFoldable edgeIds
  where
    edgeIds = _.id <$> edges sg

-- | Fetch ids of graph nodes in a sigmajs instance.
sigmaNodeIds :: SigmaGraph -> Types.SelectedNodeIds
sigmaNodeIds sg = Set.fromFoldable nodeIds
  where
    nodeIds = _.id <$> nodes sg

-- | Call `addEdge` on a sigmajs graph.
addEdge :: SigmaGraph -> Record Types.Edge -> Effect Unit
addEdge sg e = pure $ sg ... "addEdge" $ [e]
-- | Call `removeEdge` on a sigmajs graph.
removeEdge :: SigmaGraph -> String -> Effect Unit
removeEdge sg eId = pure $ sg ... "dropEdge" $ [eId]
--removeEdge = runEffectFn2 _removeEdge

-- | Call `addNode` on a sigmajs graph.
addNode :: SigmaGraph -> Record Types.Node -> Effect Unit
addNode sg n = pure $ sg ... "addNode" $ [n]
-- | Call `removeNode` on a sigmajs graph.
removeNode :: SigmaGraph -> String -> Effect Unit
removeNode sg nId = pure $ sg ... "dropNode" $ [nId]

-- | Iterate over all edges in a sigmajs graph.
forEachEdge :: SigmaGraph -> (Record Types.Edge -> Effect Unit) -> Effect Unit
forEachEdge sg f = traverse_ f (edges sg)
-- | Iterate over all nodes in a sigmajs graph.
forEachNode :: SigmaGraph -> (Record Types.Node -> Effect Unit) -> Effect Unit
forEachNode sg f = traverse_ f (nodes sg)

-- | Bind a `clickNode` event.
bindClickNode :: Sigma -> (Record Types.Node -> Effect Unit) -> Effect Unit
bindClickNode s f = bindNodeEvent s "clickNode" f
-- | Unbind a `clickNode` event.
unbindClickNode :: Sigma -> Effect Unit
unbindClickNode s = unbind_ s "clickNode"

-- | Bind a `clickNodes` event.
bindClickNodes :: Sigma -> (Array (Record Types.Node) -> Effect Unit) -> Effect Unit
bindClickNodes s f = bind_ s "clickNodes" $ \e -> do
  let ns = e .. "data" .. "node" :: Array (Record Types.Node)
  f ns
-- | Unbind a `clickNodes` event.
unbindClickNodes :: Sigma -> Effect Unit
unbindClickNodes s = unbind_ s "clickNodes"

-- | Bind a `overNode` event.
bindOverNode :: Sigma -> (Record Types.Node -> Effect Unit) -> Effect Unit
bindOverNode s f = bindNodeEvent s "overNode" f

-- | Bind a `clickEdge` event.
bindClickEdge :: Sigma -> (Record Types.Edge -> Effect Unit) -> Effect Unit
bindClickEdge s f = bindEdgeEvent s "clickEdge" f
-- | Unbind a `clickEdge` event.
unbindClickEdge :: Sigma -> Effect Unit
unbindClickEdge s = unbind_ s "clickEdge"

-- | Bind a `overEdge` event.
bindOverEdge :: Sigma -> (Record Types.Edge -> Effect Unit) -> Effect Unit
bindOverEdge s f = bindEdgeEvent s "overEdge" f

-- | Call `settings(s)` on a sigmajs instance.
setSettings :: forall settings. Sigma -> settings -> Effect Unit
setSettings s settings = do
  _ <- pure $ s ... "settings" $ [ settings ]
  refresh s

-- | Start forceAtlas2 on a sigmajs instance.
startForceAtlas2 :: forall settings. Sigma -> settings -> Effect Unit
startForceAtlas2 s settings = pure $ s ... "startForceAtlas2" $ [ settings ]

-- | Restart forceAtlas2 on a sigmajs instance.
restartForceAtlas2 :: Sigma -> Effect Unit
restartForceAtlas2 s = startForceAtlas2 s null

-- | Stop forceAtlas2 on a sigmajs instance.
stopForceAtlas2 :: Sigma -> Effect Unit
stopForceAtlas2 s = pure $ s ... "stopForceAtlas2" $ []

-- | Kill forceAtlas2 on a sigmajs instance.
killForceAtlas2 :: Sigma -> Effect Unit
killForceAtlas2 s = pure $ s ... "killForceAtlas2" $ []

-- | Return whether forceAtlas2 is running on a sigmajs instance.
isForceAtlas2Running :: Sigma -> Boolean
isForceAtlas2Running s = s ... "isForceAtlas2Running" $ [] :: Boolean

-- | Refresh forceAtlas2 (with a `setTimeout` hack as it seems it doesn't work
-- | otherwise).
refreshForceAtlas :: Sigma -> Effect Unit
refreshForceAtlas s = do
  let isRunning = isForceAtlas2Running s
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

sigmaEasing ::
  { linear :: SigmaEasing
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

-- | Get an array of a sigma instance's `cameras`.
cameras :: Sigma -> Array CameraInstance
cameras s = Object.values cs
  where
    -- For some reason, `sigma.cameras` is an object with integer keys.
    cs = s .. "cameras" :: Object.Object CameraInstance

goTo :: Record CameraProps -> CameraInstance -> Effect Unit
goTo props cam = pure $ cam ... "goTo" $ [props]

goToAllCameras :: Sigma -> Record CameraProps -> Effect Unit
goToAllCameras s props = traverse_ (goTo props) $ cameras s

-- | FFI
foreign import _sigma ::
  forall a b opts err.
  EffectFn3 (a -> Either a b)
            (b -> Either a b)
            (SigmaOpts opts)
            (Either err Sigma)
foreign import _addRenderer
  :: forall a b r err.
  EffectFn4 (a -> Either a b)
            (b -> Either a b)
            Sigma
            r
            (Either err Unit)
foreign import _bindMouseSelectorPlugin
  :: forall a b err.
  EffectFn3 (a -> Either a b)
            (b -> Either a b)
            Sigma
            (Either err Unit)
foreign import _bind :: forall e. EffectFn3 Sigma String (EffectFn1 e Unit) Unit
