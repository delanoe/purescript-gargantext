module Gargantext.Hooks.Sigmax.Sigma where

import Prelude

import DOM.Simple.Types (Element, Window)
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Exception as EEx
import Effect.Timer (setTimeout)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, mkEffectFn1, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import FFI.Simple ((..), (...), (.=))
import Foreign.Object as Object
import Gargantext.Hooks.Sigmax.Graphology as Graphology
import Gargantext.Hooks.Sigmax.Types as Types
import Type.Row (class Union)

-- | Type representing a sigmajs instance
foreign import data Sigma :: Type

type NodeRequiredProps = ( id :: Types.NodeId )
type EdgeRequiredProps = ( id :: Types.EdgeId, source :: Types.NodeId, target :: Types.NodeId )

class NodeProps (all :: Row Type) (extra :: Row Type) | all -> extra
class EdgeProps (all :: Row Type) (extra :: Row Type) | all -> extra

instance nodeProps
  :: Union NodeRequiredProps extra all
  => NodeProps all extra

instance edgeProps
  :: Union EdgeRequiredProps extra all
  => EdgeProps all extra

type SigmaOpts s = { settings :: s }

-- | Initialize sigmajs.
sigma :: forall opts err. Element -> SigmaOpts opts -> Effect (Either err Sigma)
sigma = runEffectFn4 _sigma Left Right

-- | Kill a sigmajs instance.
kill :: Sigma -> Effect Unit
kill s = pure $ s ... "kill" $ []

-- | Call the `refresh()` method on a sigmajs instance.
refresh :: Sigma -> Effect Unit
refresh = runEffectFn1 _refresh
--refresh s = pure $ s ... "refresh" $ []

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
graph :: Sigma -> Graphology.Graph
graph s = s ... "getGraph" $ [] :: Graphology.Graph

-- | Call `sigma.bind(event, handler)` on a sigmajs instance.
on_ :: forall e. Sigma -> String -> (e -> Effect Unit) -> Effect Unit
on_ s e h = runEffectFn3 _on s e (mkEffectFn1 h)

-- | Generic function to bind a sigmajs event for edges.
bindEdgeEvent :: Sigma -> String -> (Record Types.Edge -> Effect Unit) -> Effect Unit
bindEdgeEvent s ev f = on_ s ev $ \e -> do
  let edge = e .. "data" .. "edge" :: Record Types.Edge
  f edge
-- | Generic function to bind a sigmajs event for nodes.
bindNodeEvent :: Sigma -> String -> (Record Types.Node -> Effect Unit) -> Effect Unit
bindNodeEvent s ev f = on_ s ev $ \e -> do
  let node = e .. "data" .. "node" :: Record Types.Node
  f node

-- | Call `sigma.unbind(event)` on a sigmajs instance.
unbind_ :: Sigma -> String -> Effect Unit
unbind_ s e = pure $ s ... "unbind" $ [e]

-- | Bind a `clickNode` event.
bindClickNode :: Sigma -> (Record Types.Node -> Effect Unit) -> Effect Unit
bindClickNode s f = bindNodeEvent s "clickNode" f
-- | Unbind a `clickNode` event.
unbindClickNode :: Sigma -> Effect Unit
unbindClickNode s = unbind_ s "clickNode"

-- | Bind a `clickNodes` event.
bindClickNodes :: Sigma -> (Array Types.NodeId -> Effect Unit) -> Effect Unit
bindClickNodes s f = on_ s "clickNodes" $ \e -> do
  let ns = e .. "nodeIds" :: Array Types.NodeId
  f ns
-- | Unbind a `clickNodes` event.
unbindClickNodes :: Sigma -> Effect Unit
unbindClickNodes s = unbind_ s "clickNodes"

-- | Shift + mousewheel changes selector size
bindShiftWheel :: Sigma -> (Number -> Effect Unit) -> Effect Unit
bindShiftWheel s f = on_ s "shiftWheel" $ \e -> do
  let delta = e .. "delta" :: Number
  f delta
unbindShiftWheel :: Sigma -> Effect Unit
unbindShiftWheel s = unbind_ s "shiftWheel"

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
setSettings = runEffectFn2 _setSettings

-- | Call `settins(s)` on the the main proxy `window.sigma`
proxySetSettings :: forall settings.
  Window -> Sigma -> settings -> Effect Unit
proxySetSettings = runEffectFn3 _proxySetSettings

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

foreign import data CameraInstance' :: Row Type
type CameraInstance = { | CameraInstance' }

-- | Get an array of a sigma instance's `cameras`.
cameras :: Sigma -> Array CameraInstance
cameras s = Object.values cs
  where
    -- For some reason, `sigma.cameras` is an object with integer keys.
    cs = s .. "cameras" :: Object.Object CameraInstance

toCamera :: CameraInstance -> Record CameraProps
toCamera c = { angle, ratio, x, y }
  where
    angle = c .. "angle" :: Number
    ratio = c .. "ratio" :: Number
    x = c .. "x" :: Number
    y = c .. "y" :: Number

updateCamera :: Sigma -> { ratio :: Number, x :: Number, y :: Number } -> Effect Unit
updateCamera sig { ratio, x, y } = do
  let camera = sig .. "camera"
  _ <- pure $ (camera .= "ratio") ratio
  _ <- pure $ (camera .= "x") x
  _ <- pure $ (camera .= "y") y
  pure unit

goTo :: Record CameraProps -> CameraInstance -> Effect Unit
goTo props cam = pure $ cam ... "goTo" $ [props]

goToAllCameras :: Sigma -> Record CameraProps -> Effect Unit
goToAllCameras s props = traverse_ (goTo props) $ cameras s

takeScreenshot :: Sigma -> Effect String
takeScreenshot =  runEffectFn1 _takeScreenshot

getEdges :: Sigma -> Effect (Array (Record Types.Edge))
getEdges = runEffectFn1 _getEdges

getNodes :: Sigma -> Effect (Array (Record Types.Node))
getNodes = runEffectFn1 _getNodes

-- | FFI
foreign import _sigma ::
  forall a b opts err.
  EffectFn4 (a -> Either a b)
            (b -> Either a b)
            Element
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
foreign import _on :: forall e. EffectFn3 Sigma String (EffectFn1 e Unit) Unit
foreign import _takeScreenshot :: EffectFn1 Sigma String
foreign import _getEdges :: EffectFn1 Sigma (Array (Record Types.Edge))
foreign import _getNodes :: EffectFn1 Sigma (Array (Record Types.Node))
foreign import _proxySetSettings
  :: forall settings.
  EffectFn3 Window
            Sigma
            settings
            Unit
foreign import _setSettings :: forall settings. EffectFn2 Sigma settings Unit
foreign import _refresh :: EffectFn1 Sigma Unit
