module Gargantext.Hooks.Sigmax.Sigma where

import Prelude
import Data.Either (Either(..))
import Data.Nullable (notNull, null, Nullable)
import DOM.Simple.Console (log, log2)
import DOM.Simple.Types (Element)
import FFI.Simple (delay, (..))
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

sigma :: forall opts err. SigmaOpts opts -> Effect (Either err Sigma)
sigma = runEffectFn3 _sigma Left Right

foreign import _sigma ::
  forall a b opts err.
  EffectFn3 (a -> Either a b)
            (b -> Either a b)
            (SigmaOpts opts)
            (Either err Sigma)

graphRead :: forall node edge err. Sigma -> Graph node edge -> Effect (Either err Unit)
graphRead = runEffectFn4 _graphRead Left Right

foreign import _graphRead ::
  forall a b data_ err.
  EffectFn4 (a -> Either a b)
            (b -> Either a b)
            Sigma
            data_
            (Either err Unit)

refresh :: Sigma -> Effect Unit
refresh = runEffectFn1 _refresh

foreign import _refresh :: EffectFn1 Sigma Unit

addRenderer :: forall r err. Sigma -> r -> Effect (Either err Unit)
addRenderer = runEffectFn4 _addRenderer Left Right

foreign import _addRenderer
  :: forall a b r err.
  EffectFn4 (a -> Either a b)
            (b -> Either a b)
            Sigma 
            r
            (Either err Unit)

killRenderer :: forall r err. Sigma -> r -> Effect (Either err Unit)
killRenderer = runEffectFn4 _killRenderer Left Right

foreign import _killRenderer
  :: forall a b r err.
  EffectFn4 (a -> Either a b)
            (b -> Either a b)
            Sigma 
            r
            (Either err Unit)

getRendererContainer :: Sigma -> Effect Element
getRendererContainer sigma = runEffectFn1 _getRendererContainer sigma

foreign import _getRendererContainer
  :: EffectFn1 Sigma Element

swapRendererContainer :: R.Ref (Nullable Element) -> Sigma -> Effect Unit
swapRendererContainer ref sigma = do
  el <- getRendererContainer sigma
  log2 "[swapRendererContainer] el" el
  R.setRef ref $ notNull el

setRendererContainer :: Sigma -> Element -> Effect Unit
setRendererContainer sigma el = runEffectFn2 _setRendererContainer sigma el

foreign import _setRendererContainer
  :: EffectFn2 Sigma Element Unit

killSigma :: forall err. Sigma -> Effect (Either err Unit)
killSigma = runEffectFn3 _killSigma Left Right

clear :: Sigma -> Effect Unit
clear = runEffectFn1 _clear

foreign import _clear :: EffectFn1 Sigma Unit

foreign import _killSigma
  :: forall a b err.
  EffectFn3 (a -> Either a b)
            (b -> Either a b)
            Sigma 
            (Either err Unit)

bind_ :: forall e. Sigma -> String -> (e -> Effect Unit) -> Effect Unit
bind_ s e h = runEffectFn3 _bind s e (mkEffectFn1 h)

foreign import _bind :: forall e. EffectFn3 Sigma String (EffectFn1 e Unit) Unit

unbind_ :: forall e. Sigma -> String -> Effect Unit
unbind_ s e = runEffectFn2 _unbind s e

foreign import _unbind :: forall e. EffectFn2 Sigma String Unit

forEachNode :: Sigma -> (Record Types.Node -> Effect Unit) -> Effect Unit
forEachNode s f = runEffectFn2 _forEachNode s (mkEffectFn1 f)

foreign import _forEachNode :: EffectFn2 Sigma (EffectFn1 (Record Types.Node) Unit) Unit

forEachEdge :: Sigma -> (Record Types.Edge -> Effect Unit) -> Effect Unit
forEachEdge s f = runEffectFn2 _forEachEdge s (mkEffectFn1 f)

foreign import _forEachEdge :: EffectFn2 Sigma (EffectFn1 (Record Types.Edge) Unit) Unit

bindClickNode :: Sigma -> (Record Types.Node -> Effect Unit) -> Effect Unit
bindClickNode sigma f = bind_ sigma "clickNode" $ \e -> do
  let node = e .. "data" .. "node" :: Record Types.Node
  f node

unbindClickNode :: Sigma -> Effect Unit
unbindClickNode sigma = unbind_ sigma "clickNode"

setSettings :: forall settings. Sigma -> settings -> Effect Unit
setSettings sigma settings = do
  runEffectFn2 _setSettings sigma settings
  refresh sigma

foreign import _setSettings :: forall settings. EffectFn2 Sigma settings Unit

startForceAtlas2 :: forall settings. Sigma -> settings -> Effect Unit
startForceAtlas2 = runEffectFn2 _startForceAtlas2

restartForceAtlas2 :: Sigma -> Effect Unit
restartForceAtlas2 sigma = runEffectFn2 _startForceAtlas2 sigma null

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
refreshForceAtlas sigma = do
  isRunning <- isForceAtlas2Running sigma
  if isRunning then
    pure unit
  else do
    _ <- setTimeout 100 $ do
      restartForceAtlas2 sigma
      _ <- setTimeout 100 $
        stopForceAtlas2 sigma
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
goToAllCameras sigma props = do
  cs <- cameras sigma
  foreachE cs (goTo props)
