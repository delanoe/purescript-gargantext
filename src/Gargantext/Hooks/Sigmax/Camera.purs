module Gargantext.Hooks.Sigmax.Camera where

import Gargantext.Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import FFI.Simple ((..))
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Simple.JSON as JSON


foreign import data CameraInstance' :: Row Type
type CameraInstance = { | CameraInstance' }

type CameraRow =
  ( angle :: Number
  , ratio :: Number
  , x     :: Number
  , y     :: Number )

newtype Camera =
  Camera { |CameraRow }
derive instance Generic Camera _
derive instance Newtype Camera _
instance Eq Camera where eq = genericEq
derive newtype instance JSON.ReadForeign Camera
derive newtype instance JSON.WriteForeign Camera

defaultCamera :: Camera
defaultCamera = Camera { angle: 0.0
                       , ratio: 1.1
                       , x: 0.5
                       , y: 0.5 }


camera :: Sigma.Sigma -> CameraInstance
camera sig = sig .. "camera"

toCamera :: CameraInstance -> Camera
toCamera c = Camera { angle, ratio, x, y }
  where
    angle = c .. "angle" :: Number
    ratio = c .. "ratio" :: Number
    x = c .. "x" :: Number
    y = c .. "y" :: Number

updateCamera :: CameraInstance -> Camera -> Effect Unit
updateCamera cam (Camera { angle, ratio, x, y }) = runEffectFn2 _setState cam { angle, ratio, x, y }

foreign import _setState :: forall e. EffectFn2 CameraInstance {|CameraRow} Unit
