module Gargantext.Hooks.Sigmax.ForceAtlas2 where

-- FFI for force atlas2: https://graphology.github.io/standard-library/layout-forceatlas2.html

import Prelude

import Data.Array as A
import Data.Sequence as Seq
import Data.Set as Set
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Timer (setTimeout)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import FFI.Simple ((..), (...), (.=))
import Gargantext.Hooks.Sigmax.Graphology as Graphology
import Gargantext.Hooks.Sigmax.Types as Types
import Record as Record

-- | Type representing the web worker.
foreign import data FA2Layout :: Type

-- TODO inferSettings
-- TODO init with settings
foreign import _init :: EffectFn1 Graphology.Graph FA2Layout
foreign import _start :: EffectFn1 FA2Layout Unit
foreign import _stop :: EffectFn1 FA2Layout Unit
foreign import _kill :: EffectFn1 FA2Layout Unit
foreign import _isRunning :: EffectFn1 FA2Layout Boolean

init :: Graphology.Graph -> Effect FA2Layout
init = runEffectFn1 _init

start :: FA2Layout -> Effect Unit
start = runEffectFn1 _start

stop :: FA2Layout -> Effect Unit
stop = runEffectFn1 _stop

kill :: FA2Layout -> Effect Unit
kill = runEffectFn1 _kill

isRunning :: FA2Layout -> Effect Boolean
isRunning = runEffectFn1 _isRunning

-- TODO?
restart :: FA2Layout -> Effect Unit
restart fa2 = do
  stop fa2
  _ <- setTimeout 100 $ do
    start fa2
  pure unit

refresh :: FA2Layout -> Effect Unit
refresh f = do
  isRunning' <- isRunning f
  if isRunning' then
    pure unit
  else do
    _ <- setTimeout 100 $ do
      restart f
      _ <- setTimeout 100 $ stop f
      pure unit
    pure unit
