module Gargantext.Hooks.Sigmax.Noverlap where

-- FFI for noverlap: https://graphology.github.io/standard-library/layout-noverlap.html

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import FFI.Simple ((..))
import Gargantext.Hooks.Sigmax.Graphology as Graphology
import Prelude


-- | Type representing the web worker.
foreign import data NoverlapLayout :: Type

graph :: NoverlapLayout -> Graphology.Graph
graph s = s .. "graph" :: Graphology.Graph

foreign import _init :: forall settings. EffectFn2 Graphology.Graph settings NoverlapLayout
foreign import _start :: EffectFn1 NoverlapLayout Unit
foreign import _stop :: EffectFn1 NoverlapLayout Unit
foreign import _kill :: EffectFn1 NoverlapLayout Unit
foreign import _isRunning :: EffectFn1 NoverlapLayout Boolean

init :: forall settings. Graphology.Graph -> settings -> Effect NoverlapLayout
init = runEffectFn2 _init

start :: NoverlapLayout -> Effect Unit
start = runEffectFn1 _start

stop :: NoverlapLayout -> Effect Unit
stop = runEffectFn1 _stop

kill :: NoverlapLayout -> Effect Unit
kill = runEffectFn1 _kill

isRunning :: NoverlapLayout -> Effect Boolean
isRunning = runEffectFn1 _isRunning
