module Gargantext.Utils.Array (
    push
  , range) where

import Data.Array as A
import Data.Int as DI
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)

import Gargantext.Prelude

foreign import _push :: forall a. EffectFn2 (Array a) a Unit

push :: forall a. Array a -> a -> Effect Unit
push = runEffectFn2 _push


-- | Create an array containing a range of integers, with given step
range :: Int -> Int -> Int -> Array Int
range start end step = map (\i -> start + i*step) $ A.range 0 end'
  where
    end' = DI.floor $ (DI.toNumber $ end - start) / (DI.toNumber step)
