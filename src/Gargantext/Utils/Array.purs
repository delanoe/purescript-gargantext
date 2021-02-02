module Gargantext.Utils.Array (max, min, push) where

import Data.Array as A
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.Ord as Ord
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)

import Gargantext.Prelude

foreign import _push :: forall a. EffectFn2 (Array a) a Unit

push :: forall a. Array a -> a -> Effect Unit
push = runEffectFn2 _push


max :: forall a. Ord a => Array a -> Maybe a
max xs = foldr reducer (A.head xs) xs
  where
    reducer _ Nothing = Nothing
    reducer v (Just acc) = Just $ Ord.max acc v

min :: forall a. Ord a => Array a -> Maybe a
min xs = foldr reducer (A.head xs) xs
  where
    reducer _ Nothing = Nothing
    reducer v (Just acc) = Just $ Ord.min acc v
