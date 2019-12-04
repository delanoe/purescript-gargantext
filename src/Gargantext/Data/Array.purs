module Gargantext.Data.Array
  where

import Data.Array as DA
import Data.Maybe
import Data.Sequence as DS
import Data.Tuple (Tuple(..))
import Prelude (bind, flip, identity, (<<<))

----------------------------------------------------------------------
-- | Split arrays tools
splitEvery :: forall a. Int -> Array a -> Array (Array a)
splitEvery _ [] = []
splitEvery n xs =
  let (Tuple h t) = splitAt n xs
  in DA.cons h (splitEvery n t)

splitAt :: forall a. Int -> Array a -> Tuple (Array a) (Array a)
splitAt n ls = Tuple (DS.toUnfoldable x) (DS.toUnfoldable xs)
  where
    Tuple x xs = DS.splitAt n (DS.fromFoldable ls)

----------------------------------------------------------------------
-- | Array with Maybe tools
mapMaybe :: forall a b. (a -> Maybe b) -> Array a -> Array b
mapMaybe f = concatMap (maybe [] singleton <<< f)

catMaybes :: forall a. Array (Maybe a) -> Array a
catMaybes = mapMaybe identity

----------------------------------------------------------------------
-- | Array misc tools
concatMap :: forall a b. (a -> Array b) -> Array a -> Array b
concatMap = flip bind

singleton :: forall a. a -> Array a
singleton a = [a]


