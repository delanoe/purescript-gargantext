module Gargantext.Data.Array
  where


import Data.Tuple (Tuple(..))
import Data.Array as DA
import Data.Maybe as DM
import Data.Sequence as DS


splitEvery :: forall a. Int -> Array a -> Array (Array a)
splitEvery _ [] = []
splitEvery n xs =
  let (Tuple h t) = splitAt n xs
  in DA.cons h (splitEvery n t)

splitAt :: forall a. Int -> Array a -> Tuple (Array a) (Array a)
splitAt n ls = Tuple (DS.toUnfoldable x) (DS.toUnfoldable xs)
  where
    Tuple x xs = DS.splitAt n (DS.fromFoldable ls)

