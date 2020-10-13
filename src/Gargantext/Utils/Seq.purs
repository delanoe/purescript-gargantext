module Gargantext.Utils.Seq where

import Data.Array as Array
import Data.Maybe
import Data.Sequence
import Data.Tuple

import Gargantext.Prelude


reverse :: forall a. Seq a -> Seq a
reverse s = case uncons s of
  Nothing           -> empty
  Just (Tuple x xs) -> snoc (reverse xs) x


mapMaybe :: forall a b. (a -> Maybe b) -> Seq a -> Seq b
mapMaybe f = go empty
  where
    go acc s =
      case uncons s of
        Nothing           -> acc
        Just (Tuple x xs) ->
          case f x of
            Nothing -> go acc xs
            Just y  -> go (cons y acc) xs

-- same as
-- https://github.com/purescript/purescript-arrays/blob/v5.3.1/src/Data/Array.purs#L715-L715
sortWith :: forall a b. Ord b => (a -> b) -> Seq a -> Seq a
sortWith f l = Array.toUnfoldable $ Array.sortBy (comparing f) $ Array.fromFoldable l
