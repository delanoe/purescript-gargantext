module Gargantext.Data.Array
  where

import Data.Array as DA
import Data.Array ((..))
import Data.List as List
import Data.Maybe
import Data.Sequence as Seq
import Data.Tuple (Tuple(..))
import Prelude

----------------------------------------------------------------------
-- | Split arrays tools
splitEvery :: forall a. Int -> Array a -> Array (Array a)
splitEvery _ [] = []
splitEvery n xs =
  let (Tuple h t) = splitAt n xs
  in DA.cons h (splitEvery n t)

splitAt :: forall a. Int -> Array a -> Tuple (Array a) (Array a)
splitAt n ls = Tuple (Seq.toUnfoldable x) (Seq.toUnfoldable xs)
  where
    Tuple x xs = Seq.splitAt n (Seq.fromFoldable ls)

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

----------------------------------------------------------------------
-- | Seq with Maybe tools
seqMapMaybe :: forall a b. (a -> Maybe b) -> Seq.Seq a -> Seq.Seq b
seqMapMaybe f = seqConcatMap (maybe Seq.empty Seq.singleton <<< f)

seqCatMaybes :: forall a. Seq.Seq (Maybe a) -> Seq.Seq a
seqCatMaybes = seqMapMaybe identity

----------------------------------------------------------------------
-- | Seq misc tools
seqConcatMap :: forall a b. (a -> Seq.Seq b) -> Seq.Seq a -> Seq.Seq b
seqConcatMap = flip bind

-- swap 2 array indices
swap :: forall a. Int -> Int -> Array a -> Array a
swap i j arr = DA.updateAtIndices updates arr
  where
    updates = case DA.index arr i of
      Nothing -> []
      Just iEl -> case DA.index arr j of
        Nothing -> []
        Just jEl -> [ Tuple i jEl, Tuple j iEl ]

swapList :: forall a. Int -> Int -> List.List a -> List.List a
swapList i j seq = List.fromFoldable $ swap i j $ List.toUnfoldable seq

swapSeq :: forall a. Int -> Int -> Seq.Seq a -> Seq.Seq a
swapSeq i j seq = Seq.fromFoldable $ swap i j $ Seq.toUnfoldable seq

slidingWindow :: forall a. Array a -> Int -> Array (Array a)
slidingWindow lst len =
  let diff = DA.length lst - len
  in
   if diff < 0 then []
   else (\idx -> DA.slice idx (idx + len) lst) <$> 0 .. diff
