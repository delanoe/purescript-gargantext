module Gargantext.Utils.Seq (groupBy, mapMaybe) where

import Data.Array ((:))
import Data.Maybe (Maybe, maybe)
import Data.Sequence (Seq, concatMap, empty, null, singleton, splitAt)
import Data.Tuple (Tuple(..))

import Gargantext.Prelude ((<<<))

mapMaybe :: forall a b. (a -> Maybe b) -> Seq a -> Seq b
mapMaybe f = concatMap (maybe empty singleton <<< f)

-- | Group a given `Seq` into list of `Seq`'s of length `n` (last one can be shorter)
groupBy :: forall a. Int -> Seq a -> Array (Seq a)
groupBy n s =
  if null s
  then []
  else
    let (Tuple hd tl) = splitAt n s in
    (hd:groupBy n tl)
