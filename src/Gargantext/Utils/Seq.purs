module Gargantext.Utils.Seq (mapMaybe) where

import Data.Maybe (Maybe, maybe)
import Data.Sequence (Seq, concatMap, empty, singleton)

import Gargantext.Prelude ((<<<))

mapMaybe :: forall a b. (a -> Maybe b) -> Seq a -> Seq b
mapMaybe f = concatMap (maybe empty singleton <<< f)
