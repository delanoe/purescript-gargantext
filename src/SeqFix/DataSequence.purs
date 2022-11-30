module Data.SequenceArr where

import Prelude
import Data.Array as A
import Data.Functor as Functor
import Data.Tuple (Tuple(..))

type Seq = Array

cons = A.cons
length = A.length

concatMap = A.concatMap
drop = A.drop
filter = A.filter
map = Functor.map
take = A.take

fromFoldable = A.fromFoldable
toUnfoldable = A.toUnfoldable

empty = []
head = A.head
null = A.null
snoc = A.snoc
singleton = A.singleton
splitAt idx seq = Tuple s.before s.after
  where
    s = A.splitAt idx seq
