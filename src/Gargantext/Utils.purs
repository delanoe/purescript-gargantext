module Gargantext.Utils where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set as Set
import Data.Set (Set)

setterv :: forall nt record field. Newtype nt record => (record -> field -> record) -> field -> nt -> nt
setterv fn v t = (setter (flip fn v) t)

setter :: forall nt record. Newtype nt record => (record -> record) -> nt -> nt
setter fn = wrap <<< fn <<< unwrap

getter :: forall record field nt. Newtype nt record => (record -> field) -> nt -> field
getter fn = fn <<< unwrap

-- TODO: not optimal but Data.Set lacks some function (Set.alter)
toggleSet :: forall a. Ord a => a -> Set a -> Set a
toggleSet a s
  | Set.member a s = Set.delete a s
  | otherwise      = Set.insert a s
