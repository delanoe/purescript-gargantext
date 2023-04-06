module Gargantext.Utils.Set where

import Data.Array as A
import Data.Hashable (class Hashable, hash)
import Data.Ord (class Ord)
import Data.Set as Set
import Prelude (($))


-- instance (Hashable a, Ord a) => Hashable (Set.Set a) where
--   hash s = hash $ A.sort $ A.fromFoldable s

-- | If `a` is in Set, remove it, otherwise add it
toggle :: forall a. Ord a => Set.Set a -> a -> Set.Set a
toggle s x = if Set.member x s then Set.delete x s else Set.insert x s
