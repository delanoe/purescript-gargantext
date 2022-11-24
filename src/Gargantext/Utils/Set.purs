module Gargantext.Utils.Set where

import Data.Ord (class Ord)
import Data.Set as Set


-- | If `a` is in Set, remove it, otherwise add it
toggle :: forall a. Ord a => Set.Set a -> a -> Set.Set a
toggle s x = if Set.member x s then Set.delete x s else Set.insert x s
