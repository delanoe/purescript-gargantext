module Gargantext.Utils.List where

import Data.Ord (class Ord, comparing)
import Data.List  as List
import Data.Array as Array
import Gargantext.Prelude

-- same as
-- https://github.com/purescript/purescript-arrays/blob/v5.3.1/src/Data/Array.purs#L715-L715
sortWith :: forall a b. Ord b => (a -> b) -> List.List a -> List.List a
sortWith f l = Array.toUnfoldable $ Array.sortBy (comparing f) $ Array.fromFoldable l
