module Gargantext.Utils.Tuple where

import Data.Tuple (Tuple(..))


-- | Similar to
--   https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Bifunctor.html#v:first
first :: forall a b c. (a -> c) -> Tuple a b -> Tuple c b
first f (Tuple k v) = Tuple (f k) v
