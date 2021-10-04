module Gargantext.Utils.Tuple where

import Data.Tuple (Tuple(..))

mapFst :: forall a b c. (a -> c) -> Tuple a b -> Tuple c b
mapFst f (Tuple k v) = Tuple (f k) v
