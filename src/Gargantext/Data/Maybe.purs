module Gargantext.Data.Maybe where

import Prelude (bind, flip, identity, (<<<))
import Data.Maybe


mapMaybe :: forall a b. (a -> Maybe b) -> Array a -> Array b
mapMaybe f = concatMap (maybe [] singleton <<< f)

catMaybes :: forall a. Array (Maybe a) -> Array a
catMaybes = mapMaybe identity

concatMap :: forall a b. (a -> Array b) -> Array a -> Array b
concatMap = flip bind

singleton :: forall a. a -> Array a
singleton a = [a]

