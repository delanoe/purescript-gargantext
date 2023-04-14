module Gargantext.Utils.Show where

import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Gargantext.Prelude

-- | Derive a generic reader from items for which a `Show` class is
-- | implemented. The `Read` type class is not present in PureScript
-- | and is generally discouraged.
-- | https://discourse.purescript.org/t/read-type-class/1434/5
reader :: forall item. Show item => Array item -> (String -> Maybe item)
reader items = reader
  where
    strMap :: Map.Map String item
    strMap = Map.fromFoldable $ (\i -> Tuple (show i) i) <$> items

    reader k = Map.lookup k strMap
