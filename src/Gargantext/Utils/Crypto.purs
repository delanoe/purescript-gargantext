module Gargantext.Utils.Crypto where

import Data.Set (Set)
import Data.Set   as Set
import Data.Array as Array
import Gargantext.Prelude
import Crypto.Simple as Crypto

-- | TODO use newtype to disambiguate Set String and Set Hash
-- Set String needs Set.map hash
-- Set Hash   does not need Set.map hash (just concat)
type Hash = String

hash' :: forall a. Crypto.Hashable a => a -> String
hash' = Crypto.toString <<< Crypto.hash Crypto.SHA256

class IsHashable a where
  hash :: a -> Hash

instance isHashableString :: IsHashable String
  where
    hash = hash'

instance isHashableArray :: (Crypto.Hashable a, IsHashable a) => IsHashable (Array a)
  where
    hash = hash <<< Set.fromFoldable <<< map hash

instance isHashableSet :: IsHashable (Set String) where
  hash = hash <<< concat <<< toArray
    where
      toArray :: forall a. Set a -> Array a
      toArray = Set.toUnfoldable

      concat :: Array Hash -> String
      concat = Array.foldl (<>) ""


