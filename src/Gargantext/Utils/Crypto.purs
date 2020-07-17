module Gargantext.Utils.Crypto where

import Data.Ord
import Data.Eq
import Data.Functor
import Data.Semigroup
import Data.Set (Set)
import Data.Set   as Set
import Data.Array as Array
import Gargantext.Prelude
import Crypto.Simple as Crypto

type Hash = String

-- | Main exposed api
hash :: forall a. IsHashable a => a -> String
hash x = toString (hash'' x)

-- | Newtype HashDone is needed to disambiguate Set String and Set HashDone
-- Set String   needs Set.map hash
-- Set HashDone does not need Set.map hash (just concat)
newtype HashDone = HashDone Hash

toString :: HashDone -> Hash
toString (HashDone x) = x

instance ordHashDone :: Ord HashDone where
  compare (HashDone a) (HashDone b) = compare a b

instance eqHashDone :: Eq HashDone where
  eq (HashDone a) (HashDone b) = eq a b

------------------------------------------------------------------------
hash' :: forall a. Crypto.Hashable a => a -> HashDone
hash' x = HashDone $ Crypto.toString $ Crypto.hash Crypto.SHA256 x

class IsHashable a where
  hash'' :: a -> HashDone

instance isHashableString :: IsHashable String
  where
    hash'' = hash'

------------------------------------------------------------------------
instance isHashableArrayHashDone :: IsHashable (Array HashDone)
  where
    hash'' x = hash'' $ Set.fromFoldable x
else instance isHashableArray :: (Crypto.Hashable a, IsHashable a) => IsHashable (Array a)
  where
    hash'' x = hash'' $ map hash x

------------------------------------------------------------------------
instance isHashableSetHashDone :: IsHashable (Set HashDone) where
  hash'' x = hash'' $ concat $ map toString $ toArray x
    where
      toArray :: forall a. Set a -> Array a
      toArray = Set.toUnfoldable

      concat :: Array Hash -> String
      concat = Array.foldl (<>) ""
else instance isHashableSet :: (Crypto.Hashable a, IsHashable a) => IsHashable (Set a)
  where
    hash'' x = hash'' $ Set.map hash'' x


