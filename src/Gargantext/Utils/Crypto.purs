module Gargantext.Utils.Crypto where

import Control.Promise (Promise, toAffE)
import Data.Array as Array
import Data.Set (Set)
import Data.Set   as Set
import Data.String (joinWith)
import Effect (Effect)
import Effect.Aff (Aff)

import Gargantext.Prelude

foreign import getSHA256HashImpl :: String -> Effect (Promise String)

getSHA256Hash :: String -> Aff String
getSHA256Hash = getSHA256HashImpl >>> toAffE

-- | TODO use newtype to disambiguate Set String and Set Hash
-- Set String needs Set.map hash
-- Set Hash   does not need Set.map hash (just concat)
type Hash = String

hash' :: String -> Aff String
hash' = getSHA256Hash
--hash' :: forall a. Crypto.Hashable a => a -> String
--hash' = Crypto.toString <<< Crypto.hash Crypto.SHA256

class IsHashable a where
  hash :: a -> Aff Hash

instance IsHashable String
  where
    hash = hash'

--instance (Crypto.Hashable a, IsHashable a) => IsHashable (Array a)
--  where
--    hash = hash <<< Set.fromFoldable <<< map hash

instance IsHashable (Array String) where
  hash = hash <<< concat
    where
      concat :: Array Hash -> String
      concat = joinWith "" <<< Array.sort


instance IsHashable (Set String) where
  hash = hash <<< toArray
    where
      toArray :: forall a. Set a -> Array a
      toArray = Set.toUnfoldable
