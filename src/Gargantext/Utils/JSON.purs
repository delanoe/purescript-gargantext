module Gargantext.Utils.JSON where

import Prelude

import Control.Monad.Except (withExcept)
import Data.Int as Int
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Sequence as Seq
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign (F, Foreign, ForeignError(..), readArray, unsafeToForeign)
import Foreign as F
import Foreign.Object as Object
import Simple.JSON (writeImpl)
import Simple.JSON as JSON

import Gargantext.Utils.Tuple as GUT

readSequence :: forall a. JSON.ReadForeign a => Foreign -> F (Seq.Seq a)
readSequence f = do
    arr <- readArray f
    y <- traverseWithIndex readAtIdx arr
    pure $ Seq.fromFoldable y
  where
    readAtIdx i f' = withExcept (map (ErrorAtIndex i)) (JSON.readImpl f')

writeSequence :: forall a. JSON.WriteForeign a => Seq.Seq a -> Foreign
writeSequence xs = unsafeToForeign $ JSON.writeImpl <$> xs

readList :: forall a. JSON.ReadForeign a => Foreign -> F (List.List a)
readList f = do
    arr <- readArray f
    y <- traverseWithIndex readAtIdx arr
    pure $ List.fromFoldable y
  where
    readAtIdx i f' = withExcept (map (ErrorAtIndex i)) (JSON.readImpl f')

writeList :: forall a. JSON.WriteForeign a => List.List a -> Foreign
writeList xs = unsafeToForeign $ JSON.writeImpl <$> xs

readMapInt :: forall v. JSON.ReadForeign v => Foreign -> F (Map.Map Int v)
readMapInt f = do
  inst <- readObject' f
  let mapped = GUT.mapFst (fromJust <<< Int.fromString) <$> Object.toUnfoldable inst
  pure $ Map.fromFoldable mapped
  where
    readObject' :: Foreign -> F (Object.Object Foreign)
    readObject' value
      | F.tagOf value == "Object" = pure $ F.unsafeFromForeign value
      | otherwise                 = F.fail $ TypeMismatch "Object" $ F.tagOf value
