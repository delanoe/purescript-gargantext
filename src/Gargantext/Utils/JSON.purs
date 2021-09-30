module Gargantext.Utils.JSON where

import Prelude

import Control.Monad.Except (withExcept)
import Data.Int as Int
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import Data.Traversable (sequence)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign (F, Foreign, ForeignError(..), readArray, unsafeToForeign)
import Foreign as F
import Foreign.Object as Object
import Simple.JSON as JSON


readSequence :: forall a. JSON.ReadForeign a => Foreign -> F (Seq.Seq a)
readSequence f = do
    arr <- readArray f
    y <- traverseWithIndex readAtIdx arr
    pure $ Seq.fromFoldable y
  where
    readAtIdx i f' = withExcept (map (ErrorAtIndex i)) (JSON.readImpl f')

writeSequence :: forall a. JSON.WriteForeign a => Seq.Seq a -> Foreign
writeSequence xs = unsafeToForeign $ JSON.writeImpl $ (Seq.toUnfoldable xs :: Array a)

readList :: forall a. JSON.ReadForeign a => Foreign -> F (List.List a)
readList f = do
    arr <- readArray f
    y <- traverseWithIndex readAtIdx arr
    pure $ List.fromFoldable y
  where
    readAtIdx i f' = withExcept (map (ErrorAtIndex i)) (JSON.readImpl f')

writeList :: forall a. JSON.WriteForeign a => List.List a -> Foreign
writeList xs = unsafeToForeign $ JSON.writeImpl <$> (List.toUnfoldable xs :: Array a)

readMapInt :: forall v. JSON.ReadForeign v => Foreign -> F (Map.Map Int v)
readMapInt f = do
  (inst :: Object.Object Foreign) <- readObject' f
  let (mapped :: Array (F (Tuple Int v))) = (\(Tuple k v) ->
                                              case Int.fromString k of
                                                Nothing -> F.fail $ ErrorAtProperty k $ ForeignError "Cannot convert to int"
                                                Just kInt -> do
                                                  v' <- JSON.readImpl v
                                                  pure $ Tuple kInt v'
                                            ) <$> Object.toUnfoldable inst
  seq <- sequence mapped
  pure $ Map.fromFoldable seq
  where
    readObject' :: Foreign -> F (Object.Object Foreign)
    readObject' value
      | F.tagOf value == "Object" = pure $ F.unsafeFromForeign value
      | otherwise                 = F.fail $ TypeMismatch "Object" $ F.tagOf value
