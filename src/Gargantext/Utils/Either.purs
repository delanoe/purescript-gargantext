module Gargantext.Utils.Either where

import Gargantext.Prelude

import Data.Array (cons, uncons)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

eitherList :: forall l r. Array (Either l r) -> Either l (Array r)
eitherList xs = case uncons xs of
  Nothing -> Right []
  Just { head: Left x } -> Left x
  Just { head: Right x, tail } ->
    case eitherList tail of
      Left err -> Left err
      Right ds -> Right (cons x ds)


eitherMap :: forall k l r. Ord k => Map.Map k (Either l r) -> Either l (Map.Map k r)
eitherMap m = case eitherList (helper <$> Map.toUnfoldable m) of
  Left err  -> Left err
  Right lst -> Right $ Map.fromFoldable lst
  where
    helper (Tuple _ (Left err)) = Left err
    helper (Tuple k (Right v)) = Right (Tuple k v)
