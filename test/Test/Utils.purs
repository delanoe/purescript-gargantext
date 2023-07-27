module Test.Utils where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Exception (Error)
import Test.Spec.Assertions (fail)

-- | This function can be used to compare arrays, it reports the diff in more
-- | detail which can be useful when debuggnign tests
shouldEqualArray
  :: forall m t
   . MonadThrow Error m
  => Show t
  => Eq t
  => Array t
  -> Array t
  -> m Unit
shouldEqualArray v1 v2 =
  when (v1 /= v2) $
    fail $ show v1 <> " ≠ " <> show v2 <> diff
  where
    diffs = A.filter (\(Tuple a b) -> a /= b) $ A.zip v1 v2
    diff = case A.head diffs of
      Nothing -> ""
      Just (Tuple a1 a2) -> " (first differing element: " <> (show a1 <> " ≠ " <> show a2) <> ")"
