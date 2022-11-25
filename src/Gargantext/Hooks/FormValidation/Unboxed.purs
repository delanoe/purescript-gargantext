module Gargantext.Hooks.FormValidation.Unboxed
  ( class Equals, equals
  , class NonEmpty, nonEmpty
  , class Minimum, minimum
  , class Maximum, maximum
  , lowercase, uppercase, email, date, number, int
  ) where

import Gargantext.Prelude

import Data.Maybe (isNothing)
import Data.Number as Number
import Data.Int as Int
import Data.String (toLower, toUpper)
import Data.String.CodeUnits (length)
import Data.String.Regex (test)
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (invalid)
import Effect (Effect)
import Gargantext.Hooks.FormValidation.Types (Field, VForm, emailPattern, datePattern)

class Eq a <= Equals a where
  equals :: Field -> a -> a -> Effect VForm

class NonEmpty a where
  nonEmpty :: Field -> a -> Effect VForm

class Ord a <= Minimum a where
  minimum :: Field -> a -> Int -> Effect VForm

class Ord a <= Maximum a where
  maximum :: Field -> a -> Int -> Effect VForm

-- Regarding String field value

instance equalsString :: Equals String where
  equals field input input'
    | (not eq input input') = pure $ invalid [ field /\ "equals" ]
    | otherwise             = pure $ pure unit

instance nonEmptyString :: NonEmpty String where
  nonEmpty field "" = pure $ invalid [ field /\ "nonEmpty" ]
  nonEmpty _ _      = pure $ pure unit

instance minimumString :: Minimum String where
  minimum field input min
    | (length input) < min = pure $ invalid [ field /\ "minimum" ]
    | otherwise            = pure $ pure unit

instance maximumString :: Maximum String where
  maximum field input max
    | (length input) > max = pure $ invalid [ field /\ "maximum" ]
    | otherwise            = pure $ pure unit

-- Regarding Boolean field value

instance equalsBoolean :: Equals Boolean where
  equals field input input'
    | (not eq input input') = pure $ invalid [ field /\ "equals" ]
    | otherwise             = pure $ pure unit

uppercase :: Field -> String -> Effect VForm
uppercase field input
  | (toLower input) == input = pure $ invalid [ field /\ "uppercase" ]
  | otherwise                = pure $ pure unit

lowercase :: Field -> String -> Effect VForm
lowercase field input
  | (toUpper input) == input = pure $ invalid [ field /\ "lowercase" ]
  | otherwise                = pure $ pure unit

email :: Field -> String -> Effect VForm
email field input
  | (not $ test emailPattern input) = pure $ invalid [ field /\ "email" ]
  | otherwise                       = pure $ pure unit

date :: Field -> String -> Effect VForm
date field input
  | (not $ test datePattern input) = pure $ invalid [ field /\ "date" ]
  | otherwise                      = pure $ pure unit

number :: Field -> String -> Effect VForm
number field input
  | (isNothing $ Number.fromString input) = pure $ invalid [ field /\ "number" ]
  | otherwise                             = pure $ pure unit

int :: Field -> String -> Effect VForm
int field input
  | (isNothing $ Int.fromString input) = pure $ invalid [ field /\ "int" ]
  | otherwise                          = pure $ pure unit
