module Gargantext.Prelude (module Prelude, logs, id, class Read, read)
  where

import Data.Maybe (Maybe(..))
import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class Category, class CommutativeRing, class Discard, class DivisionRing, class Eq, class EuclideanRing, class Field, class Functor, class HeytingAlgebra, class Monad, class Monoid, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, type (~>), Ordering(..), Unit, Void, absurd, add, ap, append, apply, between, bind, bottom, clamp, compare, comparing, compose, conj, const, degree, discard, disj, eq, flap, flip, gcd, identity, ifM, join, lcm, liftA1, liftM1, map, max, mempty, min, mod, mul, negate, not, notEq, one, otherwise, pure, recip, show, sub, top, unit, unless, unlessM, void, when, whenM, zero, (#), ($), ($>), (&&), (*), (*>), (+), (-), (/), (/=), (<), (<#>), (<$), (<$>), (<*), (<*>), (<<<), (<=), (<=<), (<>), (<@>), (=<<), (==), (>), (>=), (>=>), (>>=), (>>>), (||))
import Effect.Console (log)
import Effect.Class (class MonadEffect, liftEffect)


-- | JL: Astonishingly, not in the prelude
--   AD: recent Preludes in Haskell much prefer identity
--   then id can be used as a variable name (in records for instance)
--   since records in Purescript are not the same as in Haskell
--   this behavior is questionable indeed.
id :: forall a. a -> a
id a = a

class Read a where
  read :: String -> Maybe a

logs:: forall message effect.
       (MonadEffect effect)
       => Show message
       => message 
       -> effect Unit
logs = liftEffect <<< log <<< show



