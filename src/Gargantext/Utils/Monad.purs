module Gargantext.Utils.Monad where

import Prelude (unit, pure, Unit, (<<<))
import Control.Monad (class Monad, (>>=))
import Data.Foldable (class Foldable, foldr)

bind_ :: forall a b m. Monad m => m a -> m b -> m b
bind_ m k = m >>= \_ -> k -- See Note [Recursive bindings for Applicative/Monad]

infixr 1 bind_ as >>

mapM_ :: forall a b t m. Foldable t => Monad m => (a -> m b) -> t a -> m Unit
mapM_ f = foldr ((>>) <<< f) (pure unit)
