module Gargantext.Hooks.UpdateEffect
  ( useUpdateEffect, useUpdateEffect'
  , useUpdateEffect1, useUpdateEffect1'
  ) where

import Gargantext.Prelude

import Effect (Effect)
import Gargantext.Hooks.FirstEffect (useFirstMount)
import Reactix (nothing, thenNothing)
import Reactix as R

-- | Hook triggered on update only (not on first mount)
useUpdateEffect :: Effect (Effect Unit) -> R.Hooks Unit
useUpdateEffect e = useFirstMount >>= eff e

-- | Like `useUpdateEffect` but Effect fn does return a cleanup handler
useUpdateEffect' :: forall a. Effect a -> R.Hooks Unit
useUpdateEffect' e = useFirstMount >>= eff (e # thenNothing)

-- | Like `useUpdateEffect` but with a memo value
useUpdateEffect1 :: forall a. a -> Effect (Effect Unit) -> R.Hooks Unit
useUpdateEffect1 a e = useFirstMount >>= eff1 a e

-- | Like `useUpdateEffect` but the provided Effect does not return a
-- | cleanup handler
useUpdateEffect1' :: forall a b. a -> Effect b -> R.Hooks Unit
useUpdateEffect1' a e = useFirstMount >>= eff1 a (e # thenNothing)

eff :: Effect (Effect Unit) -> Boolean -> R.Hooks Unit
eff e b = R.useEffect if b then nothing # thenNothing else e

eff1 :: forall a. a -> Effect (Effect Unit) -> Boolean -> R.Hooks Unit
eff1 a e b = R.useEffect1 a if b then nothing # thenNothing else e
