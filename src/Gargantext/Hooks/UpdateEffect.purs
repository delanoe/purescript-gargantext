module Gargantext.Hooks.UpdateEffect
  ( useUpdateEffect, useUpdateEffect'
  , useUpdateEffect1, useUpdateEffect1'
  , useUpdateEffect2, useUpdateEffect2'
  , useUpdateEffect3, useUpdateEffect3'
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

useUpdateEffect2 :: forall a b. a -> b -> Effect (Effect Unit) -> R.Hooks Unit
useUpdateEffect2 a b e = useFirstMount >>= eff2 a b e

useUpdateEffect2' :: forall a b c. a -> b -> Effect c -> R.Hooks Unit
useUpdateEffect2' a b e = useFirstMount >>= eff2 a b (e # thenNothing)

useUpdateEffect3 :: forall a b c. a -> b -> c -> Effect (Effect Unit) -> R.Hooks Unit
useUpdateEffect3 a b c e = useFirstMount >>= eff3 a b c e

useUpdateEffect3' :: forall a b c d. a -> b -> c -> Effect d -> R.Hooks Unit
useUpdateEffect3' a b c e = useFirstMount >>= eff3 a b c (e # thenNothing)

eff :: Effect (Effect Unit) -> Boolean -> R.Hooks Unit
eff ef cd = R.useEffect if cd then nothing # thenNothing else ef

eff1 :: forall a. a -> Effect (Effect Unit) -> Boolean -> R.Hooks Unit
eff1 a ef cd = R.useEffect1 a if cd then nothing # thenNothing else ef

eff2 :: forall a b. a -> b -> Effect (Effect Unit) -> Boolean -> R.Hooks Unit
eff2 a b ef cd = R.useEffect2 a b if cd then nothing # thenNothing else ef

eff3 :: forall a b c. a -> b -> c -> Effect (Effect Unit) -> Boolean -> R.Hooks Unit
eff3 a b c ef cd = R.useEffect3 a b c if cd then nothing # thenNothing else ef
