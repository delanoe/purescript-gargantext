module Gargantext.Hooks.FirstEffect
  ( useFirstMount
  , useFirstEffect, useFirstEffect'
  , useFirstLayoutEffect, useFirstLayoutEffect'
  ) where

import Gargantext.Prelude

import Effect (Effect)
import Reactix (nothing, thenNothing)
import Reactix as R

-- /!\ For some reasons, cleanup function works perfectly on some cases,
--     sometimes not... (@TODO? is this a React odd behavior?)
--      ↳ use `R.useFirstEffect1 []` instead of `useFirstEffect` for now

-- | Hook triggered on first mount event only
useFirstMount :: R.Hooks (Boolean)
useFirstMount = do
  firstMount <- R.useRef true

  let firstMount' = R.readRef firstMount

  R.unsafeHooksEffect
    if firstMount' == true
    then R.setRef firstMount false
    else nothing

  pure firstMount'

-- | Hook triggered on first mount only
useFirstEffect :: Effect (Effect Unit) -> R.Hooks Unit
useFirstEffect e = useFirstMount >>= eff e

-- | Like `useFirstEffect` but Effect fn does return a cleanup handler
useFirstEffect' :: forall a. Effect a -> R.Hooks Unit
useFirstEffect' e = useFirstMount >>= eff (e # thenNothing)

-- | Hook triggered on first mount only (layout hook mode)
useFirstLayoutEffect :: Effect (Effect Unit) -> R.Hooks Unit
useFirstLayoutEffect e = useFirstMount >>= eff' e

-- | Like `useFirstLayoutEffect` but Effect fn does return a cleanup handler
useFirstLayoutEffect' :: forall a. Effect a -> R.Hooks Unit
useFirstLayoutEffect' e = useFirstMount >>= eff' (e # thenNothing)

eff :: Effect (Effect Unit) -> Boolean -> R.Hooks Unit
eff e b = R.useEffect if b then e else nothing # thenNothing

eff' :: Effect (Effect Unit) -> Boolean -> R.Hooks Unit
eff' e b = R.useLayoutEffect if b then e else nothing # thenNothing
