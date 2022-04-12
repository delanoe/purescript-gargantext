module Gargantext.Hooks.FirstEffect
  ( useFirstMount
  , useFirstEffect, useFirstEffect'
  ) where

import Gargantext.Prelude

import Effect (Effect)
import Reactix (nothing, thenNothing)
import Reactix as R

-- | Hook triggered on first mount event only
-- |
-- | /!\ @TODO cleanup function not working
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

eff :: Effect (Effect Unit) -> Boolean -> R.Hooks Unit
eff e b = R.useEffect if b then e else nothing # thenNothing
