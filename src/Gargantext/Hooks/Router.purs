module Gargantext.Hooks.Router (useHashRouter) where

import Prelude (($), bind, discard, const, pure)
import Data.Tuple.Nested ((/\))
import Reactix as R
import Routing.Match (Match)
import Routing.Hash (matches)

-- | Ties the hash router to a state hook of routes
-- | Note: if it gets sent to an unrecognised url, it will quietly drop the change
useHashRouter :: forall routes. Match routes -> routes -> R.Hooks (R.State routes)
useHashRouter routes init = do
  route@(_ /\ setRoute) <- R.useState' init
  R.useEffectOnce $ matches routes $ \_old new -> setRoute (const new)
  pure route

