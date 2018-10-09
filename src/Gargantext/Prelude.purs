module Gargantext.Prelude (module Prelude, logs)
  where

import Prelude
import Effect.Console (log)
import Effect.Class   -- (MonadEffect(), liftEffect) -- TODO fix import


logs:: forall message effect.
       (MonadEffect effect) => Show message => message 
       -> effect Unit
logs = liftEffect <<< log <<< show

