module Gargantext.Hooks.Session
  ( useSession
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Gargantext.Context.Session as SessionContext
import Gargantext.Sessions (Session)
import Gargantext.Utils.Reactix as R2
import Partial.Unsafe (unsafeCrashWith)
import Reactix as R

here :: R2.Here
here = R2.here "Gargantext.Hooks.Session"

useSession :: R.Hooks (Session)
useSession = R.useContext SessionContext.context >>= case _ of
  Nothing -> do
    R.unsafeHooksEffect $ here.error $
      "Trying to access to current `Session` but no one could " <>
      "be found. It could be an issue where a component uses this hook " <>
      "without a prior `R.provideContext`"
    unsafeCrashWith "no session found"
  Just s  -> pure s
