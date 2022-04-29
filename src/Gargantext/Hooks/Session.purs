module Gargantext.Hooks.Session
  ( useSession
  ) where

import Gargantext.Context.Session as SessionContext
import Gargantext.Sessions (Session)
import Reactix as R

useSession :: R.Hooks (Session)
useSession = R.useContext SessionContext.context
