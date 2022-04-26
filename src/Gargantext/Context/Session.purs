module Gargantext.Context.Session
  ( context
  ) where

import Gargantext.Prelude

import Gargantext.Sessions (Session)
import Reactix as R
import Unsafe.Coerce (unsafeCoerce)

-- (?) `unsafeCoerce` → cf. `Gargantext.Utils.Stores`
context :: R.Context (Session)
context = R.createContext $ unsafeCoerce unit
