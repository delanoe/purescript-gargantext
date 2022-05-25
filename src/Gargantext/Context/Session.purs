module Gargantext.Context.Session
  ( context
  ) where

import Data.Maybe (Maybe(..))
import Gargantext.Sessions (Session)
import Reactix as R

context :: R.Context (Maybe Session)
context = R.createContext Nothing
