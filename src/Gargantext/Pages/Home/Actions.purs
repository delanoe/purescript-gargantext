module Gargantext.Pages.Home.Actions where

import Prelude hiding (div)

import Effect.Class (liftEffect)
import Gargantext.Pages.Home.States (State)
import Routing.Hash (setHash)
import Thermite (PerformAction)

data Action
  = Documentation
  | Enter
  | Login
  | SignUp


performAction :: PerformAction State {} Action
performAction Documentation _ _ = pure unit

performAction Enter _ _ = void do
  liftEffect $ setHash "/search"

performAction Login _ _ = void do
  liftEffect $ setHash "/login"

performAction SignUp _ _ = pure unit
