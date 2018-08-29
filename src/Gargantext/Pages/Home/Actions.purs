module Gargantext.Pages.Home.Actions where

import Prelude hiding (div)

import Effect.Class (liftEffect)
import Gargantext.Pages.Home.States (State)
import Routing.Hash (setHash)
import Thermite (PerformAction, modifyState)

data Action
  = NoOp
  | Documentation
  | Enter
  | Login
  | SignUp


performAction :: forall props. PerformAction State props Action
performAction NoOp _ _ = void do
  modifyState \state -> state

performAction Documentation _ _ = void do
  modifyState \state -> state

performAction Enter _ _ = void do
  liftEffect $ setHash "/search"
  modifyState \state -> state

performAction Login _ _ = void do
  liftEffect $ setHash "/login"
  modifyState \state -> state

performAction SignUp _ _ = void do
  modifyState \state -> state
