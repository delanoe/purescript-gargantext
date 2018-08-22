module Gargantext.Pages.Home.Actions where


import Prelude hiding (div)

import Gargantext.Components.Lang.Landing.EnUS as En
import Gargantext.Components.Lang.Landing.FrFR as Fr
import Gargantext.Components.Data.Landing (BlockText(..), BlockTexts(..), Button(..), LandingData(..))
import Gargantext.Components.Data.Lang (Lang(..))
import Gargantext.Pages.Home.States (State(..))

import React (ReactElement)
import React.DOM (a, div, h3, i, img, p, span, text)
import React.DOM.Props (Props, _id, aria, className, href, src, target, title)

import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)

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
  lift $ setHash "/search"
  modifyState \state -> state

performAction Login _ _ = void do
  lift $ setHash "/login"
  modifyState \state -> state

performAction SignUp _ _ = void do
  modifyState \state -> state
