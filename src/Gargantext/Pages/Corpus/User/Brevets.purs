module Gargantext.Pages.Corpus.User.Brevets where

import Prelude
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)


type State = String

initialState :: State
initialState = ""

data Action = NoOp

performAction :: PerformAction State {} Action
performAction NoOp _ _ = void do
  modifyState identity

brevetsSpec :: Spec State {} Action
brevetsSpec = simpleSpec performAction render
  where
    render :: Render State {} Action
    render dispatch _ state _ =
      []
