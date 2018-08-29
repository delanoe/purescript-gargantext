module Gargantext.Pages.Corpus.User.Brevets where

import Prelude
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)


type State = String

initialState :: State
initialState = ""

data Action = NoOp

performAction :: forall props. PerformAction State props Action
performAction NoOp _ _ = void do
  modifyState identity

brevetsSpec :: forall props. Spec State props Action
brevetsSpec = simpleSpec performAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      []
