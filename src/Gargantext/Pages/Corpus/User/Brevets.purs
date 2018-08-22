module Gargantext.Pages.Corpus.User.Brevets where


import Prelude (id, void)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)


type State = String

initialState :: State
initialState = ""

data Action = NoOp

performAction :: forall props. PerformAction State props Action
performAction NoOp _ _ = void do
  modifyState id


brevetsSpec :: forall props. Spec State props Action
brevetsSpec = simpleSpec performAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      []
