module Gargantext.Pages.Corpus.User.Brevets where

import Prelude
import Data.Void
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)


type State = String

initialState :: State
initialState = ""

type Action = Void

performAction :: PerformAction State {} Action
performAction action _ _ = absurd action

brevetsSpec :: Spec State {} Action
brevetsSpec = simpleSpec performAction render
  where
    render :: Render State {} Action
    render dispatch _ state _ =
      []
