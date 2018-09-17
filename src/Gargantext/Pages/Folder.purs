module Gargantext.Pages.Folder where

import Prelude

import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)


type State = String

initialState :: State
initialState = ""

data Action = NoOp

performAction :: PerformAction State {} Action
performAction NoOp _ _ = pure unit

projets :: Spec State {} Action
projets = simpleSpec performAction render
  where
    render :: Render State {} Action
    render dispatch _ state _ =
      []
