module Gargantext.Pages.Corpus.Tabs.Authors where


import Prelude hiding (div)
import React.DOM (h3, text)
import Thermite (PerformAction, Render, Spec, defaultPerformAction, simpleSpec)

type State = {}

initialState :: State
initialState = {}

type Action = Void

authorSpec :: Spec State {} Action
authorSpec = simpleSpec defaultPerformAction render
  where
    render :: Render State {} Action
    render dispatch _ state _ = []
