module Gargantext.Pages.Corpus.Tabs.Sources where


import Prelude hiding (div)
import React.DOM (h3, text)
import Thermite (Render, Spec, defaultPerformAction, simpleSpec)

type State = {}

initialState :: State
initialState = {}

type Action = Void

sourceSpec :: Spec State {} Action
sourceSpec = simpleSpec defaultPerformAction render
  where
    render :: Render State {} Action
    render dispatch _ state _ = []
