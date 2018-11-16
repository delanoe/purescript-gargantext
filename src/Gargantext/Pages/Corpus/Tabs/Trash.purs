module Gargantext.Pages.Corpus.Tabs.Trash where

import Data.Array (fold)
import Prelude hiding (div)
import React.DOM (h3, text)
import Thermite (PerformAction, Render, Spec, defaultPerformAction, simpleSpec)


type State = {}

initialState :: State
initialState = {}

type Action = Void

spec :: Spec State {} Action
spec = simpleSpec defaultPerformAction render
  where
    render :: Render State {} Action
    render dispatch _ state _ = []
