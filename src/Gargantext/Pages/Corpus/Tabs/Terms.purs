module Gargantext.Pages.Corpus.Tabs.Terms where

import Data.Array (fold)
import Prelude hiding (div)
import React.DOM (h3, text)
import Thermite (PerformAction, Render, Spec, defaultPerformAction, simpleSpec)


type State = {}

initialState :: State
initialState = {}

type Action = Void

termsSpec :: Spec State {} Action
termsSpec = simpleSpec defaultPerformAction render
  where
    render :: Render State {} Action
    render dispatch _ state _ =
      [ h3 [] [text "Terms view"]]
