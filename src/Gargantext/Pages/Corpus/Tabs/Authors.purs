module Gargantext.Pages.Corpus.Tabs.Authors where


import Data.Array (fold)
import Gargantext.Pages.Corpus.Tabs.Documents as D
import Prelude hiding (div)
import React.DOM (h3, text)
import Thermite (PerformAction, Render, Spec, defaultPerformAction, simpleSpec)

type State = D.State

initialState :: State
initialState = D.initialState

type Action = D.Action

authorSpec :: Spec State {} Action
authorSpec = simpleSpec defaultPerformAction render
  where
    render :: Render State {} Action
    render dispatch _ state _ =
       [ h3 [] [text "AuthorView"]]

authorspec' :: Spec State {} Action
authorspec' = fold [authorSpec, D.layoutDocview]
