module Gargantext.Pages.Corpus.Tabs.Sources where


import Data.Array (fold)
import Gargantext.Pages.Corpus.Tabs.Documents as D
import Prelude hiding (div)
import React.DOM (h3, text)
import Thermite (Render, Spec, defaultPerformAction, simpleSpec)

-- NP: TODO the state should be only the one required by sourceSpec not sourcespec'
type State = D.State


initialState :: D.State
initialState = D.initialState

type Action = D.Action


sourceSpec :: Spec State {} Action
sourceSpec = simpleSpec defaultPerformAction render
  where
    render :: Render State {} Action
    render dispatch _ state _ =
      [ h3 [] [text "Source view"]]

sourcespec' :: Spec State {} Action
sourcespec' = sourceSpec <> D.layoutDocview
