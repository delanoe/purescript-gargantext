module Gargantext.Pages.Corpus.Doc.Facets.Terms where

import Data.Array (fold)
import Gargantext.Pages.Corpus.Doc.Facets.Documents as D
import Prelude hiding (div)
import React.DOM (h3, text)
import Thermite (PerformAction, Render, Spec, defaultPerformAction, modifyState, simpleSpec)



type State = D.State


initialState :: D.State
initialState = D.tdata

type Action = D.Action


termsSpec :: forall props. Spec State props Action
termsSpec = simpleSpec defaultPerformAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      [ h3 [] [text "Terms view"]]

termSpec' :: forall props. Spec State props Action
termSpec' = fold [termsSpec, D.layoutDocview]
