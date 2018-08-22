module Gargantext.Pages.Corpus.Doc.Facets.Sources where


import Data.Array (fold)
import Gargantext.Pages.Corpus.Doc.Facets.Documents as D
import Prelude hiding (div)
import React.DOM (h3, text)
import Thermite (Render, Spec, defaultPerformAction, simpleSpec)

type State = D.State


initialState :: D.State
initialState = D.tdata

type Action = D.Action


sourceSpec :: forall props. Spec State props Action
sourceSpec = simpleSpec defaultPerformAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      [ h3 [] [text "Source view"]]

sourcespec' :: forall props. Spec State props Action
sourcespec' = fold [sourceSpec, D.layoutDocview]
