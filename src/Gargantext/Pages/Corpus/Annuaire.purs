module Gargantext.Pages.Corpus.Annuaire where

import Prelude
import React.DOM (div, h1, text)
import Thermite (Render, Spec, simpleSpec, defaultPerformAction)


render :: Render {} {} Void
render dispatch _ state _ = [h1 [] [text "Annuaire"]]

layoutAnnuaire :: Spec {} {} Void
layoutAnnuaire = simpleSpec defaultPerformAction render

