module Gargantext.Pages.Corpus.Doc.Facets.Authors where


import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Array (fold)
import Gargantext.Pages.Corpus.Doc.Facets.Documents as D
import Network.HTTP.Affjax (AJAX)
import Prelude hiding (div)
import React.DOM (h3, text)
import Thermite (PerformAction, Render, Spec, defaultPerformAction, modifyState, simpleSpec)




type State = D.State


initialState :: State
initialState = D.tdata

type Action = D.Action


authorSpec :: forall props eff . Spec ( console :: CONSOLE
                                        , ajax    :: AJAX
                                        , dom     :: DOM
                                        | eff
                                        ) State props Action
authorSpec = simpleSpec defaultPerformAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
       [ h3 [] [text "AuthorView"]]

authorspec' :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
authorspec' = fold [authorSpec, D.layoutDocview]
