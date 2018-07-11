module Sourceview where


import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Array (fold)
import Gargantext.Pages.Corpus.Doc.Document as D
import Network.HTTP.Affjax (AJAX)
import Prelude hiding (div)
import React.DOM (h3, text)
import Thermite (Render, Spec, defaultPerformAction, simpleSpec)

type State = D.State


initialState :: D.State
initialState = D.tdata

type Action = D.Action


sourceSpec :: forall props eff . Spec ( console :: CONSOLE
                                        , ajax    :: AJAX
                                        , dom     :: DOM
                                        | eff
                                        ) State props Action
sourceSpec = simpleSpec defaultPerformAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      [ h3 [] [text "Source view"]]

sourcespec' :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
sourcespec' = fold [sourceSpec, D.layoutDocview]
