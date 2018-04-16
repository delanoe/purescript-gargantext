module Termsview where



import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Array (fold)
import DocView as D
import Network.HTTP.Affjax (AJAX)
import Prelude hiding (div)
import React.DOM (h3, text)
import Thermite (PerformAction, Render, Spec, defaultPerformAction, modifyState, simpleSpec)



type State = D.State


initialState :: D.State
initialState = D.tdata

type Action = D.Action


termsSpec :: forall props eff . Spec ( console :: CONSOLE
                                        , ajax    :: AJAX
                                        , dom     :: DOM
                                        | eff
                                        ) State props Action
termsSpec = simpleSpec defaultPerformAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      [ h3 [] [text "Terms view"]]

termSpec' :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
termSpec' = fold [termsSpec, D.layoutDocview]
