module Termsview where



import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Prelude hiding (div)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)




type State = String


initialState :: State
initialState = ""

data Action = NoOp


performAction :: forall eff props. PerformAction ( console :: CONSOLE
                                                 , ajax    :: AJAX
                                                 , dom     :: DOM
                                                 | eff
                                                 ) State props Action
performAction NoOp _ _ = void do
  modifyState id




termsSpec :: forall props eff . Spec ( console :: CONSOLE
                                        , ajax    :: AJAX
                                        , dom     :: DOM
                                        | eff
                                        ) State props Action
termsSpec = simpleSpec performAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      []
