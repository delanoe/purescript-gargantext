module Users.Types.Types where

import Brevets as B
import Control.Monad.Aff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Prelude (id, void)
import Projects as PS
import Publications as P
import Tab as Tab
import Thermite (PerformAction, modifyState)

type State =
  { activeTab :: Int
  , publications :: P.State
  , brevets :: B.State
  , projects :: PS.State
  }

initialState :: State
initialState =
  { activeTab : 0
  , publications : P.initialState
  , brevets : B.initialState
  , projects : PS.initialState
  }

data Action
  = NoOp
  | PublicationA P.Action
  | BrevetsA B.Action
  | ProjectsA PS.Action
  | TabA Tab.Action

performAction :: forall eff props. PerformAction ( console :: CONSOLE
                                                 , ajax    :: AJAX
                                                 , dom     :: DOM
                                                 | eff ) State props Action
performAction NoOp _ _ = void do
  modifyState id
performAction _ _ _ = void do
  modifyState id
