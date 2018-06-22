module Gargantext.Users.API
       where

import Gargantext.Users.Types

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Lens (set)
import Data.Maybe (Maybe(..))
import Gargantext.REST (get)
import Network.HTTP.Affjax (AJAX)
import Prelude (id, show, void, ($), (<>), bind)
import Thermite (PerformAction, modifyState)

getUser :: forall eff. Int -> Aff
        (console :: CONSOLE, ajax :: AJAX | eff) (Either String User)
getUser id = get $ "localhost:8008/node/" <> show id


performAction :: forall eff props. PerformAction ( console :: CONSOLE
                                                 , ajax    :: AJAX
                                                 , dom     :: DOM
                                                 | eff ) State props Action
performAction NoOp _ _ = void do
  modifyState id
performAction FetchUser _ _ = void do
  value <- lift $ getUser 452145
  let user = case value of
        (Right user) -> Just user
        _ -> Nothing
  modifyState \state -> set _user user state
performAction _ _ _ = void do
  modifyState id
