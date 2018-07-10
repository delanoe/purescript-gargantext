module Gargantext.Users.API where

import Prelude

import Gargantext.Users.Types (Action(..), State, User, _user)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Lens (set)
import Data.Maybe (Maybe(..))
import Gargantext.Config.REST (get)
import Network.HTTP.Affjax (AJAX)
import Thermite (PerformAction, modifyState)

getUser :: forall eff. Int -> Aff
        (console :: CONSOLE, ajax :: AJAX | eff) (Either String User)
getUser id = get $ "http://localhost:8008/node/" <> show id


performAction :: forall eff props. PerformAction ( console :: CONSOLE
                                                 , ajax    :: AJAX
                                                 , dom     :: DOM
                                                 | eff ) State props Action
performAction NoOp _ _ = void do
  modifyState id
performAction (FetchUser userId) _ _ = void do
  value <- lift $ getUser userId
  _ <- case value of
    (Right user) -> modifyState \state -> set _user (Just user) state
    (Left err) -> do
      _ <- lift $ log err
      modifyState id
  lift <<< log $ "Fetching user..."
performAction _ _ _ = void do
  modifyState id
