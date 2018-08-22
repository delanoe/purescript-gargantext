module Gargantext.Pages.Corpus.User.Users.API where

import Prelude

import Gargantext.Pages.Corpus.User.Users.Types (Action(..), State, User, _user)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Lens (set)
import Data.Maybe (Maybe(..))
import Gargantext.Config.REST (get)
import Thermite (PerformAction, modifyState)

getUser :: Int -> Aff (Either String User)
getUser id = get $ "http://localhost:8008/node/" <> show id


performAction :: forall props. PerformAction State props Action
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
