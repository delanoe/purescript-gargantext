module Gargantext.Pages.Corpus.User.Users.API where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Lens (set)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Gargantext.Config.REST (get)
import Gargantext.Pages.Corpus.User.Users.Types (Action(..), State, User, _user)
import Thermite (PerformAction, modifyState)

getUser :: Int -> Aff (Either String User)
getUser id = get $ "http://localhost:8008/node/" <> show id


performAction :: PerformAction State {} Action
performAction NoOp _ _ = void do
  modifyState identity
performAction (FetchUser userId) _ _ = void do
  value <- lift $ getUser userId
  _ <- case value of
    (Right user) -> modifyState \state -> set _user (Just user) state
    (Left err) -> do
      _ <- liftEffect $ log err
      modifyState identity
  liftEffect <<< log $ "Fetching user..."
performAction _ _ _ = void do
  modifyState identity
