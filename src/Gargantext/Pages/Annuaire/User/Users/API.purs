module Gargantext.Pages.Annuaire.User.Users.API where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Gargantext.Config (toUrl, NodeType(..), End(..))
import Gargantext.Config.REST (get)
import Gargantext.Pages.Annuaire.User.Users.Types (Action(..), State, User, _user)
import Thermite (PerformAction, modifyState)

getUser :: Int -> Aff (Either String User)
getUser id = get $ toUrl Back Node id


performAction :: PerformAction State {} Action
performAction (FetchUser userId) _ _ = do
  value <- lift $ getUser userId
  _ <- case value of
    (Right user) -> void $ modifyState $ _user ?~ user
    (Left err) -> do
      liftEffect $ log err
  liftEffect <<< log $ "Fetching user..."
performAction _ _ _ = pure unit
