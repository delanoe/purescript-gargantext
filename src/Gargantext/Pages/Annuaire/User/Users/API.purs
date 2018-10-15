module Gargantext.Pages.Annuaire.User.Users.API where

import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Thermite (StateCoTransformer, modifyState)

import Gargantext.Config (toUrl, NodeType(..), End(..))
import Gargantext.Config.REST (get)
import Gargantext.Pages.Annuaire.User.Users.Types (Action(..), State, User, _user)
import Gargantext.Prelude

getUser :: Int -> Aff User
getUser id = get $ toUrl Back Node id

fetchUser :: Int -> StateCoTransformer State Unit
fetchUser userId = do
  user <- lift $ getUser userId
  void $ modifyState $ _user ?~ user
  logs "Fetching user..."
