module Gargantext.Pages.Annuaire.User.Contacts.API where

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
import Gargantext.Pages.Annuaire.User.Contacts.Types (Action(..), State, Contact, _contact)
import Thermite (PerformAction, modifyState)

getUser :: Int -> Aff (Either String Contact)
getUser id = get $ toUrl Back Node id


performAction :: PerformAction State {} Action
performAction (FetchContact contactId) _ _ = do
  value <- lift $ getUser contactId
  _ <- case value of
    (Right contact) -> void $ modifyState $ _contact ?~ contact
    (Left err) -> do
      liftEffect $ log err
  liftEffect <<< log $ "Fetching contact..."
performAction _ _ _ = pure unit
