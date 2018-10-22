module Gargantext.Pages.Annuaire.User.Contacts.API where

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
import Gargantext.Prelude
import Gargantext.Pages.Annuaire.User.Contacts.Types (Action(..), State, Contact, _contact)
import Thermite (PerformAction, modifyState)

getContact :: Int -> Aff Contact
getContact id = get $ toUrl Back Node id

fetchContact :: Int -> StateCoTransformer State Unit
fetchContact contactId = do
  contact <- lift $ getContact contactId
  void $ modifyState $ _contact ?~ contact
  logs "Fetching contact..."
