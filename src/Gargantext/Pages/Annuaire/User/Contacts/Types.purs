module Gargantext.Pages.Annuaire.User.Contacts.Types where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, (.?), (.??))
import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, prism)
import Data.Maybe (Maybe(..), maybe)
import Data.Map (Map(..))

import React (ReactElement)
import React.DOM (div)

import Gargantext.Components.Tab as Tab
import Gargantext.Utils.DecodeMaybe ((.?|))

data Contact = Contact {
  id :: Int
  , typename :: Maybe Int
  , userId :: Maybe Int
  , parentId :: Maybe Int
  , name :: Maybe String
  , date :: Maybe String
  , hyperdata :: HyperdataContact
  }

data HyperdataContact =
     HyperdataContact { bdd :: Maybe String
                      , uniqId :: Maybe String
                      , uniqIdBdd :: Maybe String
                      , title :: Maybe String
                      , source :: Maybe String
                    }

instance decodeHyperdataContact :: DecodeJson HyperdataContact
  where
    decodeJson json = do
      obj <- decodeJson json
      bdd <- obj .?? "bdd"
      uniqId <- obj .?? "uniqId"
      uniqIdBdd <- obj .?? "uniqIdBdd"
      title     <- obj .?? "title"
      source    <- obj .?? "source"
      pure $ HyperdataContact {bdd, uniqId, uniqIdBdd, title, source}


data HyperData c s =
  HyperData
  { common :: c
  , shared :: s
  , specific :: Map String String
  }

instance decodeUserHyperData :: (DecodeJson c, DecodeJson s) =>
                                DecodeJson (HyperData c s) where
  decodeJson json = do
    common <- decodeJson json
    shared <- decodeJson json
    specific <- decodeJson json
    pure $ HyperData {common, shared, specific}

instance decodeUser :: DecodeJson Contact where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    typename <- obj .?| "typename"
    userId <- obj .?? "userId"
    parentId <- obj .?| "parentId"
    name <- obj .?? "name"
    date <- obj .?| "date"
    hyperdata <- obj .? "hyperdata"
    
    pure $ Contact { id, typename, userId
                   , parentId, name, date
                   , hyperdata
                   }

data Action
  = TabA Tab.Action
  | FetchContact Int

type State =
  { activeTab :: Int
  , contact :: Maybe Contact
  }

initialState :: State
initialState =
  { activeTab : 0
  , contact: Nothing
  }

_contact :: Lens' State (Maybe Contact)
_contact = lens (\s -> s.contact) (\s ss -> s{contact = ss})

_tablens :: Lens' State Tab.State
_tablens = lens (\s -> s.activeTab) (\s ss -> s {activeTab = ss})

_tabAction :: Prism' Action Tab.Action
_tabAction = prism TabA \ action ->
  case action of
    TabA laction -> Right laction
    _-> Left action
