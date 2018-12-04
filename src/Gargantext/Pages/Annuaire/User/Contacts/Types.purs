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
import Data.Newtype

newtype Contact = Contact {
  id :: Int
  , typename :: Maybe Int
  , userId :: Maybe Int
  , parentId :: Maybe Int
  , name :: Maybe String
  , date :: Maybe String
  , hyperdata :: HyperdataContact
  }

derive instance newtypeContact :: Newtype Contact _

newtype ContactWho =
     ContactWho { idWho     :: Maybe String
                , firstName :: Maybe String
                , lastName  :: Maybe String
                , keywords  :: Maybe (Array String)
                , freetags  :: Maybe (Array String)
                }

derive instance newtypeContactWho :: Newtype ContactWho _

instance decodeContactWho :: DecodeJson ContactWho
  where
    decodeJson json = do
      obj <- decodeJson json
      idWho  <- obj .?? "id"
      firstName <- obj .?? "firstName"
      lastName  <- obj .?? "lastName"
      keywords  <- obj .?? "keywords"
      freetags  <- obj .?? "freetags"
      pure $ ContactWho {idWho, firstName, lastName, keywords, freetags}

newtype ContactWhere =
     ContactWhere { organization :: Maybe (Array String)
                  , labTeamDepts :: Maybe (Array String)
                  
                  , role         :: Maybe String
                  
                  , office       :: Maybe String
                  , country      :: Maybe String
                  , city         :: Maybe String
                  
                  , touch        :: Maybe ContactTouch
                  
                  , entry        :: Maybe String
                  , exit         :: Maybe String
  }
derive instance newtypeContactWhere :: Newtype ContactWhere _

instance decodeContactWhere :: DecodeJson ContactWhere
  where
    decodeJson json = do
      obj <- decodeJson json
      organization  <- obj .?? "organization"
      labTeamDepts  <- obj .?? "labTeamDepts"
      role          <- obj .?? "role"
      office        <- obj .?? "office"
      country       <- obj .?? "country"
      city          <- obj .?? "city"
      touch         <- obj .?? "touch"
      entry         <- obj .?? "entry"
      exit          <- obj .?? "exit"
      pure $ ContactWhere {organization, labTeamDepts, role, office, country, city, touch, entry, exit}

newtype ContactTouch =
     ContactTouch { mail      :: Maybe String
                  , phone     :: Maybe String
                  , url       :: Maybe String
  }
derive instance newtypeContactTouch :: Newtype ContactTouch _

instance decodeContactTouch :: DecodeJson ContactTouch
  where
    decodeJson json = do
      obj <- decodeJson json
      mail  <- obj .?? "mail"
      phone <- obj .?? "phone"
      url   <- obj .?? "url"
      pure $ ContactTouch {mail, phone, url}


newtype HyperdataContact =
     HyperdataContact { bdd :: Maybe String
                      , who :: Maybe ContactWho
                      , ou  :: Maybe (Array ContactWhere)
                      , title :: Maybe String
                      , source :: Maybe String
                      , lastValidation :: Maybe String
                      , uniqId :: Maybe String
                      , uniqIdBdd :: Maybe String
                    }
derive instance newtypeHyperdataContact :: Newtype HyperdataContact _

instance decodeHyperdataContact :: DecodeJson HyperdataContact
  where
    decodeJson json = do
      obj <- decodeJson json
      bdd            <- obj .?? "bdd"
      who            <- obj .?? "who"
      ou             <- obj .?? "where"
      title          <- obj .?? "title"
      source         <- obj .?? "source"
      lastValidation <- obj .?? "lastValidation"
      uniqId         <- obj .?? "uniqId"
      uniqIdBdd      <- obj .?? "uniqIdBdd"
      
      pure $ HyperdataContact {bdd, who, ou, title, source, lastValidation, uniqId, uniqIdBdd}


newtype HyperData c s =
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
