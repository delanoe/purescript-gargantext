module Gargantext.Components.Nodes.Annuaire.User.Contacts.Types where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, (.:), (.:!))
import Data.Array as A
import Data.Lens
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (Map)
import Data.Tuple (Tuple(..))
import Gargantext.Utils.DecodeMaybe ((.?|))
import Data.Newtype (class Newtype)

-- TODO: should it be a NodePoly HyperdataContact ?
newtype Contact =
  Contact
  { id :: Int
  , typename :: Maybe Int
  , userId :: Maybe Int
  , parentId :: Maybe Int
  , name :: Maybe String
  , date :: Maybe String
  , hyperdata :: HyperdataUser
  }

derive instance newtypeContact :: Newtype Contact _

newtype ContactWho =
  ContactWho
  { idWho     :: Maybe String
  , firstName :: Maybe String
  , lastName  :: Maybe String
  , keywords  :: (Array String)
  , freetags  :: (Array String) }

derive instance newtypeContactWho :: Newtype ContactWho _

instance decodeContactWho :: DecodeJson ContactWho
  where
    decodeJson json = do
      obj <- decodeJson json
      idWho  <- obj .:! "id"
      firstName <- obj .:! "firstName"
      lastName  <- obj .:! "lastName"
      keywords  <- obj .:! "keywords"
      freetags  <- obj .:! "freetags"

      let k = fromMaybe [] keywords
      let f = fromMaybe [] freetags

      pure $ ContactWho {idWho, firstName, lastName, keywords:k, freetags:f}

defaultContactWho :: ContactWho
defaultContactWho =
  ContactWho {
      idWho: Nothing
    , firstName: Nothing
    , lastName: Nothing
    , keywords: []
    , freetags: []
    }

newtype ContactWhere =
  ContactWhere
  { organization :: (Array String)
  , labTeamDepts :: (Array String)
                  
  , role         :: Maybe String
                    
  , office       :: Maybe String
  , country      :: Maybe String
  , city         :: Maybe String
                    
  , touch        :: Maybe ContactTouch
                    
  , entry        :: Maybe String
  , exit         :: Maybe String }

derive instance newtypeContactWhere :: Newtype ContactWhere _

instance decodeContactWhere :: DecodeJson ContactWhere
  where
    decodeJson json = do
      obj <- decodeJson json
      organization  <- obj .:! "organization"
      labTeamDepts  <- obj .:! "labTeamDepts"
      role          <- obj .:! "role"
      office        <- obj .:! "office"
      country       <- obj .:! "country"
      city          <- obj .:! "city"
      touch         <- obj .:! "touch"
      entry         <- obj .:! "entry"
      exit          <- obj .:! "exit"

      let o = fromMaybe [] organization
      let l = fromMaybe [] labTeamDepts

      pure $ ContactWhere {organization:o, labTeamDepts:l, role, office, country, city, touch, entry, exit}

defaultContactWhere :: ContactWhere
defaultContactWhere =
  ContactWhere {
    organization: []
  , labTeamDepts: []
  , role: Nothing
  , office: Nothing
  , country: Nothing
  , city: Nothing
  , touch: Nothing
  , entry: Nothing
  , exit: Nothing
  }

newtype ContactTouch =
  ContactTouch
  { mail  :: Maybe String
  , phone :: Maybe String
  , url   :: Maybe String }

derive instance newtypeContactTouch :: Newtype ContactTouch _

instance decodeContactTouch :: DecodeJson ContactTouch
  where
    decodeJson json = do
      obj <- decodeJson json
      mail  <- obj .:! "mail"
      phone <- obj .:! "phone"
      url   <- obj .:! "url"
      pure $ ContactTouch {mail, phone, url}


newtype HyperdataUser =
  HyperdataUser { shared :: Maybe HyperdataContact }
derive instance newtypeHyperdataUser :: Newtype HyperdataUser _

instance decodeHyperdataUser :: DecodeJson HyperdataUser
  where
    decodeJson json = do
      obj <- decodeJson json
      shared            <- obj .:! "shared"
      pure $ HyperdataUser { shared }

defaultHyperdataUser :: HyperdataUser
defaultHyperdataUser =
  HyperdataUser {
    shared: Just defaultHyperdataContact
  }


newtype HyperdataContact =
     HyperdataContact { bdd :: Maybe String
                      , who :: Maybe ContactWho
                      , ou  :: (Array ContactWhere)
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
      bdd            <- obj .:! "bdd"
      who            <- obj .:! "who"
      ou             <- obj .:! "where"
      title          <- obj .:! "title"
      source         <- obj .:! "source"
      lastValidation <- obj .:! "lastValidation"
      uniqId         <- obj .:! "uniqId"
      uniqIdBdd      <- obj .:! "uniqIdBdd"
      
      let ou' = fromMaybe [] ou

      pure $ HyperdataContact {bdd, who, ou:ou', title, source, lastValidation, uniqId, uniqIdBdd}

defaultHyperdataContact :: HyperdataContact
defaultHyperdataContact =
  HyperdataContact {
      bdd: Nothing
    , who: Nothing
    , ou: []
    , title: Nothing
    , source: Nothing
    , lastValidation: Nothing
    , uniqId: Nothing
    , uniqIdBdd: Nothing
    }


-- newtype HyperData c s =
--   HyperData
--   { common :: c
--   , shared :: s
--   , specific :: Map String String
--   }

-- instance decodeUserHyperData :: (DecodeJson c, DecodeJson s) =>
--                                 DecodeJson (HyperData c s) where
--   decodeJson json = do
--     common <- decodeJson json
--     shared <- decodeJson json
--     specific <- decodeJson json
--     pure $ HyperData {common, shared, specific}

instance decodeUser :: DecodeJson Contact where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    typename <- obj .?| "typename"
    userId <- obj .:! "userId"
    parentId <- obj .?| "parentId"
    name <- obj .:! "name"
    date <- obj .?| "date"
    hyperdata <- obj .: "hyperdata"
    pure $ Contact { id, typename, userId
                   , parentId, name, date
                   , hyperdata
                   }

type ContactData = {contactNode :: Contact, defaultListId :: Int}

_shared :: Lens' HyperdataUser HyperdataContact
_shared = lens getter setter
  where
    getter (HyperdataUser h@{shared}) = fromMaybe defaultHyperdataContact shared
    setter (HyperdataUser h) c = HyperdataUser $ h { shared = Just c }

_who :: Lens' HyperdataContact ContactWho
_who = lens getter setter
  where
    getter (HyperdataContact hc@{who}) = fromMaybe defaultContactWho who
    setter (HyperdataContact hc) w = HyperdataContact $ hc { who = Just w }
_ouFirst :: Lens' HyperdataContact ContactWhere
_ouFirst = lens getter setter
  where
    getter (HyperdataContact hc@{ou}) = fromMaybe defaultContactWhere $ A.head ou
    setter (HyperdataContact hc@{ou}) o = HyperdataContact $ hc { ou = fromMaybe [o] $ A.updateAt 0 o ou }

_lastName :: Lens' ContactWho String
_lastName = lens getter setter
  where
    getter (ContactWho cw@{lastName}) = fromMaybe "" lastName
    setter (ContactWho cw) ln = ContactWho $ cw { lastName = Just ln }
_firstName :: Lens' ContactWho String
_firstName = lens getter setter
  where
    getter (ContactWho cw@{firstName}) = fromMaybe "" firstName
    setter (ContactWho cw) fn = ContactWho $ cw { firstName = Just fn }
