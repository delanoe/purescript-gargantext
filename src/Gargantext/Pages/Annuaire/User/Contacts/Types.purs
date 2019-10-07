module Gargantext.Pages.Annuaire.User.Contacts.Types where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson, (.:), (.??))
import Data.Maybe (Maybe, fromMaybe)
import Data.Map (Map)
import Gargantext.Utils.DecodeMaybe ((.?|))
import Data.Newtype (class Newtype)

-- TODO: should it be a NodePoly HyperdataContact ?
newtype Contact
  = Contact
  { id :: Int
  , typename :: Maybe Int
  , userId :: Maybe Int
  , parentId :: Maybe Int
  , name :: Maybe String
  , date :: Maybe String
  , hyperdata :: HyperdataContact
  }

derive instance newtypeContact :: Newtype Contact _

newtype ContactWho
  = ContactWho
  { idWho :: Maybe String
  , firstName :: Maybe String
  , lastName :: Maybe String
  , keywords :: (Array String)
  , freetags :: (Array String)
  }

derive instance newtypeContactWho :: Newtype ContactWho _

instance decodeContactWho :: DecodeJson ContactWho where
  decodeJson json = do
    obj <- decodeJson json
    idWho <- obj .?? "id"
    firstName <- obj .?? "firstName"
    lastName <- obj .?? "lastName"
    keywords <- obj .?? "keywords"
    freetags <- obj .?? "freetags"
    let
      k = fromMaybe [] keywords
    let
      f = fromMaybe [] freetags
    pure $ ContactWho { idWho, firstName, lastName, keywords: k, freetags: f }

newtype ContactWhere
  = ContactWhere
  { organization :: (Array String)
  , labTeamDepts :: (Array String)
  , role :: Maybe String
  , office :: Maybe String
  , country :: Maybe String
  , city :: Maybe String
  , touch :: Maybe ContactTouch
  , entry :: Maybe String
  , exit :: Maybe String
  }

derive instance newtypeContactWhere :: Newtype ContactWhere _

instance decodeContactWhere :: DecodeJson ContactWhere where
  decodeJson json = do
    obj <- decodeJson json
    organization <- obj .?? "organization"
    labTeamDepts <- obj .?? "labTeamDepts"
    role <- obj .?? "role"
    office <- obj .?? "office"
    country <- obj .?? "country"
    city <- obj .?? "city"
    touch <- obj .?? "touch"
    entry <- obj .?? "entry"
    exit <- obj .?? "exit"
    let
      o = fromMaybe [] organization
    let
      l = fromMaybe [] labTeamDepts
    pure $ ContactWhere { organization: o, labTeamDepts: l, role, office, country, city, touch, entry, exit }

newtype ContactTouch
  = ContactTouch
  { mail :: Maybe String
  , phone :: Maybe String
  , url :: Maybe String
  }

derive instance newtypeContactTouch :: Newtype ContactTouch _

instance decodeContactTouch :: DecodeJson ContactTouch where
  decodeJson json = do
    obj <- decodeJson json
    mail <- obj .?? "mail"
    phone <- obj .?? "phone"
    url <- obj .?? "url"
    pure $ ContactTouch { mail, phone, url }

newtype HyperdataContact
  = HyperdataContact
  { bdd :: Maybe String
  , who :: Maybe ContactWho
  , ou :: (Array ContactWhere)
  , title :: Maybe String
  , source :: Maybe String
  , lastValidation :: Maybe String
  , uniqId :: Maybe String
  , uniqIdBdd :: Maybe String
  }

derive instance newtypeHyperdataContact :: Newtype HyperdataContact _

instance decodeHyperdataContact :: DecodeJson HyperdataContact where
  decodeJson json = do
    obj <- decodeJson json
    bdd <- obj .?? "bdd"
    who <- obj .?? "who"
    ou <- obj .?? "where"
    title <- obj .?? "title"
    source <- obj .?? "source"
    lastValidation <- obj .?? "lastValidation"
    uniqId <- obj .?? "uniqId"
    uniqIdBdd <- obj .?? "uniqIdBdd"
    let
      ou' = fromMaybe [] ou
    pure $ HyperdataContact { bdd, who, ou: ou', title, source, lastValidation, uniqId, uniqIdBdd }

newtype HyperData c s
  = HyperData
  { common :: c
  , shared :: s
  , specific :: Map String String
  }

instance decodeUserHyperData ::
  (DecodeJson c, DecodeJson s) =>
  DecodeJson (HyperData c s) where
  decodeJson json = do
    common <- decodeJson json
    shared <- decodeJson json
    specific <- decodeJson json
    pure $ HyperData { common, shared, specific }

instance decodeUser :: DecodeJson Contact where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    typename <- obj .?| "typename"
    userId <- obj .?? "userId"
    parentId <- obj .?| "parentId"
    name <- obj .?? "name"
    date <- obj .?| "date"
    hyperdata <- obj .: "hyperdata"
    pure
      $ Contact
          { id
          , typename
          , userId
          , parentId
          , name
          , date
          , hyperdata
          }

type ContactData
  = { contactNode :: Contact, defaultListId :: Int }
