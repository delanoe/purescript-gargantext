module Gargantext.Pages.Annuaire.User.Contacts.Types.Types where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Maybe (Maybe)
import Gargantext.Utils.DecodeMaybe ((.?|))

{-
newtype User =
User {
  id :: Int,
  ... fields for all the gargantext utilities
  authors :: [Author]
}

newtype Author =
Author {
  user :: Maybe User,
  name :: String,
  hyperdata :: [Map String String]
  ...
}

newtype Document =
Document {
  authors :: [Author],
  ...
}

So Users have many Author and Authors have one User. This relation permit to
retrieve all the authors of a user to create corpus with it.
It also permit to have multiple authors name to permit to retrace document signed with a nickname.
It will happend that we can't establish a link between an Author and a User, this is why
the "user" field is encapsulated in a Maybe.

-}

newtype Contact =
  Contact { id        :: Int
       , typename  :: Maybe Int
       , userId    :: Int
       , parentId  :: Int
       , name      :: String
       , date      :: Maybe String
       , hyperdata :: HyperData
       }

newtype HyperData =
  HyperData
  { bureau :: Maybe String
  , atel   :: Maybe String
  , fax    :: Maybe String
  , aprecision :: Maybe String
  , service    :: Maybe String
  , service2   :: Maybe String
  , groupe     :: Maybe String
  , lieu       :: Maybe String
  , pservice   :: Maybe String
  , date_modification :: Maybe String
  , fonction          :: Maybe String
  , pfonction         :: Maybe String
  , url               :: Maybe String
  , prenom            :: Maybe String
  , nom               :: Maybe String
  , idutilentite      :: Maybe String
  , afonction         :: Maybe String
  , grprech           :: Maybe String
  , entite            :: Maybe String
  , entite2           :: Maybe String
  , mail              :: Maybe String
  }

instance decodeUserHyperData :: DecodeJson HyperData where
  decodeJson json = do
    obj <- decodeJson json
    bureau <- obj .?| "bureau"
    atel   <- obj .?| "atel"
    fax    <- obj .?| "fax"
    aprecision <- obj .?| "aprecision"
    service    <- obj .?| "service"
    service2   <- obj .?| "service2"
    groupe     <- obj .?| "groupe"
    lieu       <- obj .?| "lieu"
    pservice   <- obj .?| "pservice"
    date_modification <- obj .?| "date_modification"
    fonction          <- obj .?| "fonction"
    pfonction         <- obj .?| "pfonction"
    url               <- obj .?| "url"
    prenom            <- obj .?| "prenom"
    nom               <- obj .?| "nom"
    idutilentite    <- obj .?| "idutilentite"
    afonction       <- obj .?| "afonction"
    grprech         <- obj .?| "grprech"
    entite          <- obj .?| "entite"
    entite2         <- obj .?| "entite2"
    mail            <- obj .?| "mail"
    pure $ HyperData { bureau, atel, fax
                     , aprecision, service
                     , service2, groupe, lieu
                     , pservice, date_modification
                     , fonction, pfonction, url
                     , prenom, nom, idutilentite
                     , afonction, grprech, entite
                     , entite2, mail
                     }

instance decodeUser :: DecodeJson Contact where
  decodeJson json = do
    obj      <- decodeJson json
    id       <- obj .? "id"
    typename <- obj .?| "typename"
    userId   <- obj .? "userId"
    parentId <- obj .? "parentId"
    name     <- obj .? "name"
    date     <- obj .?| "date"
    hyperdata <- obj .? "hyperdata"
    pure $ Contact { id, typename, userId
                , parentId, name, date
                , hyperdata
                }
