module Gargantext.Users.Types.Types where

import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Generic (class Generic)
import Prelude (bind, pure, ($))

newtype User =
  User {
    id :: Int,
    typename :: Int,
    userId :: Int,
    parentId :: Int,
    name :: String,
    date :: String,
    hyperData :: HyperData
       }

newtype HyperData =
  HyperData
  {
    bureau :: String,
    atel :: String,
    fax :: String,
    aprecision :: String,
    service :: String,
    service2 :: String,
    groupe :: String,
    lieu :: String,
    pservice :: String,
    date_modification :: String,
    fonction :: String,
    pfonction :: String,
    url :: String,
    prenom :: String,
    nom :: String,
    idutilentite :: String,
    afonction :: String,
    grprech :: String,
    entite :: String,
    entite2 :: String
  }

instance decodeUserHyperData :: DecodeJson HyperData where
  decodeJson json = do
    obj <- decodeJson json
    bureau <- obj .? "bureau"
    atel <- obj .? "atel"
    fax <- obj .? "fax"
    aprecision <- obj .? "aprecision"
    service <- obj .? "service"
    service2 <- obj .? "service2"
    groupe <- obj .? "groupe"
    lieu <- obj .? "lieu"
    pservice <- obj .? "pservice"
    date_modification  <- obj .? "date_modification"
    fonction <- obj .? "fonction"
    pfonction <- obj .? "pfonction"
    url <- obj .? "url"
    prenom <- obj .? "prenom"
    nom <- obj .? "nom"
    idutilentite <- obj .? "idutilentite"
    afonction <- obj .? "afonction"
    grprech <- obj .? "grprech"
    entite <- obj .? "entite"
    entite2 <- obj .? "entite2"
    pure $ HyperData {bureau, atel, fax, aprecision, service, service2, groupe, lieu, pservice, date_modification, fonction, pfonction, url, prenom, nom, idutilentite, afonction, grprech, entite, entite2}

instance decodeUser :: DecodeJson User where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    typename <- obj .? "typename"
    userId <- obj .? "userId"
    parentId <- obj .? "parentId"
    name <- obj .? "name"
    date <- obj .? "date"
    hyperData <- obj .? "hyperData"
    pure $ User {id, typename, userId, parentId, name, date, hyperData}
