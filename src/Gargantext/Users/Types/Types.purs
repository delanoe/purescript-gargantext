module Gargantext.Users.Types.Types where

import Brevets as B
import Control.Monad.Aff.Console (CONSOLE)
import DOM (DOM)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Network.HTTP.Affjax (AJAX)
import Prelude (bind, id, pure, void, ($))
import Projects as PS
import Publications as P
import Tab as Tab
import Thermite (PerformAction, modifyState)

type State =
  { activeTab :: Int
  , publications :: P.State
  , brevets :: B.State
  , projects :: PS.State
  }

initialState :: State
initialState =
  { activeTab : 0
  , publications : P.initialState
  , brevets : B.initialState
  , projects : PS.initialState
  }

data Action
  = NoOp
  | PublicationA P.Action
  | BrevetsA B.Action
  | ProjectsA PS.Action
  | TabA Tab.Action

performAction :: forall eff props. PerformAction ( console :: CONSOLE
                                                 , ajax    :: AJAX
                                                 , dom     :: DOM
                                                 | eff ) State props Action
performAction NoOp _ _ = void do
  modifyState id
performAction _ _ _ = void do
  modifyState id


-- id	452146
-- typename	41
-- userId	1
-- parentId	452132
-- name	"Pierre DEMUGNIER"
-- date	"2018-06-18T14:27:50.670952Z"
-- hyperdata
-- bureau	"V.305"
-- atel	""
-- fax	""
-- aprecision	""
-- service2	null
-- groupe	""
-- service	"ARMINES"
-- lieu	"Paris"
-- pservice	""
-- date_modification	"07/06/2018"
-- pfonction	""
-- fonction	"Service Développement Partenarial - Chargé de Projet France & International"
-- url	""
-- statut	null
-- prenom	"Pierre"
-- idutilentite	"1055"
-- afonction	""
-- grprech	""
-- nom	"DEMUGNIER"
-- entite	"Armines"
-- entite2	null
-- id	"11002"
-- tel	"01.40.51.93.66"
-- idutilsiecoles	null
-- groupe2	null
-- sexe	"1"
-- mail	"pierre.demugnier@mines-paristech.fr"
-- actif	"1"

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
