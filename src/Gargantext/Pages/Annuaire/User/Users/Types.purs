module Gargantext.Pages.Annuaire.User.Users.Types where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, prism)
import Data.Maybe (Maybe(..))

import Gargantext.Components.Tab as Tab
import Gargantext.Utils.DecodeMaybe ((.?|))

newtype User =
  User { id        :: Int
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

instance decodeUser :: DecodeJson User where
  decodeJson json = do
    obj      <- decodeJson json
    id       <- obj .? "id"
    typename <- obj .?| "typename"
    userId   <- obj .? "userId"
    parentId <- obj .? "parentId"
    name     <- obj .? "name"
    date     <- obj .?| "date"
    hyperdata <- obj .? "hyperdata"
    pure $ User { id, typename, userId
                , parentId, name, date
                , hyperdata
                }

data Action
  = TabA Tab.Action
  | FetchUser Int

type State =
  { activeTab :: Int
  , user :: Maybe User
  }

initialState :: State
initialState =
  { activeTab : 0
  , user: Nothing
  }

_user :: Lens' State (Maybe User)
_user = lens (\s -> s.user) (\s ss -> s{user = ss})

_tablens :: Lens' State Tab.State
_tablens = lens (\s -> s.activeTab) (\s ss -> s {activeTab = ss})

_tabAction :: Prism' Action Tab.Action
_tabAction = prism TabA \ action ->
  case action of
    TabA laction -> Right laction
    _-> Left action
