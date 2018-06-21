module Users.Types.Types where

import Brevets as B
import Control.Monad.Aff.Console (CONSOLE)
import DOM (DOM)
import Data.Date (Date)
import Data.Generic (class Generic)
import Network.HTTP.Affjax (AJAX)
import Prelude (id, void)
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

type User =
  {
    id :: Int,
    typename :: Int,
    userId :: Int,
    parentId :: Int,
    name :: String,
    date :: Date,
    hyperData :: HyperData
  }

type HyperData =
  {}
