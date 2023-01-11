module Gargantext.Components.GraphQL.Contact
  ( AnnuaireContact
  , annuaireContactQuery
  -- Lenses
  , _ac_title
  , _ac_source
  , _ac_firstName
  , _ac_lastName
  , _ac_labTeamDepts
  , _ac_labTeamDeptsFirst
  , _ac_organization
  , _ac_organizationFirst
  , _ac_role
  , _ac_office
  , _ac_country
  , _ac_city
  , _ac_touchMail
  , _ac_touchPhone
  , _ac_touchUrl
  ) where

import Gargantext.Prelude

import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..), fromMaybe)
import GraphQL.Client.Args (Args, (=>>))
import GraphQL.Client.Variable (Var(..))

import Data.Array as A

type AnnuaireContact
  = { ac_title        :: Maybe String
    , ac_source       :: Maybe String
    , ac_id           :: Int
    , ac_firstName    :: Maybe String
    , ac_lastName     :: Maybe String
    , ac_labTeamDepts :: Array String
    , ac_organization :: Array String
    , ac_role         :: Maybe String
    , ac_office       :: Maybe String
    , ac_country      :: Maybe String
    , ac_city         :: Maybe String
    , ac_touchMail    :: Maybe String
    , ac_touchPhone   :: Maybe String
    , ac_touchUrl     :: Maybe String
    }

type AnnuaireContactQuery
  = { annuaire_contacts   :: Args
      { contact_id        :: Var "id" Int }
      { ac_title          :: Unit
      , ac_source         :: Unit
      , ac_id             :: Unit
      , ac_firstName      :: Unit
      , ac_lastName       :: Unit
      , ac_labTeamDepts   :: Unit
      , ac_organization   :: Unit
      , ac_role           :: Unit
      , ac_office         :: Unit
      , ac_country        :: Unit
      , ac_city           :: Unit
      , ac_touchMail      :: Unit
      , ac_touchPhone     :: Unit
      , ac_touchUrl       :: Unit
      }
    }
annuaireContactQuery :: AnnuaireContactQuery
annuaireContactQuery
  = { annuaire_contacts:
      { contact_id: Var :: _ "id" Int } =>>
        { ac_title: unit
        , ac_source: unit
        , ac_id: unit
        , ac_firstName: unit
        , ac_lastName: unit
        , ac_labTeamDepts: unit
        , ac_organization: unit
        , ac_role: unit
        , ac_office: unit
        , ac_country: unit
        , ac_city: unit
        , ac_touchMail: unit
        , ac_touchPhone: unit
        , ac_touchUrl: unit
        }
    }

------------------------------------------------------------------------

_ac_title :: Lens' AnnuaireContact String
_ac_title = lens getter setter
  where
    getter ({ ac_title: val }) = fromMaybe "" val
    setter rec val = rec { ac_title = Just val }

_ac_source :: Lens' AnnuaireContact String
_ac_source = lens getter setter
  where
    getter ({ ac_source: val }) = fromMaybe "" val
    setter rec val = rec { ac_source = Just val }

_ac_firstName :: Lens' AnnuaireContact String
_ac_firstName = lens getter setter
  where
    getter ({ ac_firstName: val }) = fromMaybe "" val
    setter rec val = rec { ac_firstName = Just val }

_ac_lastName :: Lens' AnnuaireContact String
_ac_lastName = lens getter setter
  where
    getter ({ ac_lastName: val }) = fromMaybe "" val
    setter rec val = rec { ac_lastName = Just val }

_ac_labTeamDepts :: Lens' AnnuaireContact (Array String)
_ac_labTeamDepts = lens getter setter
  where
    getter ({ ac_labTeamDepts: val }) = val
    setter ac val = ac { ac_labTeamDepts = val }

_ac_labTeamDeptsFirst :: Lens' AnnuaireContact String
_ac_labTeamDeptsFirst = lens getter setter
  where
    getter ({ ac_labTeamDepts: val }) = fromMaybe "" $ A.head val
    setter ac val = ac { ac_labTeamDepts = fromMaybe [val] $ A.updateAt 0 val ac.ac_labTeamDepts }

_ac_organization :: Lens' AnnuaireContact (Array String)
_ac_organization = lens getter setter
  where
    getter ({ ac_organization: val }) = val
    setter ac val = ac { ac_organization = val }

_ac_organizationFirst :: Lens' AnnuaireContact String
_ac_organizationFirst = lens getter setter
  where
    getter ({ ac_organization: val }) = fromMaybe "" $ A.head val
    setter ac val = ac { ac_organization = fromMaybe [val] $ A.updateAt 0 val ac.ac_organization }

_ac_role :: Lens' AnnuaireContact String
_ac_role = lens getter setter
  where
    getter ({ ac_role: val }) = fromMaybe "" val
    setter rec val = rec { ac_role = Just val }

_ac_office :: Lens' AnnuaireContact String
_ac_office = lens getter setter
  where
    getter ({ ac_office: val }) = fromMaybe "" val
    setter rec val = rec { ac_office = Just val }

_ac_country :: Lens' AnnuaireContact String
_ac_country = lens getter setter
  where
    getter ({ ac_country: val }) = fromMaybe "" val
    setter rec val = rec { ac_country = Just val }

_ac_city :: Lens' AnnuaireContact String
_ac_city = lens getter setter
  where
    getter ({ ac_city: val }) = fromMaybe "" val
    setter rec val = rec { ac_city = Just val }

_ac_touchMail :: Lens' AnnuaireContact String
_ac_touchMail = lens getter setter
  where
    getter ({ ac_touchMail: val }) = fromMaybe "" val
    setter rec val = rec { ac_touchMail = Just val }

_ac_touchPhone :: Lens' AnnuaireContact String
_ac_touchPhone = lens getter setter
  where
    getter ({ ac_touchPhone: val }) = fromMaybe "" val
    setter rec val = rec { ac_touchPhone = Just val }

_ac_touchUrl :: Lens' AnnuaireContact String
_ac_touchUrl = lens getter setter
  where
    getter ({ ac_touchUrl: val }) = fromMaybe "" val
    setter rec val = rec { ac_touchUrl = Just val }
