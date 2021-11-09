module Gargantext.Components.GraphQL.User where

import Gargantext.Prelude

import Data.Array as A
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import GraphQL.Client.Args (NotNull(..), (=>>))
import GraphQL.Client.Variable (Var)
import GraphQL.Client.Variables (withVars)
import Type.Proxy (Proxy(..))


type UserInfo
  = { ui_id             :: Int
    , ui_username       :: String
    , ui_email          :: String
    , ui_title          :: Maybe String
    , ui_source         :: Maybe String
    , ui_cwFirstName    :: Maybe String
    , ui_cwLastName     :: Maybe String
    , ui_cwOrganization :: Array String
    , ui_cwLabTeamDepts :: Array String
    , ui_cwOffice       :: Maybe String
    , ui_cwCity         :: Maybe String
    , ui_cwCountry      :: Maybe String
    , ui_cwRole         :: Maybe String
    , ui_cwTouchPhone   :: Maybe String
    , ui_cwTouchMail    :: Maybe String }
type UserInfoM
  = { ui_id             :: NotNull Int
    , ui_username       :: String
    , ui_email          :: String
    , ui_title          :: String
    , ui_source         :: String
    , ui_cwFirstName    :: String
    , ui_cwLastName     :: String
    , ui_cwOrganization :: (Array String)
    , ui_cwLabTeamDepts :: (Array String)
    , ui_cwOffice       :: String
    , ui_cwCity         :: String
    , ui_cwCountry      :: String
    , ui_cwRole         :: String
    , ui_cwTouchPhone   :: String
    , ui_cwTouchMail    :: String }
--defaultUserInfoM :: UserInfoM
--defaultUserInfoM =
--  { ui_id: NotNull 0
--  , ui_username: Nothing
--  , ui_email: Nothing
--  , ui_title: Nothing
--  , ui_source: Nothing
--  , ui_cwFirstName: Nothing
--  , ui_cwLastName: Nothing
--  , ui_cwOrganization: Nothing
--  , ui_cwLabTeamDepts: Nothing
--  , ui_cwOffice: Nothing
--  , ui_cwCity: Nothing
--  , ui_cwCountry: Nothing
--  , ui_cwRole: Nothing
--  , ui_cwTouchPhone: Nothing
--  , ui_cwTouchMail: Nothing }

userInfoQuery = { user_infos: { user_id: Var _ "id" Int } =>>
                  { ui_id: unit
                  , ui_username: unit
                  , ui_email: unit
                  , ui_title: unit
                  , ui_source: unit
                  , ui_cwFirstName: unit
                  , ui_cwLastName: unit
                  , ui_cwCity: unit
                  , ui_cwCountry: unit
                  , ui_cwLabTeamDepts: unit
                  , ui_cwOrganization: unit
                  , ui_cwOffice: unit
                  , ui_cwRole: unit
                  , ui_cwTouchMail: unit
                  , ui_cwTouchPhone: unit }
                }

_ui_cwFirstName :: Lens' UserInfo String
_ui_cwFirstName = lens getter setter
  where
    getter ({ ui_cwFirstName: val }) = fromMaybe "" val
    setter ui val = ui { ui_cwFirstName = Just val }
_ui_cwLastName :: Lens' UserInfo String
_ui_cwLastName = lens getter setter
  where
    getter ({ ui_cwLastName: val }) = fromMaybe "" val
    setter ui val = ui { ui_cwLastName = Just val }
_ui_cwCity :: Lens' UserInfo String
_ui_cwCity = lens getter setter
  where
    getter ({ ui_cwCity: val }) = fromMaybe "" val
    setter ui val = ui { ui_cwCity = Just val }
_ui_cwCountry :: Lens' UserInfo String
_ui_cwCountry = lens getter setter
  where
    getter ({ ui_cwCountry: val }) = fromMaybe "" val
    setter ui val = ui { ui_cwCountry = Just val }
_ui_cwLabTeamDepts :: Lens' UserInfo (Array String)
_ui_cwLabTeamDepts = lens getter setter
  where
    getter ({ ui_cwLabTeamDepts: val }) = val
    setter ui val = ui { ui_cwLabTeamDepts = val }
_ui_cwLabTeamDeptsFirst :: Lens' UserInfo String
_ui_cwLabTeamDeptsFirst = lens getter setter
  where
    getter ({ ui_cwLabTeamDepts: val }) = fromMaybe "" $ A.head val
    setter ui val = ui { ui_cwLabTeamDepts = fromMaybe [val] $ A.updateAt 0 val ui.ui_cwLabTeamDepts }
_ui_cwOffice :: Lens' UserInfo String
_ui_cwOffice = lens getter setter
  where
    getter ({ ui_cwOffice: val }) = fromMaybe "" val
    setter ui val = ui { ui_cwOffice = Just val }
_ui_cwOrganization :: Lens' UserInfo (Array String)
_ui_cwOrganization = lens getter setter
  where
    getter ({ ui_cwOrganization: val }) = val
    setter ui val = ui { ui_cwOrganization = val }
_ui_cwOrganizationFirst :: Lens' UserInfo String
_ui_cwOrganizationFirst = lens getter setter
  where
    getter ({ ui_cwOrganization: val }) = fromMaybe "" $ A.head val
    setter ui val = ui { ui_cwOrganization = fromMaybe [val] $ A.updateAt 0 val ui.ui_cwOrganization }
_ui_cwRole :: Lens' UserInfo String
_ui_cwRole = lens getter setter
  where
    getter ({ ui_cwRole: val }) = fromMaybe "" val
    setter ui val = ui { ui_cwRole = Just val }
_ui_cwTouchMail :: Lens' UserInfo String
_ui_cwTouchMail = lens getter setter
  where
    getter ({ ui_cwTouchMail: val }) = fromMaybe "" val
    setter ui val = ui { ui_cwTouchMail = Just val }
_ui_cwTouchPhone :: Lens' UserInfo String
_ui_cwTouchPhone = lens getter setter
  where
    getter ({ ui_cwTouchPhone: val }) = fromMaybe "" val
    setter ui val = ui { ui_cwTouchPhone = Just val }

type User
  = { u_id        :: Int
    , u_hyperdata ::
         { shared :: Maybe
              { title :: Maybe String
              , source :: Maybe String
              , who :: Maybe
                { firstName :: Maybe String
                , lastName :: Maybe String
                }
              , "where" :: Array
                { organization :: Array String }
              }
         }
    , u_username  :: String
    , u_email     :: String
    }
showUser { u_id
         , u_username
         , u_email } = "[" <> show u_id <> "] " <> u_username <> " :: " <> u_email
showMUser u = maybe "" showUser u
