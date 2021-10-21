module Gargantext.Components.GraphQL.User where

import Data.Maybe (Maybe(..), maybe)
import Gargantext.Prelude
import Type.Proxy (Proxy(..))


type User
  = { u_id        :: Int
    , u_hyperdata ::
         { _hu_shared :: Maybe
              { _hc_title :: Maybe String
              , _hc_source :: Maybe String
              , _hc_who :: Maybe
                { _cw_firstName :: Maybe String
                , _cw_lastName :: Maybe String
                }
              , _hc_where :: Array
                { _cw_organization :: Array String }
              }
         }
    , u_username  :: String
    , u_email     :: String
    }
showUser { u_id
         , u_username
         , u_email } = "[" <> show u_id <> "] " <> u_username <> " :: " <> u_email
showMUser u = maybe "" showUser u

-- Symbols 
u_id :: Proxy "u_id"
u_id = Proxy
u_hyperdata :: Proxy "u_hyperdata"
u_hyperdata = Proxy
u_username :: Proxy "u_username"
u_username = Proxy
u_email :: Proxy "u_email"
u_email = Proxy

_hu_shared :: Proxy "shared"
_hu_shared = Proxy
_hc_source :: Proxy "_hc_source"
_hc_source = Proxy
_hc_title :: Proxy "_hc_title"
_hc_title = Proxy
_hc_who :: Proxy "_hc_who"
_hc_who = Proxy
_hc_where :: Proxy "_cw_where"
_hc_where = Proxy
_cw_firstName :: Proxy "_cw_firstName"
_cw_firstName = Proxy
_cw_lastName :: Proxy "_cw_lastName"
_cw_lastName = Proxy
_cw_organization :: Proxy "_cw_organization"
_cw_organization = Proxy
