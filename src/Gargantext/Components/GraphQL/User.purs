module Gargantext.Components.GraphQL.User where

import Data.Maybe (Maybe(..), maybe)
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types (HyperdataUser)
import Gargantext.Prelude
import Type.Proxy (Proxy(..))


type User
  = { u_id        :: Int
    , u_hyperdata :: HyperdataUser
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
