module Gargantext.Components.GraphQL.User where

import Data.Maybe (Maybe(..), maybe)
import Gargantext.Prelude
import Type.Proxy (Proxy(..))


type User
  = { userLight_id :: Int
    , userLight_username :: String
    , userLight_password :: String
    , userLight_email :: String
    }
showUser { userLight_id
         , userLight_username
         , userLight_password
         , userLight_email } = "[" <> show userLight_id <> "] " <> userLight_username <> " :: " <> userLight_email
showMUser u = maybe "" showUser u

-- Symbols 
userLight_id :: Proxy "userLight_id"
userLight_id = Proxy
userLight_username :: Proxy "userLight_username"
userLight_username = Proxy
userLight_password :: Proxy "userLight_password"
userLight_password = Proxy
userLight_email :: Proxy "userLight_email"
userLight_email = Proxy
