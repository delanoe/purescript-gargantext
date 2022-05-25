module Gargantext.Components.Login.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Lens (Iso', iso)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Simple.JSON as JSON

type Username = String
type Password = String
type Token    = String
type TreeId   = Int
type UserId   = Int

newtype AuthRequest = AuthRequest
  { username :: Username
  , password :: Password
  }
derive instance Generic AuthRequest _
derive instance Newtype AuthRequest _
derive newtype instance JSON.ReadForeign AuthRequest
derive newtype instance JSON.WriteForeign AuthRequest

newtype AuthResponse = AuthResponse
  { valid :: Maybe AuthData
  , inval :: Maybe AuthInvalid
  }
derive instance Generic AuthResponse _
derive instance Newtype AuthResponse _
derive newtype instance JSON.ReadForeign AuthResponse
derive newtype instance JSON.WriteForeign AuthResponse

newtype AuthInvalid = AuthInvalid { message :: String }
derive instance Generic AuthInvalid _
derive instance Newtype AuthInvalid _
derive newtype instance JSON.ReadForeign AuthInvalid
derive newtype instance JSON.WriteForeign AuthInvalid

newtype AuthData = AuthData
  { token   :: Token
  , tree_id :: TreeId
  , user_id :: UserId
  }
derive instance Generic AuthData _
derive instance Newtype AuthData _
derive newtype instance JSON.ReadForeign AuthData
derive newtype instance JSON.WriteForeign AuthData

instance Eq AuthData where
  eq = genericEq

_AuthData :: Iso' AuthData { token :: Token, tree_id :: TreeId, user_id :: UserId }
_AuthData = iso (\(AuthData v) -> v) AuthData

data FormType = Login | ForgotPassword
derive instance Generic FormType _
derive instance Eq FormType
