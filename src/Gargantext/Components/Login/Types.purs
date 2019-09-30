module Gargantext.Components.Login.Types where

import Prelude
import Data.Argonaut ( class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject
                      , (.:), (.??), (:=), (~>)
                      )
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Lens (Iso', iso)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

type Username = String
type Password = String
type Token    = String
type TreeId   = Int

newtype AuthRequest = AuthRequest
  { username :: Username
  , password :: Password
  }

newtype AuthResponse = AuthResponse
  { valid :: Maybe AuthData
  , inval :: Maybe AuthInvalid
  }

newtype AuthInvalid = AuthInvalid
  { message :: String }

newtype AuthData = AuthData
  { token   :: Token
  , tree_id :: TreeId
  }

derive instance genericAuthData :: Generic AuthData _

derive instance newtypeAuthData :: Newtype AuthData _

instance eqAuthData :: Eq AuthData where
  eq = genericEq

_AuthData :: Iso' AuthData { token :: Token, tree_id :: TreeId }
_AuthData = iso (\(AuthData v) -> v) AuthData

instance decodeAuthInvalid :: DecodeJson AuthInvalid where
  decodeJson json = do
    obj     <- decodeJson json
    message <- obj .: "message"
    pure $ AuthInvalid {message}

instance decodeAuthResponse :: DecodeJson AuthResponse where
  decodeJson json = do
    obj   <- decodeJson json
    valid <- obj .?? "valid"
    inval <- obj .?? "inval"
    pure $ AuthResponse {valid, inval}

instance decodeAuthData :: DecodeJson AuthData where
  decodeJson json = do
    obj   <- decodeJson json
    token <- obj .: "token"
    tree_id <- obj .: "tree_id"
    pure $ AuthData {token, tree_id}

instance encodeAuthRequest :: EncodeJson AuthRequest where
  encodeJson (AuthRequest {username, password}) =
       "username" := username
    ~> "password" := password
    ~> jsonEmptyObject
