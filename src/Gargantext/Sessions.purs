-- | A module for authenticating to create sessions and handling them
module Gargantext.Sessions where

import Prelude (class Eq, class Show, Unit, const, otherwise, pure, show, unit, ($), (*>), (<*), (<$>), (<>), (==), (/=), (>>=), (<<<), bind)
import Data.Argonaut ( class DecodeJson, decodeJson, class EncodeJson, encodeJson, (:=), (~>), jsonEmptyObject, (.:), Json, fromArray)
import Data.Array as A
import Data.Traversable (traverse)
import DOM.Simple.Console (log2)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import Data.Sequence (Seq)
import Effect (Effect)
import Effect.Aff (Aff)
import Reactix as R
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (removeItem) -- (getItem, setItem, removeItem)
import Gargantext.Components.Login.Types
  (AuthRequest(..), AuthResponse(..), AuthInvalid(..), AuthData(..))
import Gargantext.Config.REST as REST
import Gargantext.Ends (class ToUrl, Backend, backendUrl, toUrl, sessionPath)
import Gargantext.Routes (SessionRoute)
import Gargantext.Types (NodePath, SessionId(..), nodePath)
import Gargantext.Utils.Reactix as R2


-- | A Session represents an authenticated session for a user at a
-- | backend. It contains a token and root tree id.
newtype Session = Session
  { backend  :: Backend
  , username :: String
  , token    :: String
  , treeId   :: Int }

------------------------------------------------------------------------
-- | Main instances

derive instance genericSession :: Generic Session _

instance eqSession :: Eq Session where
  eq = genericEq

instance showSession :: Show Session where
  show (Session {backend, username}) = username <> "@" <> show backend

instance toUrlSessionRoute :: ToUrl Session SessionRoute where
  toUrl (Session {backend}) r = backendUrl backend (sessionPath r)

instance toUrlSessionNodePath :: ToUrl Session NodePath where
  toUrl (Session {backend}) np = backendUrl backend (nodePath np)

sessionUrl :: Session -> String -> String
sessionUrl (Session {backend}) = backendUrl backend

sessionId :: Session -> SessionId
sessionId = SessionId <<< show

instance toUrlSessionString :: ToUrl Session String where
  toUrl = sessionUrl

--------------------
-- | JSON instances
instance encodeJsonSession :: EncodeJson Session where
  encodeJson (Session {backend, username, token, treeId})
    =  "backend"  := encodeJson backend
    ~> "username" := username
    ~> "token"    :=  token
    ~> "treeId"   := treeId
    ~> jsonEmptyObject

instance decodeJsonSession :: DecodeJson Session where
  decodeJson json = do
    obj      <- decodeJson json
    backend  <- obj .: "backend"
    username <- obj .: "username"
    token    <- obj .: "token"
    treeId   <- obj .: "treeId"
    pure $ Session { backend, username, token, treeId}

------------------------------------------------------------------------

newtype Sessions = Sessions (Seq Session)

derive instance genericSessions :: Generic Sessions _

instance eqSessions :: Eq Sessions where
  eq = genericEq

instance decodeJsonSessions :: DecodeJson Sessions where
  decodeJson json = do
    ss <- decodeSessions json
    pure (Sessions (Seq.fromFoldable ss))
    
    where
      decodeSessions :: Json -> Either String (Array Session)
      decodeSessions json = decodeJson json >>= traverse decodeJson

instance encodeJsonSessions :: EncodeJson Sessions where
  encodeJson (Sessions ss) = "sessions" := (encodeSessions ss)
                           ~> jsonEmptyObject
    where
      encodeSessions :: Seq Session -> Json
      encodeSessions ss2 = fromArray $ encodeJson <$> (Seq.toUnfoldable ss2)

unSessions :: Sessions -> Array Session
unSessions (Sessions s) = A.fromFoldable s

useSessions :: R.Hooks (R2.Reductor Sessions Action)
useSessions = R2.useReductor actAndSave (const loadSessions) unit
  where
    actAndSave :: R2.Actor Sessions Action
    actAndSave s a = act s a >>= saveSessions

lookup :: SessionId -> Sessions -> Maybe Session
lookup sid (Sessions ss) = Seq.head (Seq.filter f ss) where
  f s = sid == sessionId s

cons :: Session -> Sessions -> Sessions
cons s (Sessions ss) = Sessions (Seq.cons s ss)

tryCons :: Session -> Sessions -> Either Unit Sessions
tryCons s ss = try (lookup sid ss) where
  sid = sessionId s
  try Nothing = Right (cons s ss)
  try _ = Left unit

remove :: SessionId -> Sessions -> Sessions
remove sid (Sessions ss) = Sessions (Seq.filter f ss) where
  f s = sid /= sessionId s

tryRemove :: SessionId -> Sessions -> Either Unit Sessions
tryRemove sid old@(Sessions ss) = ret where
  new = remove sid old
  ret
    | new == old = Left unit
    | otherwise =  Right new

data Action
  = Login Session
  | Logout Session

act :: Sessions -> Action -> Effect Sessions
act ss (Login s) =
  case tryCons s ss of
    Right new -> pure new
    _ -> pure ss <* log2 "Cannot overwrite existing session: " (sessionId s)
act old@(Sessions ss) (Logout s) =
  case tryRemove (sessionId s) old of
    Right new -> pure $ new
    _ -> pure old <* log2 "Logged out of stale session:" (sessionId s)
       

-- Key we will store the data under
localStorageKey :: String
localStorageKey = "garg-sessions"

empty :: Sessions
empty = Sessions Seq.empty

-- True if there are no sessions stored
null :: Sessions -> Boolean
null (Sessions seq) = Seq.null seq

-- | Will attempt to load saved sessions from localstorage. should log if decoding fails
loadSessions :: Effect Sessions
loadSessions = pure empty
{-
loadSessions = window >>= localStorage >>= getItem "auths" >>= traverse decode
  where
    decode :: String -> Effect (Maybe Sessions)
    decode = ret <<< runExcept <<< decodeJson
    ret (Right v) = pure $ Just v
    ret (Left e)  = log2 "Error reading serialised sessions:" e *> pure (Malformed e)
    -}
saveSessions :: Sessions -> Effect Sessions
saveSessions sessions = effect *> pure sessions
  where
    effect
      | null sessions = window >>= localStorage >>= removeItem localStorageKey
      | otherwise = pure unit
-- | otherwise = window >>= localStorage >>= setItem localStorageKey (encodeJSON sessions)

postAuthRequest :: Backend -> AuthRequest -> Aff (Either String Session)
postAuthRequest backend ar@(AuthRequest {username}) =
  decode <$> REST.post Nothing (toUrl backend "auth") ar
  where
    decode (AuthResponse ar2)
      | {inval: Just (AuthInvalid {message})}     <- ar2 = Left message
      | {valid: Just (AuthData {token, tree_id})} <- ar2 =
          Right $ Session { backend, username, token, treeId: tree_id }
      | otherwise = Left "Invalid response from server"

get :: forall a p. DecodeJson a => ToUrl Session p => Session -> p -> Aff a
get session@(Session {token}) p = REST.get (Just token) (toUrl session p)

put :: forall a b p. EncodeJson a => DecodeJson b => ToUrl Session p => Session -> p -> a -> Aff b
put session@(Session {token}) p = REST.put (Just token) (toUrl session p)

delete :: forall a p. DecodeJson a => ToUrl Session p => Session -> p -> Aff a
delete session@(Session {token}) p = REST.delete (Just token) (toUrl session p)

-- This might not be a good idea:
-- https://stackoverflow.com/questions/14323716/restful-alternatives-to-delete-request-body
deleteWithBody :: forall a b p. EncodeJson a => DecodeJson b => ToUrl Session p => Session -> p -> a -> Aff b
deleteWithBody session@(Session {token}) p = REST.deleteWithBody (Just token) (toUrl session p)

post :: forall a b p. EncodeJson a => DecodeJson b => ToUrl Session p => Session -> p -> a -> Aff b
post session@(Session {token}) p = REST.post (Just token) (toUrl session p)

postWwwUrlencoded :: forall b p. DecodeJson b => ToUrl Session p => Session -> p -> String -> Aff b
postWwwUrlencoded session@(Session {token}) p = REST.postWwwUrlencoded (Just token) (toUrl session p)
