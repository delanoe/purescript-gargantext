module Gargantext.Sessions.Types
  ( Session(..), Sessions(..), OpenNodes, NodeId, mkNodeId
  , sessionUrl, sessionId
  , empty, null, unSessions, lookup, cons, tryCons, update, remove, tryRemove
  ) where

import Prelude
  ( class Eq, class Show, Unit, bind, otherwise, pure, show, unit
  , ($), (/=), (<$>), (<<<), (<>), (==), (>>=) )
import Data.Argonaut
  ( class DecodeJson, class EncodeJson, decodeJson, encodeJson
  , (:=), (~>), (.:) )
import Data.Argonaut.Core (Json, fromArray, jsonEmptyObject)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Array as A
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Set (Set)
import Data.Traversable (traverse)

import Gargantext.Components.Login.Types (TreeId)
import Gargantext.Components.Nodes.Lists.Types as NT
import Gargantext.Ends (class ToUrl, Backend(..), backendUrl, sessionPath)
import Gargantext.Routes (SessionRoute)
import Gargantext.Types (NodePath, SessionId(..), nodePath)

-- | A Session represents an authenticated session for a user at a
-- | backend. It contains a token and root tree id.
newtype Session = Session
  { backend  :: Backend
  , caches   :: Map Int NT.CacheState  -- whether cache is turned on for node id
  , token    :: String
  , treeId   :: TreeId
  , username :: String
  }

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

instance toUrlSessionString :: ToUrl Session String where
  toUrl = sessionUrl

sessionUrl :: Session -> String -> String
sessionUrl (Session {backend}) = backendUrl backend

sessionId :: Session -> SessionId
sessionId = SessionId <<< show

--------------------
-- | JSON instances
instance encodeJsonSession :: EncodeJson Session where
  encodeJson (Session { backend, caches, username, token, treeId })
    = "backend"  := encodeJson backend
    ~> "caches"   := encodeJson caches
    ~> "token"    := token
    ~> "treeId"   := treeId
    ~> "username" := username
    ~> jsonEmptyObject

instance decodeJsonSession :: DecodeJson Session where
  decodeJson json = do
    obj      <- decodeJson json
    backend  <- obj .: "backend"
    caches   <- obj .: "caches"
    token    <- obj .: "token"
    treeId   <- obj .: "treeId"
    username <- obj .: "username"
    pure $ Session { backend, caches, token, treeId, username }

------------------------------------------------------------------------

data Sessions = Sessions { sessions :: (Seq Session)}

derive instance genericSessions :: Generic Sessions _

instance eqSessions :: Eq Sessions where
  eq = genericEq

instance decodeJsonSessions :: DecodeJson Sessions where
  decodeJson json = do
    ss <- decodeSessions json
    pure (Sessions {sessions:Seq.fromFoldable ss})

    where
      decodeSessions :: Json -> Either JsonDecodeError (Array Session)
      decodeSessions json2 = decodeJson json2
                          >>= \obj -> obj .: "sessions"
                          >>= traverse decodeJson

instance encodeJsonSessions :: EncodeJson Sessions where
  encodeJson (Sessions {sessions:ss}) = "sessions" := (encodeSessions ss)
                           ~> jsonEmptyObject
    where
      encodeSessions :: Seq Session -> Json
      encodeSessions ss2 = fromArray $ encodeJson <$> (Seq.toUnfoldable ss2)

empty :: Sessions
empty = Sessions { sessions: Seq.empty }

-- True if there are no sessions stored
null :: Sessions -> Boolean
null (Sessions { sessions: seq }) = Seq.null seq

unSessions :: Sessions -> Array Session
unSessions (Sessions {sessions:s}) = A.fromFoldable s

lookup :: SessionId -> Sessions -> Maybe Session
lookup sid (Sessions {sessions:ss}) = Seq.head (Seq.filter f ss) where
  f s = sid == sessionId s

cons :: Session -> Sessions -> Sessions
cons s (Sessions {sessions:ss}) = Sessions {sessions:(Seq.cons s ss)}

tryCons :: Session -> Sessions -> Either Unit Sessions
tryCons s ss = try $ lookup sid ss
  where
    sid = sessionId s
    try Nothing = Right (cons s ss)
    try _ = Left unit

update :: Session -> Sessions -> Sessions
update s ss = up $ lookup sid ss
  where
    sid = sessionId s
    up Nothing = cons s ss
    up _ = cons s $ remove sid ss

remove :: SessionId -> Sessions -> Sessions
remove sid (Sessions {sessions:ss}) = Sessions {sessions: Seq.filter f ss} where
  f s = sid /= sessionId s

tryRemove :: SessionId -> Sessions -> Either Unit Sessions
tryRemove sid old@(Sessions ss) = ret where
  new = remove sid old
  ret
    | new == old = Left unit
    | otherwise =  Right new

-- open tree nodes data
type OpenNodes = Set NodeId

type NodeId =
  { treeId :: TreeId  -- Id of the node
  , baseUrl :: String -- the baseUrl of the backend
  }

mkNodeId :: Session -> TreeId -> NodeId
mkNodeId (Session {backend: Backend {baseUrl}}) treeId = { treeId, baseUrl }

