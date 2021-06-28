module Gargantext.Sessions.Types
  ( Session(..), Sessions(..), OpenNodes, NodeId, mkNodeId
  , sessionUrl, sessionId
  , empty, null, unSessions, lookup, cons, tryCons, update, remove, tryRemove
  ) where

import Data.Array as A
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Set (Set)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Simple.JSON as JSON

import Gargantext.Prelude

import Gargantext.Components.Login.Types (TreeId)
import Gargantext.Components.Nodes.Lists.Types as NT
import Gargantext.Ends (class ToUrl, Backend(..), backendUrl, sessionPath)
import Gargantext.Routes (SessionRoute)
import Gargantext.Types (NodePath, SessionId(..), nodePath)
import Gargantext.Utils.JSON as GJSON
import Gargantext.Utils.String as GUS
import Gargantext.Utils.Tuple as GUT

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

derive instance Generic Session _
derive instance Newtype Session _
instance JSON.ReadForeign Session where
  readImpl f = do
    r <- JSON.readImpl f
    let objTuple = Object.toUnfoldable r.caches :: Array (Tuple String NT.CacheState)
    let rUp = r { caches = Map.fromFoldable (GUT.mapFst (fromMaybe 0 <<< Int.fromString) <$> objTuple) }
    pure $ Session rUp
instance JSON.WriteForeign Session where
  writeImpl (Session { backend, caches, token, treeId, username }) =
      JSON.writeImpl { backend, caches: caches', token, treeId, username }
    where
      caches' = JSON.writeImpl $ Object.fromFoldable (GUT.mapFst show <$> Map.toUnfoldable caches :: Array (Tuple String NT.CacheState))

instance Eq Session where
  eq = genericEq

instance Show Session where
  show (Session {backend, username}) = username <> "@" <> show backend

instance ToUrl Session SessionRoute where
  toUrl (Session {backend}) r = backendUrl backend (sessionPath r)

instance ToUrl Session NodePath where
  toUrl (Session {backend}) np = backendUrl backend (nodePath np)

instance ToUrl Session String where
  toUrl = sessionUrl

sessionUrl :: Session -> String -> String
sessionUrl (Session {backend}) = backendUrl backend

sessionId :: Session -> SessionId
sessionId = SessionId <<< show
------------------------------------------------------------------------

newtype Sessions = Sessions { sessions :: Seq Session }

derive instance Generic Sessions _
derive instance Newtype Sessions _
instance JSON.ReadForeign Sessions where
  readImpl f = do
    sessions <- GJSON.readSequence f
    pure $ Sessions { sessions }
instance JSON.WriteForeign Sessions where
  writeImpl (Sessions { sessions }) = GJSON.writeSequence sessions

instance Eq Sessions where
  eq = genericEq

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

