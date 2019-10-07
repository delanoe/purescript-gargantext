-- | A module for authenticating to create sessions and handling them
module Gargantext.Sessions where

import Prelude (class Eq, class Show, Unit, const, otherwise, pure, show, unit, ($), (*>), (<*), (<$>), (<>), (==), (/=), (>>=), (<<<))
import Data.Array as A
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
import Gargantext.Config.REST (post)
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

newtype Sessions = Sessions (Seq Session)

derive instance genericSessions :: Generic Sessions _

instance eqSessions :: Eq Sessions where
  eq = genericEq

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

delete :: SessionId -> Sessions -> Sessions
delete sid (Sessions ss) = Sessions (Seq.filter f ss) where
  f s = sid /= sessionId s

tryDelete :: SessionId -> Sessions -> Either Unit Sessions
tryDelete sid old@(Sessions ss) = ret where
  new = delete sid old
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
  case tryDelete (sessionId s) old of
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
-- loadSessions = window >>= localStorage >>= getItem "auths" >>= traverse decode
--   where
--     decode :: String -> Effect (Maybe Sessions)
--     decode = ret <<< runExcept <<< decodeJSON
--     ret (Right v) = pure $ Just v
--     ret (Left e) = log2 "Error reading serialised sessions:" e *> pure (Malformed e)

saveSessions :: Sessions -> Effect Sessions
saveSessions sessions = effect *> pure sessions
  where
    effect
      | null sessions = window >>= localStorage >>= removeItem localStorageKey
      | otherwise = pure unit
-- | otherwise = window >>= localStorage >>= setItem localStorageKey (encodeJSON sessions)

postAuthRequest :: Backend -> AuthRequest -> Aff (Either String Session)
postAuthRequest backend ar@(AuthRequest {username}) =
  decode <$> post (toUrl backend "auth") ar
  where
    decode (AuthResponse ar2)
      | {inval: Just (AuthInvalid {message})}     <- ar2 = Left message
      | {valid: Just (AuthData {token, tree_id})} <- ar2 =
          Right $ Session { backend, username, token, treeId: tree_id }
      | otherwise = Left "Invalid response from server"
