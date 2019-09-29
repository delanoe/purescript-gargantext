-- | A module for authenticating to create sessions and handling them
module Gargantext.Sessions where

import Prelude (class Eq, class Show, const, otherwise, pure, show, unit, ($), (*>), (<$>), (<>), (==), (>>=))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Maybe (Maybe(..))
import DOM.Simple.Console (log, log2)
import Effect (Effect)
import Effect.Aff (Aff)
import Reactix as R
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem, removeItem)
import Gargantext.Components.Login.Types
  (AuthRequest(..), AuthResponse(..), AuthInvalid(..), AuthData(..))
import Gargantext.Config.REST (post)
import Gargantext.Ends (class ToUrl, Backend, backendUrl, toUrl, sessionPath)
import Gargantext.Routes (SessionRoute)
import Gargantext.Types (NodePath, nodePath)
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

instance toUrlSessionString :: ToUrl Session String where
  toUrl = sessionUrl

newtype Sessions = Sessions (Maybe Session)

unSessions :: Sessions -> Maybe Session
unSessions (Sessions s) = s

useSessions :: R.Hooks (R2.Reductor Sessions Action)
useSessions = R2.useReductor actAndSave (const loadSessions) unit
  where
    actAndSave :: R2.Actor Sessions Action
    actAndSave s a = act s a >>= saveSessions

data Action
  = Login Session
  | Logout Session

act :: Sessions -> Action -> Effect Sessions
act _ (Login session) = pure $ Sessions (Just session)
act (Sessions s) (Logout session)
  | Just session == s = pure (Sessions Nothing)
  | Just s2 <- s = log2 "Alien session:" s2 *> pure (Sessions Nothing)
  | otherwise = log "Can't log out of nonexistent session" *> pure (Sessions Nothing)

-- Key we will store the data under
localStorageKey :: String
localStorageKey = "garg-sessions"

empty :: Sessions
empty = Sessions Nothing

-- True if there are no sessions stored
null :: Sessions -> Boolean
null (Sessions Nothing) = true
null _ = false

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
      | {inval: Just (AuthInvalid {message})} <- ar2 = Left message
      | {valid: Just (AuthData {token, tree_id})} <- ar2 =
          Right $ Session { backend, username, token, treeId: tree_id }
      | otherwise = Left "Invalid response from server"
