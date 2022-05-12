-- | A module for authenticating to create sessions and handling them
module Gargantext.Sessions
  ( module Gargantext.Sessions.Types
  , WithSession, WithSessionContext
  , load, change
  , Action(..), act, delete, get, post, put, put_
  , postAuthRequest, postForgotPasswordRequest
  , deleteWithBody, postWwwUrlencoded
  , getCacheState, setCacheState
  ) where

import DOM.Simple.Console (log2)
import Data.Either (Either(..), hush)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Reactix as R
import Simple.JSON as JSON
import Toestand as T
import Web.Storage.Storage (getItem, removeItem, setItem)

import Gargantext.Prelude

import Gargantext.Components.Login.Types (AuthData(..), AuthInvalid(..), AuthRequest(..), AuthResponse(..))
import Gargantext.Components.Nodes.Lists.Types as NT
import Gargantext.Config.REST as REST
import Gargantext.Ends (class ToUrl, Backend, toUrl)
import Gargantext.Sessions.Types (Session(..), Sessions(..), OpenNodes, NodeId, mkNodeId, sessionUrl, sessionId, empty, null, unSessions, lookup, cons, tryCons, update, remove, tryRemove)
import Gargantext.Utils.Reactix as R2

type WithSession c =
  ( session :: Session
  | c )

type WithSessionContext c =
  ( session :: R.Context Session
  | c )

load :: forall c. T.Write c Sessions => c -> Effect Sessions
load cell = do
  sessions <- loadSessions
  T.write sessions cell

change
  :: forall c
   . T.Read  c Sessions
  => T.Write c Sessions
  => Action -> c -> Effect Sessions
change action cell = do
  cur <- T.read cell
  new <- act cur action
  saveSessions new *> T.write new cell

data Action
  = Login Session
  | Logout Session
  | Update Session

act :: Sessions -> Action -> Effect Sessions
act ss (Login s) =
  case tryCons s ss of
    Right new -> pure new
    _ -> pure ss <* log2 "Cannot overwrite existing session: " (sessionId s)
act old@(Sessions ss) (Logout s) =
  case tryRemove (sessionId s) old of
    Right new -> pure $ new
    _ -> pure old <* log2 "Logged out of stale session:" (sessionId s)
act ss (Update s) = saveSessions $ update s ss

-- Key we will store the data under
localStorageKey :: String
localStorageKey = "garg-sessions"

getCacheState :: NT.CacheState -> Session -> Int -> NT.CacheState
getCacheState defaultCacheState (Session { caches }) nodeId =
  fromMaybe defaultCacheState $ Map.lookup nodeId caches

setCacheState :: Session -> Int -> NT.CacheState -> Session
setCacheState (Session session@{ caches }) nodeId cacheState =
  Session $ session { caches = Map.insert nodeId cacheState caches }

-- | Will attempt to load saved sessions from localstorage. should log
-- | if decoding fails
loadSessions :: Effect Sessions
loadSessions = do
  storage <- R2.getls
  mItem :: Maybe String <- getItem localStorageKey storage
  case mItem of
    Nothing -> pure empty
    Just val -> do
      let r = JSON.readJSON val
      case hush r of
        Nothing -> pure empty
        Just p -> pure p
-- loadSessions = R2.getls >>= getItem localStorageKey >>= handleMaybe
--   where
--     -- a localstorage lookup can find nothing
--     handleMaybe (Just val) = handleEither (JSON.readJSON val)
--     handleMaybe Nothing    = pure empty

--     -- either parsing or decoding could fail, hence two errors
--     handleEither (Left err) = err *> pure empty
--     handleEither (Right ss) = pure ss

saveSessions :: Sessions -> Effect Sessions
saveSessions sessions = effect *> pure sessions where
  rem = R2.getls >>= removeItem localStorageKey
  set v  = R2.getls >>= setItem    localStorageKey v
  effect
    | null sessions = rem
    | otherwise = set (JSON.writeJSON sessions)

postAuthRequest :: Backend -> AuthRequest -> Aff (Either String Session)
postAuthRequest backend ar@(AuthRequest {username}) =
  decode <$> REST.post Nothing (toUrl backend "auth") ar
  where
    decode (Left _err) = Left "Error when sending REST.post"
    decode (Right (AuthResponse ar2))
      | {inval: Just (AuthInvalid {message})}     <- ar2 = Left message
      | {valid: Just (AuthData {token, tree_id, user_id})} <- ar2 =
          Right $ Session { backend, caches: Map.empty, token, treeId: tree_id, username, userId: user_id }
      | otherwise = Left "Invalid response from server"

postForgotPasswordRequest :: Backend -> String -> Aff (Either String String)
postForgotPasswordRequest backend email =
  decode <$> REST.post Nothing (toUrl backend "forgotPassword") { email }
  where
    decode (Left _err) = Left "Error when sending REST.post"
    decode (Right s) = Right s

get :: forall a p. JSON.ReadForeign a => ToUrl Session p =>
       Session -> p -> REST.AffRESTError a
get session@(Session {token}) p = REST.get (Just token) (toUrl session p)

put :: forall a b p. JSON.WriteForeign a => JSON.ReadForeign b => ToUrl Session p =>
       Session -> p -> a -> REST.AffRESTError b
put session@(Session {token}) p = REST.put (Just token) (toUrl session p)

put_ :: forall b p. JSON.ReadForeign b => ToUrl Session p => Session -> p -> REST.AffRESTError b
put_ session@(Session {token}) p = REST.put_ (Just token) (toUrl session p)

delete :: forall a p. JSON.ReadForeign a => ToUrl Session p =>
          Session -> p -> REST.AffRESTError a
delete session@(Session {token}) p = REST.delete (Just token) (toUrl session p)

-- This might not be a good idea:
-- https://stackoverflow.com/questions/14323716/restful-alternatives-to-delete-request-body
deleteWithBody :: forall a b p. JSON.WriteForeign a => JSON.ReadForeign b => ToUrl Session p =>
                  Session -> p -> a -> REST.AffRESTError b
deleteWithBody session@(Session {token}) p = REST.deleteWithBody (Just token) (toUrl session p)

post :: forall a b p. JSON.WriteForeign a => JSON.ReadForeign b => ToUrl Session p =>
        Session -> p -> a -> REST.AffRESTError b
post session@(Session {token}) p = REST.post (Just token) (toUrl session p)

postWwwUrlencoded :: forall b p. JSON.ReadForeign b => ToUrl Session p =>
                     Session -> p -> REST.FormDataParams -> REST.AffRESTError b
postWwwUrlencoded session@(Session {token}) p = REST.postWwwUrlencoded (Just token) (toUrl session p)
