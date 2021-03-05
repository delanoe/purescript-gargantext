-- | A module for authenticating to create sessions and handling them
module Gargantext.Sessions
  ( module Gargantext.Sessions.Types
  , load, change
  , Action(..), act, delete, get, post, put, put_
  , postAuthRequest, deleteWithBody, postWwwUrlencoded
  , getCacheState, setCacheState
  ) where

import Gargantext.Sessions.Types
  ( Session(..), Sessions(..), OpenNodes, NodeId, mkNodeId
  , sessionUrl, sessionId
  , empty, null, unSessions, lookup, cons, tryCons, update, remove, tryRemove
  )

import Data.Argonaut
  ( class DecodeJson, decodeJson, class EncodeJson, encodeJson )
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import DOM.Simple.Console (log2)
import Effect (Effect)
import Effect.Aff (Aff)
import Prelude
  ( Unit, bind, discard, otherwise, pure, unit, ($), (*>), (<$>), (<*), (>>=))
import Toestand as T
import Web.Storage.Storage (getItem, removeItem, setItem)

import Gargantext.Components.Login.Types
  ( AuthData(..), AuthInvalid(..), AuthRequest(..), AuthResponse(..) )
import Gargantext.Components.Nodes.Lists.Types as NT
import Gargantext.Config.REST as REST
import Gargantext.Ends (class ToUrl, Backend, toUrl)
import Gargantext.Utils.Reactix (getls)
import Gargantext.Utils.Toestand as T2

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
loadSessions = getls >>= getItem localStorageKey >>= handleMaybe
  where
    -- a localstorage lookup can find nothing
    handleMaybe (Just val) = handleEither (parse val >>= decode)
    handleMaybe Nothing    = pure empty

    -- either parsing or decoding could fail, hence two errors
    handleEither (Left err) = err *> pure empty
    handleEither (Right ss) = pure ss

    parse  s = mapLeft (log2 "Error parsing serialised sessions:") (jsonParser s)
    decode j = mapLeft (log2 "Error decoding serialised sessions:") (decodeJson j)

mapLeft :: forall l m r. (l -> m) -> Either l r -> Either m r
mapLeft f (Left  l) = Left (f l)
mapLeft _ (Right r) = Right r

saveSessions :: Sessions -> Effect Sessions
saveSessions sessions = effect *> pure sessions where
  rem = getls >>= removeItem localStorageKey
  set v  = getls >>= setItem    localStorageKey v
  effect
    | null sessions = rem
    | otherwise = set (stringify $ encodeJson sessions)

updateSession :: Session -> Effect Unit
updateSession s = do
  ss <- loadSessions
  _ <- saveSessions $ update s ss
  pure unit

postAuthRequest :: Backend -> AuthRequest -> Aff (Either String Session)
postAuthRequest backend ar@(AuthRequest {username}) =
  decode <$> REST.post Nothing (toUrl backend "auth") ar
  where
    decode (AuthResponse ar2)
      | {inval: Just (AuthInvalid {message})}     <- ar2 = Left message
      | {valid: Just (AuthData {token, tree_id})} <- ar2 =
          Right $ Session { backend, caches: Map.empty, token, treeId: tree_id, username }
      | otherwise = Left "Invalid response from server"

get :: forall a p. DecodeJson a => ToUrl Session p => Session -> p -> Aff a
get session@(Session {token}) p = REST.get (Just token) (toUrl session p)

put :: forall a b p. EncodeJson a => DecodeJson b => ToUrl Session p => Session -> p -> a -> Aff b
put session@(Session {token}) p = REST.put (Just token) (toUrl session p)

put_ :: forall b p. DecodeJson b => ToUrl Session p => Session -> p -> Aff b
put_ session@(Session {token}) p = REST.put_ (Just token) (toUrl session p)

delete :: forall a p. DecodeJson a => ToUrl Session p => Session -> p -> Aff a
delete session@(Session {token}) p = REST.delete (Just token) (toUrl session p)

-- This might not be a good idea:
-- https://stackoverflow.com/questions/14323716/restful-alternatives-to-delete-request-body
deleteWithBody :: forall a b p. EncodeJson a => DecodeJson b => ToUrl Session p => Session -> p -> a -> Aff b
deleteWithBody session@(Session {token}) p = REST.deleteWithBody (Just token) (toUrl session p)

post :: forall a b p. EncodeJson a => DecodeJson b => ToUrl Session p => Session -> p -> a -> Aff b
post session@(Session {token}) p = REST.post (Just token) (toUrl session p)

postWwwUrlencoded :: forall b p. DecodeJson b => ToUrl Session p => Session -> p -> REST.FormDataParams -> Aff b
postWwwUrlencoded session@(Session {token}) p = REST.postWwwUrlencoded (Just token) (toUrl session p)

postMultipartFormData :: forall b p. DecodeJson b => ToUrl Session p => Session -> p -> String -> Aff b
postMultipartFormData session@(Session {token}) p = REST.postMultipartFormData (Just token) (toUrl session p)
