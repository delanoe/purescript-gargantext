module Gargantext.AsyncTasks where

import Gargantext.Prelude

import DOM.Simple.Console (log2)
import Data.Argonaut (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as A
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Tuple (fst)
import Effect (Effect)
import Reactix as R
import Toestand as T
import Web.Storage.Storage as WSS

import Gargantext.Types as GT
import Gargantext.Utils as GU
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

localStorageKey :: String
localStorageKey = "garg-async-tasks"


type Storage = Map.Map GT.NodeID (Array GT.AsyncTaskWithType)

empty :: Storage
empty = Map.empty

getAsyncTasks :: Effect Storage
getAsyncTasks = R2.getls >>= WSS.getItem localStorageKey >>= handleMaybe
  where
    handleMaybe (Just val) = handleEither (parse val >>= decode)
    handleMaybe Nothing    = pure empty

    -- either parsing or decoding could fail, hence two errors
    handleEither (Left err) = err *> pure empty
    handleEither (Right ss) = pure ss

    parse  s = GU.mapLeft (log2 "Error parsing serialised sessions:") (jsonParser s)
    decode j = GU.mapLeft (log2 "Error decoding serialised sessions:") (decodeJson j)

getTasks :: Record ReductorProps -> GT.NodeID -> Array GT.AsyncTaskWithType
getTasks { storage } nodeId = fromMaybe [] $ Map.lookup nodeId storage

getTasksMaybe :: Maybe Reductor -> GT.NodeID -> Array GT.AsyncTaskWithType
getTasksMaybe mTasks nodeId = case mTasks of
    Just tasks -> getTasks (fst tasks) nodeId
    Nothing -> []

removeTaskFromList :: Array GT.AsyncTaskWithType -> GT.AsyncTaskWithType -> Array GT.AsyncTaskWithType
removeTaskFromList ts (GT.AsyncTaskWithType { task: GT.AsyncTask { id: id' } }) =
  A.filter (\(GT.AsyncTaskWithType { task: GT.AsyncTask { id: id'' } }) -> id' /= id'') ts

type ReductorProps = (
    reloadForest :: T.Box T2.Reload
  , reloadRoot   :: T.Box T2.Reload
  , storage      :: Storage
  )

type Reductor = R2.Reductor (Record ReductorProps) Action
type ReductorAction = Action -> Effect Unit

useTasks :: T.Box T2.Reload -> T.Box T2.Reload -> R.Hooks Reductor
useTasks reloadRoot reloadForest = R2.useReductor act initializer unit
  where
    act :: R2.Actor (Record ReductorProps) Action
    act a s = action s a
    initializer _ = do
      storage <- getAsyncTasks
      pure { reloadRoot, reloadForest, storage }

data Action =
    Insert GT.NodeID GT.AsyncTaskWithType
  | Finish GT.NodeID GT.AsyncTaskWithType
  | Remove GT.NodeID GT.AsyncTaskWithType

action :: Record ReductorProps -> Action -> Effect (Record ReductorProps)
action p@{ reloadForest, storage } (Insert nodeId t) = do
  -- _ <- T2.reload reloadForest
  let newStorage = Map.alter (maybe (Just [t]) (\ts -> Just $ A.cons t ts)) nodeId storage
  pure $ p { storage = newStorage }
action p (Finish nodeId t) = do
  action p (Remove nodeId t)
action p@{ reloadRoot, reloadForest, storage } (Remove nodeId t@(GT.AsyncTaskWithType { typ })) = do
  _ <- if GT.asyncTaskTriggersAppReload typ then
    pure unit
    -- T2.reload reloadRoot
  else
    pure unit
  _ <- if GT.asyncTaskTriggersTreeReload typ then
    pure unit
    -- T2.reload reloadForest
  else
    pure unit
  let newStorage = Map.alter (maybe Nothing $ (\ts -> Just $ removeTaskFromList ts t)) nodeId storage
  pure $ p { storage = newStorage }
