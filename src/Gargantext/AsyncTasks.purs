module Gargantext.AsyncTasks where

import Data.Argonaut (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as A
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Tuple (snd)
import DOM.Simple.Console (log2)
import Effect (Effect)
import Reactix as R
import Web.Storage.Storage as WSS

import Gargantext.Prelude
import Gargantext.Types as GT
import Gargantext.Utils as GU
import Gargantext.Utils.Reactix as R2


localStorageKey :: String
localStorageKey = "garg-async-tasks"


type NodeId = Int
type Storage = Map.Map NodeId (Array GT.AsyncTaskWithType)

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

getTasks :: Record ReductorProps -> NodeId -> Array GT.AsyncTaskWithType
getTasks { storage } nodeId = fromMaybe [] $ Map.lookup nodeId storage

removeTaskFromList :: Array GT.AsyncTaskWithType -> GT.AsyncTaskWithType -> Array GT.AsyncTaskWithType
removeTaskFromList ts (GT.AsyncTaskWithType { task: GT.AsyncTask { id: id' } }) =
  A.filter (\(GT.AsyncTaskWithType { task: GT.AsyncTask { id: id'' } }) -> id' /= id'') ts

type ReductorProps = (
    appReload  :: R.State Int
  , treeReload :: R.State Int
  , storage    :: Storage
  )

type Reductor = R2.Reductor (Record ReductorProps) Action
type ReductorAction = Action -> Effect Unit
type OnFinish = Effect Unit

useTasks :: R.State Int -> R.State Int -> R.Hooks Reductor
useTasks appReload treeReload = R2.useReductor act initializer unit
  where
    act :: R2.Actor (Record ReductorProps) Action
    act a s = action s a
    initializer _ = do
      storage <- getAsyncTasks
      pure { appReload, treeReload, storage }

data Action =
    Insert NodeId GT.AsyncTaskWithType
  | Finish NodeId GT.AsyncTaskWithType
  | Remove NodeId GT.AsyncTaskWithType

action :: Record ReductorProps -> Action -> Effect (Record ReductorProps)
action p@{ treeReload, storage } (Insert nodeId t) = do
  _ <- snd treeReload $ (_ + 1)
  let newStorage = Map.alter (maybe (Just [t]) (\ts -> Just $ A.cons t ts)) nodeId storage
  pure $ p { storage = newStorage }
action p (Finish nodeId t) = do
  action p (Remove nodeId t)
action p@{ appReload, storage } (Remove nodeId t) = do
  _ <- snd appReload $ (_ + 1)
  let newStorage = Map.alter (maybe Nothing $ (\ts -> Just $ removeTaskFromList ts t)) nodeId storage
  pure $ p { storage = newStorage }
