module Gargantext.AsyncTasks where

import Gargantext.Prelude

import DOM.Simple.Console (log2)
import Data.Argonaut (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as A
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Effect (Effect)
import Gargantext.Types as GT
import Gargantext.Utils as GU
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Toestand as T
import Web.Storage.Storage as WSS

localStorageKey :: String
localStorageKey = "garg-async-tasks"


type TaskList = Array GT.AsyncTaskWithType
type Storage = Map.Map GT.NodeID TaskList

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

getTasks :: GT.NodeID -> Storage -> TaskList
getTasks nodeId storage = fromMaybe [] $ Map.lookup nodeId storage

setTasks :: GT.NodeID -> TaskList -> Storage -> Storage
setTasks id tasks s = Map.insert id tasks s

focus :: GT.NodeID -> T.Box Storage -> R.Hooks (T.Box TaskList)
focus id tasks = T.useFocused (getTasks id) (setTasks id) tasks

removeTaskFromList :: TaskList -> GT.AsyncTaskWithType -> TaskList
removeTaskFromList ts (GT.AsyncTaskWithType { task: GT.AsyncTask { id: id' } }) =
  A.filter (\(GT.AsyncTaskWithType { task: GT.AsyncTask { id: id'' } }) -> id' /= id'') ts

type ReductorProps = (
    reloadForest :: T2.ReloadS
  , reloadRoot   :: T2.ReloadS
  , storage      :: Storage
  )

insert :: GT.NodeID -> GT.AsyncTaskWithType -> T.Box Storage -> Effect Unit
insert id task storage = T.modify_ newStorage storage
  where
    newStorage s = Map.alter (maybe (Just [task]) (\ts -> Just $ A.cons task ts)) id s

finish :: GT.NodeID -> GT.AsyncTaskWithType -> T.Box Storage -> Effect Unit
finish id task storage = remove id task storage

remove :: GT.NodeID -> GT.AsyncTaskWithType -> T.Box Storage -> Effect Unit
remove id task storage = T.modify_ newStorage storage
  where
    newStorage s = Map.alter (maybe Nothing $ (\ts -> Just $ removeTaskFromList ts task)) id s


-- When a task is finished: which tasks cause forest or app reload
asyncTaskTriggersAppReload :: GT.AsyncTaskType -> Boolean
asyncTaskTriggersAppReload GT.UpdateNgramsCharts = true
asyncTaskTriggersAppReload _                     = false

asyncTaskTTriggersAppReload :: GT.AsyncTaskWithType -> Boolean
asyncTaskTTriggersAppReload (GT.AsyncTaskWithType { typ }) = asyncTaskTriggersAppReload typ

asyncTaskTriggersTreeReload :: GT.AsyncTaskType -> Boolean
asyncTaskTriggersTreeReload GT.Form       = true
asyncTaskTriggersTreeReload GT.UploadFile = true
asyncTaskTriggersTreeReload _            = false

asyncTaskTTriggersTreeReload :: GT.AsyncTaskWithType -> Boolean
asyncTaskTTriggersTreeReload (GT.AsyncTaskWithType { typ }) = asyncTaskTriggersTreeReload typ
