module Gargantext.Components.Forest.Tree.Node.Tools.Task
  where

import Data.Array as A
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Gargantext.AsyncTasks as GAT
import Gargantext.Prelude (Unit, discard, identity, ($), (+))
import Gargantext.Types (Reload)
import Gargantext.Types as GT
import Reactix as R


type Tasks =
  ( onTaskAdd    :: GT.AsyncTaskWithType -> Effect Unit
  , onTaskFinish :: GT.AsyncTaskWithType -> Effect Unit
  , tasks        :: Array GT.AsyncTaskWithType
  )

tasksStruct :: Int
            -> R.State GAT.Storage
            -> R.State Reload
            -> Record Tasks
tasksStruct id (asyncTasks /\ setAsyncTasks) (_ /\ setReload) =
  { onTaskAdd, onTaskFinish, tasks }
    where
      tasks = maybe [] identity $ Map.lookup id asyncTasks

      onTaskAdd t = do
        setReload (_ + 1)
        setAsyncTasks $ Map.alter (maybe (Just [t])
                      $ (\ts -> Just $ A.cons t ts)) id

      onTaskFinish t = do
        setReload (_ + 1)
        setAsyncTasks $ Map.alter (maybe Nothing $ (\ts -> Just $ GAT.removeTaskFromList ts t)) id
