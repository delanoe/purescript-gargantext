module Gargantext.Components.Forest.Tree.Node.Tools.Task
  where

import Data.Array as A
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Reactix as R

import Gargantext.AsyncTasks as GAT
import Gargantext.Prelude (Unit, discard, identity, ($), (+))
import Gargantext.Types (Reload)
import Gargantext.Types as GT


type Tasks =
  ( onTaskAdd    :: GT.AsyncTaskWithType -> Effect Unit
  , onTaskFinish :: GT.AsyncTaskWithType -> Effect Unit
  , tasks        :: Array GT.AsyncTaskWithType
  )

tasksStruct :: Int
            -> GAT.Reductor
            -> R.State Reload
            -> Record Tasks
tasksStruct id ({ storage } /\ dispatch) (_ /\ setReload) =
  { onTaskAdd, onTaskFinish, tasks }
    where
      tasks = maybe [] identity $ Map.lookup id storage

      onTaskAdd t = dispatch $ GAT.Insert id t

      onTaskFinish t = dispatch $ GAT.Remove id t
