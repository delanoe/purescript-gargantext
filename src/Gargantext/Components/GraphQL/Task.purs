module Gargantext.Components.GraphQL.Task where

import Gargantext.Types (AsyncTaskID)

type AsyncTaskStatus = String
type AsyncTaskType = String

type AsyncTask =
  { id     :: AsyncTaskID
  , status :: AsyncTaskStatus}

type AsyncTaskWithType =
  { task :: AsyncTask
  , typ  :: AsyncTaskType }
