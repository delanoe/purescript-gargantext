module Gargantext.AsyncTasks where

import Data.Argonaut (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as A
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import DOM.Simple.Console (log2)
import Effect (Effect)
import Web.Storage.Storage as WSS

import Gargantext.Prelude
import Gargantext.Types as GT
import Gargantext.Utils as GU
import Gargantext.Utils.Reactix as R2


localStorageKey :: String
localStorageKey = "garg-async-tasks"

type Storage = Map.Map Int (Array GT.AsyncTaskWithType)

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

removeTaskFromList :: Array GT.AsyncTaskWithType -> GT.AsyncTaskWithType -> Array GT.AsyncTaskWithType
removeTaskFromList ts (GT.AsyncTaskWithType { task: GT.AsyncTask { id: id' } }) =
  A.filter (\(GT.AsyncTaskWithType { task: GT.AsyncTask { id: id'' } }) -> id' /= id'') ts
