module Gargantext.AsyncTasks where

import Data.Argonaut (decodeJson, class EncodeJson, encodeJson, (:=), (~>), (.:))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import DOM.Simple.Console (log2)
import Effect (Effect)
import Web.Storage.Storage as WSS

import Gargantext.Prelude
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2


localStorageKey :: String
localStorageKey = "garg-async-tasks"

empty :: Map.Map Int (Array GT.AsyncTaskWithType)
empty = Map.empty

getAsyncTasks :: Effect (Map.Map Int (Array GT.AsyncTaskWithType))
getAsyncTasks = R2.getls >>= WSS.getItem localStorageKey >>= handleMaybe
  where
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

