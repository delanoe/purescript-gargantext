-- | The memory is a persisted dynamic state of the forest, so the
-- | user can pick up where they left off after a page refresh
module Gargantext.Components.Forest.Memories where
  -- ( Memory(..), Memories(..)
  -- , emptyMemory, emptyMemories
  -- , loadMemories, saveMemories
  -- ) where

import Prelude
import Data.Argonaut ( class DecodeJson, decodeJson, class EncodeJson, encodeJson, (:=), (~>), (.:))
import Data.Argonaut.Core (Json, fromArray, jsonEmptyObject, stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.Set (Set)
import Data.Traversable (traverse)
import DOM.Simple.Console (log2)
import Effect (Effect)
import Reactix as R
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (Storage, getItem, setItem, removeItem)

import Gargantext.Utils.Reactix as R2
import Gargantext.Sessions (Session, mapLeft, sessionId)
import Gargantext.Types (SessionId)

newtype Memory = Memory
  { openTrees :: Set Int }

isOpen :: Memory -> Int -> Boolean
isOpen (Memory { openTrees }) tid = Set.member tid openTrees

toggleOpen' :: Memory -> Int -> Boolean -> Memory
toggleOpen' (Memory {openTrees}) tid true =
  Memory { openTrees: Set.insert tid openTrees }
toggleOpen' (Memory {openTrees}) tid false =
  Memory { openTrees: Set.delete tid openTrees }

toggleOpen :: Memories -> Session -> Int -> Boolean -> Memories
toggleOpen (Memories {sessions}) s tid set =
  Memories {sessions: memories} where
    sid = sessionId s
    origMemory = maybe emptyMemory identity $ Map.lookup sid sessions
    memory = toggleOpen' origMemory tid set
    memories = Map.insert sid memory sessions

emptyMemory :: Memory
emptyMemory = Memory
  { openTrees: mempty }

newtype Memories = Memories
  { sessions :: Map SessionId Memory }

emptyMemories :: Memories
emptyMemories = Memories
  { sessions: mempty }

null :: Memories -> Boolean
null (Memories {sessions}) = Map.isEmpty sessions

findMemory :: Memories -> Session -> Maybe Memory
findMemory (Memories {sessions}) s = Map.lookup (sessionId s) sessions

-- Key we will store the data under
localStorageKey :: String
localStorageKey = "garg-forest-memories"

loadMemories :: Effect Memories
loadMemories = getls >>= getItem localStorageKey >>= handleMaybe
  where
    -- a localstorage lookup can find nothing
    handleMaybe (Just val) = handleEither (parse val >>= decode)
    handleMaybe Nothing    = pure emptyMemories

    -- either parsing or decoding could fail, hence two errors
    handleEither (Left err) = err *> pure emptyMemories
    handleEither (Right ss) = pure ss

    parse  s = mapLeft (log2 "Error parsing serialised forest memories:") (jsonParser s)
    decode j = mapLeft (log2 "Error decoding serialised forest memories:") (decodeJson j)

saveMemories :: Memories -> Effect Memories
saveMemories memories = effect *> pure memories where
  rem = getls >>= removeItem localStorageKey
  set v  = getls >>= setItem    localStorageKey v
  effect
    | null memories = rem
    | otherwise = set (stringify $ encodeJson memories)

data Action = ToggleTreeOpen Session Int Boolean

useMemories :: R.Hooks (R2.Reductor Memories Action)
useMemories = do
  R2.useReductor actAndSave (const loadMemories) unit
  where
    actAndSave :: R2.Actor Memories Action
    actAndSave a m = act m a >>= saveMemories

act :: Memories -> Action -> Effect Memories
act m (ToggleTreeOpen s tid b) = pure $ toggleOpen m s tid b

getls :: Effect Storage
getls = window >>= localStorage

instance encodeJsonMemory :: EncodeJson Memory where
  encodeJson (Memory { openTrees })
    = "openTrees" := encodeJson (Array.fromFoldable openTrees)
    ~> jsonEmptyObject

instance decodeJsonMemory :: DecodeJson Memory where
  decodeJson json = do
    obj <- decodeJson json
    (openTrees :: Array Int) <- obj .: "openTrees"
    pure $ Memory { openTrees: Set.fromFoldable openTrees }

instance encodeJsonMemories :: EncodeJson Memories where
  encodeJson (Memories { sessions })
    = "sessions" := encodeJson sessions
    ~> jsonEmptyObject

instance decodeJsonMemories :: DecodeJson Memories where
  decodeJson json = do
    obj <- decodeJson json
    sessions <- obj .: "sessions"
    pure $ Memories { sessions }
