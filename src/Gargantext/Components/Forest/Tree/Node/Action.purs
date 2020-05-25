module Gargantext.Components.Forest.Tree.Node.Action where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Prelude hiding (div)

import Gargantext.Components.Lang (Lang)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get, put, post, delete)
import Gargantext.Types as GT

data Action = CreateSubmit String GT.NodeType
            | DeleteNode
            | SearchQuery GT.AsyncTaskWithType
            | Submit       String
            | UploadFile  GT.NodeType FileType (Maybe String) UploadFileContents
            | RefreshTree

-----------------------------------------------------
-- UploadFile Action
-- file upload types
data FileType = CSV | CSV_HAL | WOS | PresseRIS

derive instance genericFileType :: Generic FileType _

instance eqFileType :: Eq FileType where
    eq = genericEq

instance showFileType :: Show FileType where
    show = genericShow

readFileType :: String -> Maybe FileType
readFileType "CSV"       = Just CSV
readFileType "CSV_HAL"   = Just CSV_HAL
readFileType "PresseRIS" = Just PresseRIS
readFileType "WOS"       = Just WOS
readFileType _           = Nothing

data DroppedFile = DroppedFile {
    contents :: UploadFileContents
  , fileType :: Maybe FileType
  , lang     :: Maybe Lang
    }
type FileHash = String

type Name = String
type ID   = Int
type Reload = Int

newtype UploadFileContents = UploadFileContents String
type UploadFile = {
    contents :: UploadFileContents
  , name     :: String
  }

createNode :: Session -> ID -> CreateValue -> Aff (Array ID)
createNode session parentId = post session $ NodeAPI GT.Node (Just parentId) ""

renameNode :: Session -> ID -> RenameValue -> Aff (Array ID)
renameNode session renameNodeId = put session $ NodeAPI GT.Node (Just renameNodeId) "rename"

deleteNode :: Session -> ID -> Aff ID
deleteNode session nodeId = delete session $ NodeAPI GT.Node (Just nodeId) ""

loadNode :: Session -> ID -> Aff FTree
loadNode session nodeId = get session $ NodeAPI GT.Tree (Just nodeId) ""


newtype RenameValue = RenameValue
  {
    name :: Name
  }

instance encodeJsonRenameValue :: EncodeJson RenameValue where
  encodeJson (RenameValue {name})
     = "r_name" := name
    ~> jsonEmptyObject

newtype CreateValue = CreateValue
  {
    name :: Name
  , nodeType :: GT.NodeType
  }

instance encodeJsonCreateValue :: EncodeJson CreateValue where
  encodeJson (CreateValue {name, nodeType})
     = "pn_name"     := name
    ~> "pn_typename" := nodeType
    ~> jsonEmptyObject

data NTree a = NTree a (Array (NTree a))
type FTree = NTree LNode
type Tree = { tree :: FTree, asyncTasks :: Array GT.AsyncTaskWithType }

instance ntreeFunctor :: Functor NTree where
  map f (NTree x ary) = NTree (f x) (map (map f) ary)

newtype LNode = LNode { id :: ID
                      , name :: Name
                      , nodeType :: GT.NodeType
                      }

derive instance newtypeLNode :: Newtype LNode _

instance decodeJsonLNode :: DecodeJson LNode where
  decodeJson json = do
    obj  <- decodeJson json
    id_  <- obj .: "id"
    name <- obj .: "name"
    nodeType <- obj .: "type"
    pure $ LNode { id : id_
                 , name
                 , nodeType}

instance decodeJsonFTree :: DecodeJson (NTree LNode) where
  decodeJson json = do
    obj    <- decodeJson json
    node   <- obj .: "node"
    nodes  <- obj .: "children"
    node'  <- decodeJson node
    nodes' <- decodeJson nodes
    pure $ NTree node' nodes'

