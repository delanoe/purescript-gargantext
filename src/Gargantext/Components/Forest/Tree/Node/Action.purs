module Gargantext.Components.Forest.Tree.Node.Action where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get, put, post, delete)
import Gargantext.Types (NodeType(..), AsyncTask(..))
import Prelude hiding (div)

data Action = Submit       String
            | DeleteNode
            | CreateSubmit String NodeType
            | UploadFile   FileType UploadFileContents

-----------------------------------------------------
-- UploadFile Action
-- file upload types
data FileType = CSV | CSV_HAL | PresseRIS

derive instance genericFileType :: Generic FileType _

instance eqFileType :: Eq FileType where
    eq = genericEq

instance showFileType :: Show FileType where
    show = genericShow

readFileType :: String -> Maybe FileType
readFileType "CSV_HAL"   = Just CSV_HAL
readFileType "CSV"       = Just CSV
readFileType "PresseRIS" = Just PresseRIS
readFileType _           = Nothing

data DroppedFile = DroppedFile {
    contents :: UploadFileContents
  , fileType :: Maybe FileType
    }
type FileHash = String

type Name = String
type ID   = Int
type Reload = Int

newtype UploadFileContents = UploadFileContents String


createNode :: Session -> ID -> CreateValue -> Aff (Array ID)
createNode session parentId = post session $ NodeAPI Node (Just parentId) ""

renameNode :: Session -> ID -> RenameValue -> Aff (Array ID)
renameNode session renameNodeId = put session $ NodeAPI Node (Just renameNodeId) "rename"

deleteNode :: Session -> ID -> Aff ID
deleteNode session nodeId = delete session $ NodeAPI Node (Just nodeId) ""

loadNode :: Session -> ID -> Aff FTree
loadNode session nodeId = get session $ NodeAPI Tree (Just nodeId) ""


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
  , nodeType :: NodeType
  }

instance encodeJsonCreateValue :: EncodeJson CreateValue where
  encodeJson (CreateValue {name, nodeType})
     = "pn_name"     := name
    ~> "pn_typename" := nodeType
    ~> jsonEmptyObject

data NTree a = NTree a (Array (NTree a))
type FTree = NTree LNode
type Tree = { tree :: FTree, asyncTasks :: Array AsyncTask }

instance ntreeFunctor :: Functor NTree where
  map f (NTree x ary) = NTree (f x) (map (map f) ary)


newtype LNode = LNode { id :: ID
                      , name :: Name
                      , nodeType :: NodeType
                      }

derive instance newtypeLNode :: Newtype LNode _

instance decodeJsonLNode :: DecodeJson LNode where
  decodeJson json = do
    obj <- decodeJson json
    id_ <- obj .: "id"
    name <- obj .: "name"
    nodeType <- obj .: "type"
    pure $ LNode { id : id_
                 , name
                 , nodeType}

instance decodeJsonFTree :: DecodeJson (NTree LNode) where
  decodeJson json = do
    obj <- decodeJson json
    node <- obj .: "node"
    nodes <- obj .: "children"
    node' <- decodeJson node
    nodes' <- decodeJson nodes
    pure $ NTree node' nodes'

