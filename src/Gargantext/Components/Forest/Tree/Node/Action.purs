module Gargantext.Components.Forest.Tree.Node.Action where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Gargantext.Prelude
import Gargantext.Components.Lang (Lang)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, get, put, post, delete)
import Gargantext.Types  as GT

type Props =
  ( dispatch :: Action -> Aff Unit
  , id       :: Int
  , nodeType :: GT.NodeType
  , session  :: Session
  )

data Action = AddNode String GT.NodeType
            | DeleteNode
            | UpdateNode  GT.AsyncTaskWithType
            | RenameNode  String
            | DoSearch    GT.AsyncTaskWithType
            | UploadFile  GT.NodeType FileType (Maybe String) UploadFileContents
            | RefreshTree
            | ShareNode   String

instance showShow :: Show Action where
  show DeleteNode       = "DeleteNode"
  show RefreshTree      = "RefreshTree"
  show (ShareNode   _)  = "ShareNode"
  show (UpdateNode  _)  = "UpdateNode"
  show (RenameNode  _)  = "RenameNode"
  show (DoSearch    _)  = "SearchQuery"
  show (AddNode   _ _)  = "AddNode"
  show (UploadFile  _ _ _ _)= "UploadFile"

-----------------------------------------------------------------------
icon :: Action -> String
icon DeleteNode = "trash"
icon (AddNode _ _)    = "plus"
icon _ = "hand-o-right"

text :: Action -> String
text DeleteNode     = "Delete !"
text (AddNode   _ _)  = "Add !"
text (UpdateNode  _) = "Update !"
text (RenameNode  _) = "Rename !"
text (DoSearch  _) = "Launch search !"
text (UploadFile  _ _ _ _)= "Upload File !"
text RefreshTree = "Refresh Tree !"
text (ShareNode _)  = "Share !"
-----------------------------------------------------------------------

-- TODO move code below elsewhere

-- TODO Delete with asyncTaskWithType
deleteNode :: Session -> GT.ID -> Aff GT.ID
deleteNode session nodeId = delete session $ NodeAPI GT.Node (Just nodeId) ""

data FileType = CSV | CSV_HAL | WOS | PresseRIS

derive instance genericFileType :: Generic FileType _

instance eqFileType :: Eq FileType where
    eq = genericEq

instance showFileType :: Show FileType where
    show = genericShow

newtype UploadFileContents = UploadFileContents String
