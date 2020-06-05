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
import Gargantext.Routes as GR
import Gargantext.Types  as GT

data Action = AddNode String GT.NodeType
            | DeleteNode
            | UpdateNode  GT.AsyncTaskWithType
            | RenameNode  String
            | SearchQuery GT.AsyncTaskWithType
            | UploadFile  GT.NodeType FileType (Maybe String) UploadFileContents
            | RefreshTree

-----------------------------------------------------
-- TODO Delete with asyncTaskWithType
deleteNode :: Session -> GT.ID -> Aff GT.ID
deleteNode session nodeId = delete session $ NodeAPI GT.Node (Just nodeId) ""
-----------------------------------------------------------------------

type Props =
  ( dispatch :: Action -> Aff Unit
  , id       :: Int
  , nodeType :: GT.NodeType
  , session  :: Session
  )

-- TODO remove these types from here

data FileType = CSV | CSV_HAL | WOS | PresseRIS

derive instance genericFileType :: Generic FileType _

instance eqFileType :: Eq FileType where
    eq = genericEq

instance showFileType :: Show FileType where
    show = genericShow


newtype UploadFileContents = UploadFileContents String
