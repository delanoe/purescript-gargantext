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

data Action = CreateSubmit String GT.NodeType
            | DeleteNode
            | UpdateNode  GT.AsyncTaskWithType
            | SearchQuery GT.AsyncTaskWithType
            | Submit      String
            | UploadFile  GT.NodeType FileType (Maybe String) UploadFileContents
            | RefreshTree

-----------------------------------------------------
-- TODO Delete with asyncTaskWithType
deleteNode :: Session -> GT.ID -> Aff GT.ID
deleteNode session nodeId = delete session $ NodeAPI GT.Node (Just nodeId) ""
-----------------------------------------------------------------------
