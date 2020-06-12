module Gargantext.Components.Forest.Tree.Node.Action where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Gargantext.Prelude
import Gargantext.Sessions (Session)
import Gargantext.Types  as GT
import Gargantext.Components.Forest.Tree.Node (NodeAction(..), glyphiconNodeAction)

type Props =
  ( dispatch :: Action -> Aff Unit
  , id       :: Int
  , nodeType :: GT.NodeType
  , session  :: Session
  )

data Action = AddNode     String GT.NodeType
            | DeleteNode
            | UpdateNode  GT.AsyncTaskWithType
            | RenameNode  String
            | DoSearch    GT.AsyncTaskWithType
            | UploadFile  GT.NodeType FileType (Maybe String) UploadFileContents
            | RefreshTree
            | ShareNode   String


instance showShow :: Show Action where
  show  DeleteNode          = "DeleteNode"
  show  RefreshTree         = "RefreshTree"
  show (ShareNode   _      )= "ShareNode"
  show (UpdateNode  _      )= "UpdateNode"
  show (RenameNode  _      )= "RenameNode"
  show (DoSearch    _      )= "SearchQuery"
  show (AddNode     _ _    )= "AddNode"
  show (UploadFile  _ _ _ _)= "UploadFile"

-----------------------------------------------------------------------
icon :: Action -> String
icon (AddNode _ _)        = glyphiconNodeAction (Add [])
icon DeleteNode           = glyphiconNodeAction Delete
icon (UpdateNode _)       = glyphiconNodeAction Refresh
icon (RenameNode _)       = glyphiconNodeAction Config
icon (DoSearch   _)       = glyphiconNodeAction SearchBox
icon (UploadFile _ _ _ _) = glyphiconNodeAction Upload
icon RefreshTree          = glyphiconNodeAction Refresh
icon (ShareNode _)        = glyphiconNodeAction Share
-- icon _             = "hand-o-right"

text :: Action -> String
text  DeleteNode          = "Delete !"
text  RefreshTree         = "Refresh Tree !"
text (AddNode     _ _    )= "Add !"
text (UpdateNode  _      )= "Update !"
text (RenameNode  _      )= "Rename !"
text (DoSearch    _      )= "Launch search !"
text (ShareNode   _      )= "Share !"
text (UploadFile  _ _ _ _)= "Upload File !"
-----------------------------------------------------------------------

-- TODO move code below elsewhere
data FileType = CSV | CSV_HAL | WOS | PresseRIS

derive instance genericFileType :: Generic FileType _

instance eqFileType :: Eq FileType where
    eq = genericEq

instance showFileType :: Show FileType where
    show = genericShow

newtype UploadFileContents = UploadFileContents String
