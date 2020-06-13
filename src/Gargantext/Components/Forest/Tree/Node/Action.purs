module Gargantext.Components.Forest.Tree.Node.Action where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Gargantext.Prelude (class Eq, class Show, class Read, Unit)
import Gargantext.Sessions (Session)
import Gargantext.Types  as GT
import Gargantext.Components.Forest.Tree.Node (NodeAction(..), glyphiconNodeAction)
import Gargantext.Components.Forest.Tree.Node.Action.Upload.Types (FileType, UploadFileContents)

type Props =
  ( dispatch :: Action -> Aff Unit
  , id       :: Int
  , nodeType :: GT.NodeType
  , session  :: Session
  )

data Action = AddNode     String GT.NodeType
            | DeleteNode
            | RenameNode  String
            | UpdateNode  GT.AsyncTaskWithType
            | ShareNode   String
            | DoSearch    GT.AsyncTaskWithType
            | UploadFile  GT.NodeType FileType (Maybe String) UploadFileContents
            | RefreshTree


instance showShow :: Show Action where
  show (AddNode     _ _    )= "AddNode"
  show  DeleteNode          = "DeleteNode"
  show (RenameNode  _      )= "RenameNode"
  show (UpdateNode  _      )= "UpdateNode"
  show (ShareNode   _      )= "ShareNode"
  show (DoSearch    _      )= "SearchQuery"
  show (UploadFile  _ _ _ _)= "UploadFile"
  show  RefreshTree         = "RefreshTree"

-----------------------------------------------------------------------
icon :: Action -> String
icon (AddNode _ _)        = glyphiconNodeAction (Add [])
icon  DeleteNode          = glyphiconNodeAction Delete
icon (RenameNode _)       = glyphiconNodeAction Config
icon (UpdateNode _)       = glyphiconNodeAction Refresh
icon (ShareNode _)        = glyphiconNodeAction Share
icon (DoSearch   _)       = glyphiconNodeAction SearchBox
icon (UploadFile _ _ _ _) = glyphiconNodeAction Upload
icon  RefreshTree         = glyphiconNodeAction Refresh
-- icon _             = "hand-o-right"

text :: Action -> String
text (AddNode     _ _    )= "Add !"
text  DeleteNode          = "Delete !"
text (RenameNode  _      )= "Rename !"
text (UpdateNode  _      )= "Update !"
text (ShareNode   _      )= "Share !"
text (DoSearch    _      )= "Launch search !"
text (UploadFile  _ _ _ _)= "Upload File !"
text  RefreshTree         = "Refresh Tree !"
-----------------------------------------------------------------------
