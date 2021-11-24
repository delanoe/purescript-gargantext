module Gargantext.Components.Forest.Tree.Node.Action.Rename where


import Gargantext.Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(..))
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, put)
import Gargantext.Types (ID)
import Gargantext.Types as GT
import Simple.JSON as JSON

------------------------------------------------------------------------
rename :: Session -> ID -> RenameValue -> AffRESTError (Array ID)
rename session renameNodeId =
  put session $ GR.NodeAPI GT.Node (Just renameNodeId) "rename"

renameAction :: String -> Action
renameAction newName = RenameNode newName

------------------------------------------------------------------------
newtype RenameValue = RenameValue
  { text :: String }
derive instance Generic RenameValue _
derive instance Newtype RenameValue _
instance JSON.WriteForeign RenameValue where
  writeImpl (RenameValue {text}) = JSON.writeImpl { name: text }

------------------------------------------------------------------------
