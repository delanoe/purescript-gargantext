module Gargantext.Components.Forest.Tree.Node.Action.Rename where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Simple.JSON as JSON

import Gargantext.Prelude

import Gargantext.Components.Forest.Tree.Node.Action
import Gargantext.Types as GT
import Gargantext.Types (ID)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, put)

------------------------------------------------------------------------
rename :: Session -> ID -> RenameValue -> Aff (Array ID)
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
