module Gargantext.Components.Forest.Tree.Node.Action.Rename where

import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Prelude (($))
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

instance encodeJsonRenameValue :: EncodeJson RenameValue where
  encodeJson (RenameValue {text})
     = "name" := text
    ~> jsonEmptyObject

------------------------------------------------------------------------
