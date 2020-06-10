module Gargantext.Components.Forest.Tree.Node.Action.Delete
  where

import Data.Maybe (Maybe(..))
import Gargantext.Prelude
import Effect.Aff (Aff)
import Gargantext.Types  as GT
import Gargantext.Sessions (Session, delete)
import Gargantext.Routes (SessionRoute(..))

-- TODO Delete with asyncTaskWithType
deleteNode :: Session -> GT.ID -> Aff GT.ID
deleteNode session nodeId = delete session $ NodeAPI GT.Node (Just nodeId) ""


