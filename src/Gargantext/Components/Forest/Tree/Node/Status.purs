module Gargantext.Components.Forest.Tree.Node.Status where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq   (genericEq)
import Gargantext.Prelude (class Eq, class Show, show, (&&), (<>), (==))
import Data.Array (foldl)
import Gargantext.Types
import Gargantext.Components.Forest.Tree.Node.Settings (NodeAction(..))

------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
-- Beta Status
data Status = Prod | Test | WIP

hasStatus :: NodeType -> NodeAction -> Status
hasStatus _ SearchBox         = WIP
hasStatus _ (Documentation _) = Test
hasStatus _ _                 = Prod

