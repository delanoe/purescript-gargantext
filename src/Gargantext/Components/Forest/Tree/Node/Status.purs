module Gargantext.Components.Forest.Tree.Node.Status where

import Gargantext.Types
import Gargantext.Components.Forest.Tree.Node.Settings (NodeAction(..))

------------------------------------------------------------------------
-- Beta Status
data Status = Stable | Test | Dev

hasStatus :: NodeType -> NodeAction -> Status
hasStatus _ SearchBox         = Dev
hasStatus _ Refresh           = Dev
hasStatus _ Config            = Dev
hasStatus _ (Link  _)         = Dev
hasStatus _ (Merge _)         = Dev
hasStatus _ (Move  _)         = Test
hasStatus _ (Documentation _) = Dev
hasStatus Annuaire Upload     = Dev
hasStatus Texts    Upload     = Dev
hasStatus _ _                 = Stable

