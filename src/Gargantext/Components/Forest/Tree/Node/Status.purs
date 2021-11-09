module Gargantext.Components.Forest.Tree.Node.Status where

import Gargantext.Types
import Gargantext.Components.Forest.Tree.Node.Settings (NodeAction(..))

------------------------------------------------------------------------
-- Beta Status
data Status = Stable | Test | Dev

hasStatus :: NodeType -> NodeAction -> Status
hasStatus _ SearchBox         = Test
hasStatus _ Refresh           = Dev
hasStatus _ Config            = Dev
hasStatus _ (Merge _)         = Dev
hasStatus _ (Documentation _) = Dev
hasStatus Annuaire  Upload    = Dev
hasStatus NodeTexts Upload    = Dev
hasStatus Corpus   (Add _)    = Dev
hasStatus _ _                 = Stable

