module Gargantext.Components.Nodes.Lists.SidePanel where

import Data.Maybe (Maybe(..))
import Gargantext.Core.NgramsTable.Types (NgramsTerm)


-- type SidePanel :: forall k. Row k
type SidePanel = (
  mCurrentNgrams :: Maybe NgramsTerm
)

initialSidePanel :: Maybe (Record SidePanel)
initialSidePanel = Nothing
