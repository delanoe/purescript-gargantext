module Gargantext.Components.Nodes.Lists.SidePanel where

import Data.Maybe (Maybe(..))
import Gargantext.Core.NgramsTable.Types (NgramsTerm)
import Gargantext.Types (CorpusId)


-- type SidePanel :: forall k. Row k
type SidePanel = (
    mCorpusId      :: Maybe CorpusId
  , mCurrentNgrams :: Maybe NgramsTerm
)

initialSidePanel :: Maybe (Record SidePanel)
initialSidePanel = Nothing
