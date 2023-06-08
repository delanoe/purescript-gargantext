module Gargantext.Components.Nodes.Lists.SidePanel where

import Data.Maybe (Maybe(..))
import Gargantext.Core.NgramsTable.Types (NgramsTerm)
import Gargantext.Types (CorpusId, ListId)


-- type SidePanel :: forall k. Row k
type SidePanel = (
    mCorpusId      :: Maybe CorpusId
  , mCurrentNgrams :: Maybe NgramsTerm
  , mListId        :: Maybe ListId
)

initialSidePanel :: Maybe (Record SidePanel)
initialSidePanel = Nothing
