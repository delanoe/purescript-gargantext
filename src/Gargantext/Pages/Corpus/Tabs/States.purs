module Gargantext.Pages.Corpus.Tabs.States where

import Data.Lens (Lens', lens)
import Gargantext.Prelude
import Gargantext.Pages.Corpus.Tabs.Documents as D
import Gargantext.Pages.Corpus.Tabs.Ngrams.NgramsTable as N
import Gargantext.Components.Tab as Tab


type State =
  { docsView    :: D.State
  , activeTab   :: Int
  }

initialState :: {} -> State
initialState _ =
  { docsView    :
     { documentIdsToDelete : mempty
     }
  , activeTab   : 0
  }

_doclens :: Lens' State D.State
_doclens = lens (\s -> s.docsView) (\s ss -> s {docsView = ss})

_tablens :: Lens' State Tab.State
_tablens = lens (\s -> s.activeTab) (\s ss -> s {activeTab = ss})
