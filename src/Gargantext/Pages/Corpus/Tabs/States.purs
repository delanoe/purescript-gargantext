module Gargantext.Pages.Corpus.Tabs.States where

import Data.Lens (Lens', lens)
import Gargantext.Pages.Corpus.Tabs.Documents as D
import Gargantext.Pages.Corpus.Tabs.Ngrams.NgramsTable as N
import Gargantext.Components.Tab as Tab


type State =
  { docsView    :: D.State
  , ngramsView  :: {} -- N.State TODO needed
  , activeTab   :: Int
  }


initialState :: State
initialState =
  { docsView    :
     { documents : D.sampleData'
     , deleteRows : false
     , deleteRowId : []
     , delete : []
     }
  , ngramsView  : {} -- N.initialState
  , activeTab   : 0
  }

_doclens :: Lens' State D.State
_doclens = lens (\s -> s.docsView) (\s ss -> s {docsView = ss})

_ngramsView :: Lens' State {} -- N.State
_ngramsView = lens (\s -> s.ngramsView) (\s ss -> s {ngramsView = ss})

_tablens :: Lens' State Tab.State
_tablens = lens (\s -> s.activeTab) (\s ss -> s {activeTab = ss})
