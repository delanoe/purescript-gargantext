module Gargantext.Pages.Corpus.Tabs.States where

import Data.Lens (Lens', lens)
import Gargantext.Components.Tab as Tab

type State =
  { activeTab :: Int
  }

initialState :: {} -> State
initialState _ =
  { activeTab: 0
  }

_activeTab :: Lens' State Tab.State
_activeTab = lens _.activeTab (\s ss -> s {activeTab = ss})
