module Gargantext.Pages.Corpus.Tabs.States where

import Data.Lens (Lens', lens)
import Gargantext.Pages.Corpus.Tabs.Documents as D
import Gargantext.Pages.Corpus.Tabs.Sources   as S
import Gargantext.Pages.Corpus.Tabs.Authors   as A
import Gargantext.Pages.Corpus.Tabs.Terms     as T
import Gargantext.Components.Tab as Tab


type State =
  { docsView    :: D.State
  , authorsView :: A.State
  , sourcesView :: S.State
  , termsView   :: T.State
  , activeTab   :: Int
  }

initialState :: State
initialState =
  { docsView    : D.initialState
  , authorsView : A.initialState
  , sourcesView : S.initialState
  , termsView   : T.initialState
  , activeTab : 0
  }

_doclens :: Lens' State D.State
_doclens = lens (\s -> s.docsView) (\s ss -> s {docsView = ss})

_authorlens :: Lens' State A.State
_authorlens = lens (\s -> s.authorsView) (\s ss -> s {authorsView = ss})

_sourcelens :: Lens' State S.State
_sourcelens = lens (\s -> s.sourcesView) (\s ss -> s {sourcesView = ss})

_termslens :: Lens' State T.State
_termslens = lens (\s -> s.termsView) (\s ss -> s {termsView = ss})

_tablens :: Lens' State Tab.State
_tablens = lens (\s -> s.activeTab) (\s ss -> s {activeTab = ss})
