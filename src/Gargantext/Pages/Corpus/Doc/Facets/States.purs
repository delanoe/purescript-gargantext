module Gargantext.Pages.Corpus.Doc.Facets.States where

import Data.Lens (Lens', lens)
import Gargantext.Pages.Corpus.Doc.Document as DV
import Gargantext.Pages.Corpus.Doc.Facets.Sources as SV
import Gargantext.Pages.Corpus.Doc.Facets.Authors as AV
import Gargantext.Pages.Corpus.Doc.Facets.Terms as TV
import Gargantext.Components.Tab as Tab


type State =
  { docview :: DV.State
  , authorview :: AV.State
  , sourceview :: SV.State
  , termsview :: TV.State
  , activeTab :: Int
  }

initialState :: State
initialState =
  { docview : DV.tdata
  , authorview : AV.initialState
  , sourceview : SV.initialState
  , termsview : TV.initialState
  , activeTab : 0
  }

_doclens :: Lens' State DV.State
_doclens = lens (\s -> s.docview) (\s ss -> s {docview = ss})

_authorlens :: Lens' State AV.State
_authorlens = lens (\s -> s.authorview) (\s ss -> s {authorview = ss})

_sourcelens :: Lens' State SV.State
_sourcelens = lens (\s -> s.sourceview) (\s ss -> s {sourceview = ss})

_termslens :: Lens' State TV.State
_termslens = lens (\s -> s.termsview) (\s ss -> s {termsview = ss})

_tablens :: Lens' State Tab.State
_tablens = lens (\s -> s.activeTab) (\s ss -> s {activeTab = ss})




