module Gargantext.Pages.Corpus.Tabs.Actions where


import Data.Lens (Prism', prism)
import Data.Either (Either(..))
import Data.Void (Void)

import Gargantext.Pages.Corpus.Tabs.Documents as DV
import Gargantext.Pages.Corpus.Tabs.Ngrams.NgramsTable as NG
import Gargantext.Components.Tab as Tab

data Action
  = DocViewA DV.Action -- = Void
  | TabViewA   Tab.Action -- = ChangeTab which is only used locally

_docAction :: Prism' Action DV.Action
_docAction = prism DocViewA \ action ->
  case action of
    DocViewA laction -> Right laction
    _-> Left action

_tabAction :: Prism' Action Tab.Action
_tabAction = prism TabViewA \ action ->
  case action of
    TabViewA laction -> Right laction
    _-> Left action
