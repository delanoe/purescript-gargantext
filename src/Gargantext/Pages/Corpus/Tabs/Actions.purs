module Gargantext.Pages.Corpus.Tabs.Actions where

import Data.Lens (Prism', prism)
import Data.Either (Either(..))

import Gargantext.Components.Tab as Tab

data Action
  = TabAction Tab.Action -- = ChangeTab which is only used locally

_TabAction :: Prism' Action Tab.Action
_TabAction = prism TabAction \ action ->
  case action of
    TabAction laction -> Right laction
    _-> Left action
