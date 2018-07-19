module Gargantext.Pages.Corpus.Doc.Facets.Specs where

import Prelude hiding (div)

import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Lens (Lens', Prism', lens, prism)
import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))

import Gargantext.Pages.Corpus.Doc.Facets.States (State(..), _doclens, _sourcelens, _authorlens, _termslens, _tablens)
import Gargantext.Pages.Corpus.Doc.Facets.Actions (Action(..), _docAction, _sourceAction, _authorAction, _termsAction, _tabAction)

import Gargantext.Pages.Corpus.Doc.Facets.Documents as DV
import Gargantext.Pages.Corpus.Doc.Facets.Sources as SV
import Gargantext.Pages.Corpus.Doc.Facets.Authors as AV
import Gargantext.Pages.Corpus.Doc.Facets.Terms as TV
import Gargantext.Components.Tab as Tab

import Network.HTTP.Affjax (AJAX)
import Thermite (Spec, focus)



tab1 :: forall eff props. Spec ( dom :: DOM, console :: CONSOLE, ajax :: AJAX| eff) State props Action
tab1 = Tab.tabs _tablens _tabAction $ fromFoldable [ Tuple "Doc View"    docPageSpec
                                                   , Tuple "Author View" authorPageSpec
                                                   , Tuple "Source View" sourcePageSpec
                                                   , Tuple "Terms View"  termsPageSpec
                                                   ]

docPageSpec :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
docPageSpec = focus _doclens _docAction DV.layoutDocview

authorPageSpec :: forall eff props. Spec (dom :: DOM, console::CONSOLE, ajax :: AJAX | eff) State  props Action
authorPageSpec = focus _authorlens _authorAction AV.authorspec'

sourcePageSpec :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
sourcePageSpec = focus _sourcelens _sourceAction SV.sourcespec'

termsPageSpec :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
termsPageSpec = focus _termslens _termsAction TV.termSpec'



