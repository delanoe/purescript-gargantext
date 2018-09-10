module Gargantext.Pages.Corpus.Doc.Facets.Specs where

import Prelude hiding (div)

import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))

import Gargantext.Pages.Corpus.Doc.Facets.States (State(), _doclens, _sourcelens, _authorlens, _termslens, _tablens, initialState)
import Gargantext.Pages.Corpus.Doc.Facets.Actions (Action(), _docAction, _sourceAction, _authorAction, _termsAction, _tabAction)

import Gargantext.Pages.Corpus.Doc.Facets.Documents as DV
import Gargantext.Pages.Corpus.Doc.Facets.Sources as SV
import Gargantext.Pages.Corpus.Doc.Facets.Authors as AV
import Gargantext.Pages.Corpus.Doc.Facets.Terms as TV
import Gargantext.Components.Tab as Tab

import Thermite (Spec, focus, hide)


tab1 :: forall action. Spec {} {} action
tab1 = hide initialState tab1'

tab1' :: Spec State {} Action
tab1' = Tab.tabs _tablens _tabAction $ fromFoldable [ Tuple "Doc View"    docPageSpec
                                                   , Tuple "Author View" authorPageSpec
                                                   , Tuple "Source View" sourcePageSpec
                                                   , Tuple "Terms View"  termsPageSpec
                                                   ]

docPageSpec :: forall props. Spec State props Action
docPageSpec = focus _doclens _docAction DV.layoutDocview

authorPageSpec :: forall props. Spec State  props Action
authorPageSpec = focus _authorlens _authorAction AV.authorspec'

sourcePageSpec :: forall props. Spec State props Action
sourcePageSpec = focus _sourcelens _sourceAction SV.sourcespec'

termsPageSpec :: forall props. Spec State props Action
termsPageSpec = focus _termslens _termsAction TV.termSpec'
