module Gargantext.Pages.Corpus.Doc.Facets.Actions where


import Data.Lens (Prism', prism)
import Data.Either (Either(..))

import Gargantext.Pages.Corpus.Doc.Facets.Documents as DV
import Gargantext.Pages.Corpus.Doc.Facets.Sources as SV
import Gargantext.Pages.Corpus.Doc.Facets.Authors as AV
import Gargantext.Pages.Corpus.Doc.Facets.Terms as TV
import Gargantext.Components.Tab as Tab

data Action
  = DocviewA DV.Action
  | SourceviewA SV.Action
  | AuthorviewA AV.Action
  | TermsviewA TV.Action
  | TabViewA   Tab.Action

_docAction :: Prism' Action DV.Action
_docAction = prism DocviewA \ action ->
  case action of
    DocviewA laction -> Right laction
    _-> Left action

_authorAction :: Prism' Action AV.Action
_authorAction = prism AuthorviewA \ action ->
  case action of
    AuthorviewA laction -> Right laction
    _-> Left action

_sourceAction :: Prism' Action SV.Action
_sourceAction = prism SourceviewA \ action ->
  case action of
    SourceviewA laction -> Right laction
    _-> Left action

_termsAction :: Prism' Action TV.Action
_termsAction = prism TermsviewA \ action ->
  case action of
    TermsviewA laction -> Right laction
    _-> Left action

_tabAction :: Prism' Action Tab.Action
_tabAction = prism TabViewA \ action ->
  case action of
    TabViewA laction -> Right laction
    _-> Left action



