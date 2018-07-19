module Gargantext.Pages.Corpus.User.Users.Actions  where

import Gargantext.Pages.Folder as PS
import Gargantext.Pages.Corpus.Doc.Facets.Documents as P
import Gargantext.Components.Tab as Tab

data Action
  = NoOp
  | PublicationA P.Action
  | BrevetsA B.Action
  | ProjectsA PS.Action
  | TabA Tab.Action
  | FetchUser Int

----------------------------------------------

_tabAction :: Prism' Action Tab.Action
_tabAction = prism TabA \ action ->
  case action of
    TabA laction -> Right laction
    _-> Left action


_pubAction :: Prism' Action P.Action
_pubAction = prism PublicationA \ action ->
  case action of
    PublicationA laction -> Right laction
    _-> Left action


_brevetsAction :: Prism' Action B.Action
_brevetsAction = prism BrevetsA \ action ->
  case action of
    BrevetsA laction -> Right laction
    _-> Left action

_projectsAction :: Prism' Action PS.Action
_projectsAction = prism ProjectsA \ action ->
  case action of
    ProjectsA laction -> Right laction
    _-> Left action
