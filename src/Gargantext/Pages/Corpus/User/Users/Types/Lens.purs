module Gargantext.Pages.Corpus.User.Users.Types.Lens where

import Gargantext.Pages.Corpus.User.Brevets as B
import Control.Monad.Aff.Console (CONSOLE)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, prism)
import Data.Maybe (Maybe)
import Gargantext.Pages.Corpus.User.Users.Types.States (Action(..), State)
import Gargantext.Pages.Corpus.User.Users.Types.Types (User)
import Network.HTTP.Affjax (AJAX)
import Gargantext.Pages.Folder as PS
import Gargantext.Pages.Corpus.Doc.Facets.Documents as P
import Gargantext.Components.Tab as Tab
import Thermite (Spec, focus)

_user :: Lens' State (Maybe User)
_user = lens (\s -> s.user) (\s ss -> s{user = ss})

_tablens :: Lens' State Tab.State
_tablens = lens (\s -> s.activeTab) (\s ss -> s {activeTab = ss})

_tabAction :: Prism' Action Tab.Action
_tabAction = prism TabA \ action ->
  case action of
    TabA laction -> Right laction
    _-> Left action

_publens :: Lens' State P.State
_publens = lens (\s -> s.publications) (\s ss -> s { publications= ss})

_pubAction :: Prism' Action P.Action
_pubAction = prism PublicationA \ action ->
  case action of
    PublicationA laction -> Right laction
    _-> Left action

publicationSpec :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
publicationSpec = focus _publens _pubAction P.publicationSpec

_brevetslens :: Lens' State B.State
_brevetslens = lens (\s -> s.brevets) (\s ss -> s {brevets = ss})

_brevetsAction :: Prism' Action B.Action
_brevetsAction = prism BrevetsA \ action ->
  case action of
    BrevetsA laction -> Right laction
    _-> Left action

_projectslens :: Lens' State PS.State
_projectslens = lens (\s -> s.projects) (\s ss -> s {projects = ss})

_projectsAction :: Prism' Action PS.Action
_projectsAction = prism ProjectsA \ action ->
  case action of
    ProjectsA laction -> Right laction
    _-> Left action
