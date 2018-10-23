module Gargantext.Pages.Annuaire.User.Contacts.Types.Lens where

import Gargantext.Pages.Annuaire.User.Brevets as B
import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, prism)
import Data.Maybe (Maybe)
import Gargantext.Pages.Annuaire.User.Contacts.Types.States (Action(..), State)
import Gargantext.Pages.Annuaire.User.Contacts.Types.Types (Contact)
import Gargantext.Pages.Annuaire.User.Contacts.Specs.Documents as P
import Gargantext.Components.Tab as Tab
import Thermite (Spec, noState)

_contact :: Lens' State (Maybe Contact)
_contact = lens (\s -> s.contact) (\s ss -> s{contact = ss})

_tablens :: Lens' State Tab.State
_tablens = lens (\s -> s.activeTab) (\s ss -> s {activeTab = ss})

_tabAction :: Prism' Action Tab.Action
_tabAction = prism TabA \ action ->
  case action of
    TabA laction -> Right laction
    _-> Left action

publicationSpec :: Spec State {} Action
publicationSpec = noState P.publicationSpec
