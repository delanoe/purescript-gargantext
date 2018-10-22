module Gargantext.Pages.Annuaire.User.Contacts.Specs
       (module Gargantext.Pages.Annuaire.User.Contacts.Specs.Renders,
        layoutUser)
       where

import Gargantext.Pages.Annuaire.User.Contacts.Specs.Renders

import Thermite (Spec, simpleSpec)
import Gargantext.Pages.Annuaire.User.Contacts.Types (Action, State)
import Gargantext.Pages.Annuaire.User.Contacts.API (performAction)

layoutUser :: Spec State {} Action
layoutUser = simpleSpec performAction render
