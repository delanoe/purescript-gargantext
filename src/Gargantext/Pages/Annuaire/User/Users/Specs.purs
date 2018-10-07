module Gargantext.Pages.Annuaire.User.Users.Specs
       (module Gargantext.Pages.Annuaire.User.Users.Specs.Renders,
        layoutUser)
       where

import Gargantext.Pages.Annuaire.User.Users.Specs.Renders

import Thermite (Spec, simpleSpec)
import Gargantext.Pages.Annuaire.User.Users.Types (Action, State)
import Gargantext.Pages.Annuaire.User.Users.API (performAction)

layoutUser :: Spec State {} Action
layoutUser = simpleSpec performAction render
