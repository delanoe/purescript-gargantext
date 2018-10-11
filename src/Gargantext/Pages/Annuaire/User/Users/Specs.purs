module Gargantext.Pages.Annuaire.User.Users.Specs
       (module Gargantext.Pages.Annuaire.User.Users.Specs.Renders,
        layoutUser)
       where


import Thermite (PerformAction, Spec, simpleSpec)
import Gargantext.Prelude
import Gargantext.Pages.Annuaire.User.Users.Types (Action(..), State)
import Gargantext.Pages.Annuaire.User.Users.API (fetchUser)
import Gargantext.Pages.Annuaire.User.Users.Specs.Renders (render)

layoutUser :: Spec State {} Action
layoutUser = simpleSpec performAction render
  where
    performAction :: PerformAction State {} Action
    performAction (FetchUser userId) _ _ = fetchUser userId
    performAction (TabA _) _ _ = pure unit
