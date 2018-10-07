module Gargantext.Pages.Annuaire.User.Brevets where

import Prelude
import Thermite (Render, Spec, defaultPerformAction, simpleSpec)

brevetsSpec :: Spec {} {} Void
brevetsSpec = simpleSpec defaultPerformAction render
  where
    render :: Render {} {} Void
    render dispatch _ state _ =
      []
