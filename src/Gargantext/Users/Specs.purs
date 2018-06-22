module Gargantext.Users.Specs
       (module Gargantext.Users.Specs.Renders,
        layoutUser)
       where

import Gargantext.Users.Specs.Renders

import Control.Monad.Aff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Thermite (Spec, simpleSpec)
import Gargantext.Users.Types (Action, State)
import Gargantext.Users.API (performAction)

layoutUser :: forall props eff . Spec ( console :: CONSOLE
                                        , ajax    :: AJAX
                                        , dom     :: DOM
                                        | eff
                                        ) State props Action
layoutUser = simpleSpec performAction render
