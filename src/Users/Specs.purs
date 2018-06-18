module Users.Specs
       (module Users.Specs.Renders,
        layoutUser)
       where

import Users.Specs.Renders

import Control.Monad.Aff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Thermite (Spec, simpleSpec)
import Users.Types.Types (Action, State, performAction)

layoutUser :: forall props eff . Spec ( console :: CONSOLE
                                        , ajax    :: AJAX
                                        , dom     :: DOM
                                        | eff
                                        ) State props Action
layoutUser = simpleSpec performAction render
