module Gargantext.Pages.Corpus.User.Users.Specs
       (module Gargantext.Pages.Corpus.User.Users.Specs.Renders,
        layoutUser)
       where

import Gargantext.Pages.Corpus.User.Users.Specs.Renders

import Control.Monad.Aff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Thermite (Spec, simpleSpec)
import Gargantext.Pages.Corpus.User.Users.Types (Action, State)
import Gargantext.Pages.Corpus.User.Users.API (performAction)

layoutUser :: forall props eff . Spec ( console :: CONSOLE
                                        , ajax    :: AJAX
                                        , dom     :: DOM
                                        | eff
                                        ) State props Action
layoutUser = simpleSpec performAction render
