module Gargantext.Pages.Corpus.User.Users.Specs
       (module Gargantext.Pages.Corpus.User.Users.Specs.Renders,
        layoutUser)
       where

import Gargantext.Pages.Corpus.User.Users.Specs.Renders

import Thermite (Spec, simpleSpec)
import Gargantext.Pages.Corpus.User.Users.Types (Action, State)
import Gargantext.Pages.Corpus.User.Users.API (performAction)

layoutUser :: Spec State {} Action
layoutUser = simpleSpec performAction render
