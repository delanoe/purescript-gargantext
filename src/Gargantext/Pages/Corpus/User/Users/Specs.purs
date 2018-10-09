module Gargantext.Pages.Corpus.User.Users.Specs
       (module Gargantext.Pages.Corpus.User.Users.Specs.Renders,
        layoutUser)
       where

import Gargantext.Pages.Corpus.User.Users.Specs.Renders

import Thermite (Spec, simpleSpec)
import Gargantext.Pages.Corpus.User.Users.Actions
import Gargantext.Pages.Corpus.User.Users.States
import Gargantext.Pages.Corpus.User.Users.API (performAction)

layoutUser :: Spec State {} Action
layoutUser = simpleSpec performAction render

publicationSpec :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
publicationSpec = focus _publens _pubAction P.publicationSpec

brevetSpec :: forall eff props. Spec (dom :: DOM, console::CONSOLE, ajax :: AJAX | eff) State  props Action
brevetSpec = focus _brevetslens _brevetsAction B.brevetsSpec

projectSpec :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
projectSpec = focus _projectslens _projectsAction PS.projets

facets :: forall eff props. Spec ( dom :: DOM, console :: CONSOLE, ajax :: AJAX| eff) State props Action
facets = tabs _tablens _tabAction $ fromFoldable
         [ Tuple "Publications (12)" publicationSpec
         , Tuple "Brevets (2)" brevetSpec
         , Tuple "Projets IMT (5)" projectSpec
      ]
