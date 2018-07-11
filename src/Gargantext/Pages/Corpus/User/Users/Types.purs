module Gargantext.Users.Types
       (module Gargantext.Users.Types.Types,
        module Gargantext.Users.Types.Lens,
        module Gargantext.Users.Types.States,
        brevetSpec,
        projectSpec,
        facets
       )
       where

import Prelude (($))

import Gargantext.Users.Types.Lens
import Gargantext.Users.Types.Types
import Gargantext.Users.Types.States
import Brevets as B
import Control.Monad.Aff.Console (CONSOLE)
import DOM (DOM)
import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX)
import Projects as PS
import Gargantext.Components.Tab (tabs)
import Thermite (Spec, focus)

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
