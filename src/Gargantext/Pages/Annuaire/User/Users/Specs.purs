module Gargantext.Pages.Annuaire.User.Users.Specs
       (module Gargantext.Pages.Annuaire.User.Users.Specs.Renders,
        brevetSpec,
        projectSpec,
        facets,
        layoutUser)
       where


import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))
import Thermite (Render, PerformAction, Spec, focus, noState, defaultPerformAction, simpleSpec)
import Gargantext.Prelude
import Gargantext.Components.Tab as Tab
import Gargantext.Pages.Annuaire.User.Brevets as B
import Gargantext.Pages.Annuaire.User.Users.Specs.Documents as P
import Gargantext.Pages.Annuaire.User.Users.Types (Action(..), State, _tablens, _tabAction)
import Gargantext.Pages.Annuaire.User.Users.API (fetchUser)
import Gargantext.Pages.Annuaire.User.Users.Specs.Renders (render)

layoutUser :: Spec State {} Action
layoutUser = simpleSpec performAction render
  where
    performAction :: PerformAction State {} Action
    performAction (FetchUser userId) _ _ = fetchUser userId
    performAction (TabA _) _ _ = pure unit

brevetSpec :: Spec State {} Action
brevetSpec = noState B.brevetsSpec

projets :: Spec {} {} Void
projets = simpleSpec defaultPerformAction render
  where
    render :: Render {} {} Void
    render dispatch _ state _ =
      []

projectSpec :: Spec State {} Action
projectSpec = noState projets

publicationSpec :: Spec State {} Action
publicationSpec = noState P.publicationSpec

facets :: Spec State {} Action
facets = Tab.tabs _tablens _tabAction $ fromFoldable
         [ Tuple "Publications (12)" publicationSpec
         , Tuple "Brevets (2)" brevetSpec
         , Tuple "Projets IMT (5)" projectSpec
         ]
