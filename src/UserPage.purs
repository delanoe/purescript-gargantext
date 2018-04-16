module UserPage where

import Tab

import Brevets as B
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, prism)
import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))
import DocView as DV
import Network.HTTP.Affjax (AJAX)
import Prelude hiding (div)
import Projects as PS
import Publications as P
import React.DOM (a, div, h3, h5, h6, i, img, li, nav, small, span, table, tbody, td, text, th, thead, tr, ul)
import React.DOM.Props (_data, _id, aria, className, href, role, scope, src)
import Tab as Tab
import Thermite (PerformAction, Render, Spec, focus, modifyState, simpleSpec)

type State =
  { activeTab :: Int
  , publications :: P.State
  , brevets :: B.State
  , projects :: PS.State
  }


initialState :: State
initialState =
  { activeTab : 0
  , publications : P.initialState
  , brevets : B.initialState
  , projects : PS.initialState
  }

data Action
  = NoOp
  | PublicationA P.Action
  | BrevetsA B.Action
  | ProjectsA PS.Action
  | TabA Tab.Action


performAction :: forall eff props. PerformAction ( console :: CONSOLE
                                                 , ajax    :: AJAX
                                                 , dom     :: DOM
                                                 | eff ) State props Action
performAction NoOp _ _ = void do
  modifyState id


performAction _ _ _ = void do
  modifyState id




layoutUser :: forall props eff . Spec ( console :: CONSOLE
                                        , ajax    :: AJAX
                                        , dom     :: DOM
                                        | eff
                                        ) State props Action
layoutUser = simpleSpec performAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      [
        div [className "container-fluid"]
        [ div [className "row", _id "user-page-header"]
          [ div [className "col-md-2"]
            [ h3 [] [text "User Name"]
            ]
          , div [className "col-md-8"] []
          , div [className "col-md-2"]
            [ span [] [text ""]
            ]
          ]
        , div [className "row", _id "user-page-info"]
          [
            div [className "col-md-12"]
            [ div [className "row"]
              [ div [className "col-md-2"]
                [ img [src "/images/Gargantextuel-212x300.jpg"] []
                ]
              , div [className "col-md-1"] []
              , div [className "col-md-8"]
                [
                  ul [className "list-group"]
                  [
                    li [className "list-group-item justify-content-between"]
                    [  span [] [text "Fonction"]
                    , span [className "badge badge-default badge-pill"] [text "Enseignant chercheur"]
                    ]
                  , li [className "list-group-item justify-content-between"]
                    [  span [] [text "Entité, service"]
                    , span [className "badge badge-default badge-pill"] [text "Mines Saint-Etienne SPIN -PTSI"]
                    ]

                  , li [className "list-group-item justify-content-between"]
                    [  span [] [text "Téléphone"]
                    , span [className "badge badge-default badge-pill"] [text "(+33) 04 77 42 0070"]
                    ]
                   , li [className "list-group-item justify-content-between"]
                    [  span [] [text "Courriel"]
                    , span [className "badge badge-default badge-pill"] [text "gargantua@rabelais.fr"]
                    ]
                   , li [className "list-group-item justify-content-between"]
                    [  span [] [text "Bureau"]
                    , span [className "badge badge-default badge-pill"] [text "D1/10"]
                    ]
                   , li [className "list-group-item justify-content-between"]
                    [  span [] [text "Apellation"]
                    , span [className "badge badge-default badge-pill"] [text "Maître de conférences (EPA)"]
                    ]
                   , li [className "list-group-item justify-content-between"]
                    [  span [] [text "Lieu"]
                    , span [className "badge badge-default badge-pill"] [text "Saint-Etienne, 158 Cours Fauriel"]
                    ]

                  ]
                ]
              ]
            ]
          ]
        , div [className "row",_id "user-page-footer"]
          [ div [className "col-md-12"]
              []

            ]
        ]
      ]



_tablens :: Lens' State Tab.State
_tablens = lens (\s -> s.activeTab) (\s ss -> s {activeTab = ss})

_tabAction :: Prism' Action Tab.Action
_tabAction = prism TabA \ action ->
  case action of
    TabA laction -> Right laction
    _-> Left action



_publens :: Lens' State P.State
_publens = lens (\s -> s.publications) (\s ss -> s { publications= ss})


_pubAction :: Prism' Action P.Action
_pubAction = prism PublicationA \ action ->
  case action of
    PublicationA laction -> Right laction
    _-> Left action



publicationSpec :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
publicationSpec = focus _publens _pubAction P.publicationSpec


_brevetslens :: Lens' State B.State
_brevetslens = lens (\s -> s.brevets) (\s ss -> s {brevets = ss})


_brevetsAction :: Prism' Action B.Action
_brevetsAction = prism BrevetsA \ action ->
  case action of
    BrevetsA laction -> Right laction
    _-> Left action

brevetSpec :: forall eff props. Spec (dom :: DOM, console::CONSOLE, ajax :: AJAX | eff) State  props Action
brevetSpec = focus _brevetslens _brevetsAction B.brevetsSpec


_projectslens :: Lens' State PS.State
_projectslens = lens (\s -> s.projects) (\s ss -> s {projects = ss})


_projectsAction :: Prism' Action PS.Action
_projectsAction = prism ProjectsA \ action ->
  case action of
    ProjectsA laction -> Right laction
    _-> Left action


projectSpec :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
projectSpec = focus _projectslens _projectsAction PS.projets


facets :: forall eff props. Spec ( dom :: DOM, console :: CONSOLE, ajax :: AJAX| eff) State props Action
facets = tabs _tablens _tabAction $ fromFoldable
         [ Tuple "Publications(12)" publicationSpec
         , Tuple "Brevets (2)" brevetSpec
         , Tuple "Projets IMT (5)" projectSpec
      ]
