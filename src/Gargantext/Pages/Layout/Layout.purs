module Gargantext.Layout where

import Prelude hiding (div)

import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Foldable (fold, intercalate)
import Data.Lens (over)
import Data.Maybe (Maybe(Nothing, Just))
import Gargantext.Components.Data.Lang (Lang(..))
import Gargantext.Components.Login as LN
import Gargantext.Components.Tree as Tree
import Gargantext.Pages.Layout.Action (Action(..), performAction)
import Gargantext.Layout.Lens (_addCorpusAction, _addCorpusState, _corpusAction, _corpusState, _dashBoardAction, _dashBoardSate, _docAnnotationViewAction, _docAnnotationViewState, _docViewAction, _docViewState, _graphExplorerAction, _graphExplorerState, _landingAction, _landingState, _loginAction, _loginState, _ngAction, _ngState, _searchAction, _searchState, _tabviewAction, _tabviewState, _treeAction, _treeState, _userPageAction, _userPageState)
import Gargantext.Pages.Layout.State (AppState, E)
import Gargantext.Pages.Corpus as AC
import Gargantext.Pages.Corpus.Doc.Annotation as D
import Gargantext.Pages.Corpus.Doc.Body as CA
import Gargantext.Pages.Corpus.Doc.Document as DV
import Gargantext.Pages.Corpus.Doc.Facets as TV
import Gargantext.Pages.Corpus.Doc.Facets.Dashboard as Dsh
import Gargantext.Pages.Corpus.Doc.Facets.Graph as GE
import Gargantext.Pages.Corpus.Doc.Facets.Terms.NgramsTable as NG
import Gargantext.Pages.Corpus.User.Users as U
import Gargantext.Pages.Home as L
import Gargantext.Pages.Search as S
import Gargantext.Router (Routes(..))
import Network.HTTP.Affjax (AJAX)
import React (ReactElement)
import React.DOM (a, button, div, footer, hr, img, input, li, p, span, text, ul)
import React.DOM.Props (_data, _id, _type, aria, className, href, onChange, onClick, placeholder, role, src, style, tabIndex, target, title)
import Thermite (Render, Spec, _render, defaultPerformAction, defaultRender, focus, simpleSpec, withState)
import Unsafe.Coerce (unsafeCoerce)

pagesComponent :: forall props eff. AppState -> Spec (E eff) AppState props Action
pagesComponent s =
  case s.currentRoute of
    Just route -> selectSpec route
    Nothing    -> selectSpec Home
  where
    selectSpec :: Routes -> Spec ( ajax    :: AJAX
                                 , console :: CONSOLE
                                 , dom     :: DOM
                                 | eff
                                 ) AppState props Action
    selectSpec CorpusAnalysis    = layout0 $ focus _corpusState  _corpusAction CA.spec'
    selectSpec Login             = focus _loginState _loginAction LN.renderSpec
    selectSpec Home              = layout0 $ focus _landingState   _landingAction   (L.layoutLanding EN)
    selectSpec AddCorpus         = layout0 $ focus _addCorpusState _addCorpusAction AC.layoutAddcorpus
    selectSpec DocView           = layout0 $ focus _docViewState   _docViewAction   DV.layoutDocview
    selectSpec (UserPage i)      = layout0 $ focus _userPageState  _userPageAction  U.layoutUser
    selectSpec (DocAnnotation i) = layout0 $ focus _docAnnotationViewState  _docAnnotationViewAction  D.docview
    selectSpec Tabview           = layout0 $ focus _tabviewState  _tabviewAction  TV.tab1
    -- To be removed
    selectSpec SearchView        = layout0 $ focus _searchState _searchAction  S.searchSpec
    selectSpec NGramsTable       = layout0 $ focus _ngState _ngAction  NG.ngramsTableSpec
    selectSpec PGraphExplorer    = focus _graphExplorerState _graphExplorerAction  GE.specOld
    selectSpec Dashboard         = layout0 $ focus _dashBoardSate _dashBoardAction Dsh.layoutDashboard

    -- selectSpec _ = simpleSpec defaultPerformAction defaultRender

routingSpec :: forall props eff. Spec (ajax :: AJAX, console :: CONSOLE, dom :: DOM |eff) AppState props Action
routingSpec = simpleSpec performAction defaultRender

layout0 :: forall eff props. Spec (E eff) AppState props Action
                          -> Spec (E eff) AppState props Action
layout0 layout =
  fold
  [ layoutSidebar divSearchBar
  , outerLayout
  , layoutFooter
  ]
  where
    outerLayout1 = simpleSpec defaultPerformAction defaultRender
    outerLayout :: Spec (E eff) AppState props Action
    outerLayout =
      cont $ fold
      [ withState \st ->
         if ((\(LN.State s) -> s.loginC) st.loginState == true) then ls as
         else outerLayout1
      , rs bs      ]
    ls   = over _render \render d p s c -> [ div [className "col-md-2" ] (render d p s c) ]
    rs   = over _render \render d p s c -> [ div [className "col-md-10"] (render d p s c) ]
    cont = over _render \render d p s c -> [ div [ className "row"     ] (render d p s c) ]

    as = focus _treeState _treeAction Tree.treeview

    bs = innerLayout $ layout

    innerLayout :: Spec (E eff) AppState props Action
                -> Spec (E eff) AppState props Action
    innerLayout = over _render \render d p s c ->
      [  div [_id "page-wrapper"]
        [
          div [className "container-fluid"]  (render d p s c)
        ]
      ]

layoutSidebar ::  forall props eff. Spec (E eff) AppState props Action
                -> Spec (E eff) AppState props Action
layoutSidebar = over _render \render d p s c ->
      [ div [ _id "dafixedtop"
            , className "navbar navbar-inverse navbar-fixed-top"
            , role "navigation"
            ] [ div [className "container"]
                    [ div [ className "navbar-inner" ]
                          [ divLogo
                          ,  div [ className "collapse navbar-collapse"]
                             $ [ divDropdownLeft]
                             <> render d p s c <>
                             [ divDropdownRight d]
                          ]
                    ]
              ]
      ]


divLogo :: ReactElement
divLogo = a [ className "navbar-brand logoSmall"
            , href "#/"
            ] [ img [ src "images/logoSmall.png"
                    , title "Back to home."
                    ] []
              ]

divDropdownLeft :: ReactElement
divDropdownLeft = divDropdownLeft' (LiNav { title : "About Gargantext"
                                          , href  : "#"
                                          , icon  : "glyphicon glyphicon-info-sign"
                                          , text  : "Info"
                                          }
                                    )

divDropdownLeft' :: LiNav -> ReactElement
divDropdownLeft' mb =  ul [className "nav navbar-nav"]
                         [ ul [className "nav navbar-nav pull-left"]
                              [ li [className "dropdown"]
                                   [ menuButton mb
                                   , menuElements'
                                   ]
                               ]
                          ]

menuButton :: LiNav -> ReactElement
menuButton (LiNav { title : title'
                  , href : href'
                  , icon : icon'
                  , text : text'
                  }) = a [ className "dropdown-toggle navbar-text"
                        , _data {toggle: "dropdown"}
                        , href href', role "button"
                        , title title'
                        ][ span [ aria {hidden : true}
                                , className icon'
                                ] []
                         , text (" " <> text')
                         ]

menuElements' :: ReactElement
menuElements' = menuElements-- title, icon, text
  [ -- ===========================================================
    [ LiNav { title : "Quick start, tutorials and methodology"
            , href  : "https://iscpif.fr/gargantext/your-first-map/"
            , icon  : "fas fa-book"
            , text  : "Documentation"
            }
    , LiNav { title : "Report bug here"
            , href  : "https://www.iscpif.fr/gargantext/feedback-and-bug-reports/"
            , icon  : "glyphicon glyphicon-bullhorn"
            , text  : "Feedback"
            }
    ]
    , -----------------------------------------------------------
    [ LiNav { title : "Interactive chat"
            , href  : "https://chat.iscpif.fr/channel/gargantext"
            , icon  : "fab fa-rocketchat"
            , text  : "Chat"
            }
    , LiNav { title : "Asynchronous discussions"
            , href  : "https://discourse.iscpif.fr/c/gargantext"
            , icon  : "fab fa-discourse"
            , text  : "Forum"
            }
    ]
    ,------------------------------------------------------------
    [ LiNav { title : "More about us (you)"
            , href  : "https://iscpif.fr"
            , icon  : "fas fa-question-circle"
            , text  : "About"
            }
    ]
  ] -- ===========================================================

-- | Menu in the sidebar, syntactic sugar
menuElements :: Array (Array LiNav) -> ReactElement
menuElements ns = dropDown $ intercalate divider $ map (map liNav) ns
  where
    dropDown :: Array ReactElement -> ReactElement
    dropDown = ul [className "dropdown-menu"]

    divider :: Array ReactElement
    divider = [li [className "divider"] []]

-- | surgar for target : "blank"
--data LiNav_ = LiNav_ { title  :: String
--                     , href   :: String
--                     , icon   :: String
--                     , text   :: String
--                     , target :: String
--                     }

data LiNav = LiNav { title :: String
                   , href  :: String
                   , icon  :: String
                   , text  :: String
                   }

liNav :: LiNav -> ReactElement
liNav (LiNav { title : title'
             , href  : href'
             , icon  : icon'
             , text  : text'
             }
      ) = li [] [ a [ tabIndex (-1)
                    , target "blank"
                    , title title'
                    , href href'
                    ] [ span [ className icon' ] []
                      , text $ " " <> text'
                      ]
                ]

-- TODO put the search form in the center of the navBar
divSearchBar :: forall props eff. Spec (ajax :: AJAX, console :: CONSOLE, dom :: DOM |eff) AppState props Action
divSearchBar = simpleSpec performAction render
  where
    render :: Render AppState props Action
    render dispatch _ state _ = [div [ className "" ] [ searchbar']]
      where
        searchbar' = ul [ className "nav navbar-nav col-md-6 col-md-offset-3"
                        , style { "marginLeft" : "15%"}
                        ] [ div [className "navbar-form"]
                            [ input [ className   "search-query"
                                    , placeholder "Query, URL or FILE (works with Firefox or Chromium browsers)"
                                    , _type "text"
                                    , style { height: "35px"
                                            , width: "400px"
                                            }
                                    , onChange \e -> dispatch $ Search (unsafeCoerce e).target.value
                                    ] []
                            ,  button [onClick \e -> dispatch Go, className "btn btn-primary"] [text "Enter"]
                            ]
                  ]

--divDropdownRight :: Render AppState props Action
divDropdownRight :: _ -> ReactElement
divDropdownRight d =
  ul [className "nav navbar-nav pull-right"]
     [
       -- TODO if logged in : enable dropdown to logout
       li [className "dropdown"]
       [
         a [ aria {hidden : true}
           , className "glyphicon glyphicon-log-in"
           , --href "#/login"
             onClick $ \e -> d ShowLogin
           , style {color:"white"}
           , title "Log in and save your time"
           -- TODO hover: bold
            ]
          -- TODO if logged in
          --, text " username"
          -- else
         [text " Login / Signup"]
        ]
     ]

layoutFooter ::  forall props eff. Spec (ajax :: AJAX, console :: CONSOLE, dom :: DOM |eff) AppState props Action
layoutFooter = simpleSpec performAction render
  where
    render :: Render AppState props Action
    render dispatch _ state _ = [div [ className "container1" ] [ hr [] [], footerLegalInfo']]
      where
        footerLegalInfo' = footer [] [ p [] [ text "Gargantext "
                                   , span [className "glyphicon glyphicon-registration-mark" ] []
                                   , text ", version 4.0"
                                   , a [ href "http://www.cnrs.fr"
                                       , target "blank"
                                       , title "Project hosted by CNRS."
                                       ]
                                         [ text ", Copyrights "
                                         , span [ className "glyphicon glyphicon-copyright-mark" ] []
                                         , text " CNRS 2017-Present"
                                         ]
                                   , a [ href "http://gitlab.iscpif.fr/humanities/gargantext/blob/stable/LICENSE"
                                       , target "blank"
                                       , title "Legal instructions of the project."
                                       ]
                                         [ text ", Licences aGPLV3 and CECILL variant Affero compliant" ]
                                         , text "."
                                   ]
                            ]

layoutSpec :: forall eff props. Spec (E eff) AppState props Action
layoutSpec =
  fold
  [ routingSpec
  , container $ withState pagesComponent
  , withState \st ->
     fold [ focus _loginState _loginAction (LN.modalSpec st.showLogin "Login" LN.renderSpec)
          , focus _addCorpusState _addCorpusAction (AC.modalSpec st.showCorpus "Search Results" AC.layoutAddcorpus)
          ]
  ]
  where
    container :: Spec (E eff) AppState props Action -> Spec (E eff) AppState props Action
    container = over _render \render d p s c ->
      (render d p s c)
