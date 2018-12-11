module Gargantext.Pages.Layout.Specs where

import Data.Foldable (fold, intercalate)
import Data.Lens (over)
import Data.Maybe (Maybe(Nothing, Just))
import Effect (Effect)
import React (ReactElement)
import React.DOM (a, button, div, footer, hr', img, input, li, p, span, text, ul)
import React.DOM.Props (_data, _id, _type, aria, className, href, onChange, onClick, placeholder, role, src, style, tabIndex, target, title)
import Thermite (Render, Spec, _render, defaultPerformAction, defaultRender, focus, simpleSpec, withState, noState, cmapProps)
import Unsafe.Coerce (unsafeCoerce)

import Gargantext.Prelude
import Gargantext.Components.Data.Lang (Lang(..))
import Gargantext.Components.Login.Types (AuthData(..))
import Gargantext.Components.Login as LN
import Gargantext.Components.Tree  as Tree
import Gargantext.Folder           as F
import Gargantext.Pages.Annuaire   as A
import Gargantext.Pages.Annuaire.User.Contacts as C
import Gargantext.Pages.Corpus     as Corpus
import Gargantext.Pages.Corpus.Document as Annotation
import Gargantext.Pages.Corpus.Dashboard as Dsh
import Gargantext.Pages.Corpus.Graph as GE
import Gargantext.Pages.Home as L
import Gargantext.Pages.Layout.Actions (Action(..), _addCorpusAction, _documentViewAction, _graphExplorerAction, _loginAction, _searchAction, performAction)
import Gargantext.Pages.Layout.Specs.AddCorpus as AC
import Gargantext.Pages.Layout.Specs.Search    as S
import Gargantext.Pages.Layout.States (AppState, _addCorpusState, _documentViewState, _graphExplorerState, _loginState, _searchState)
import Gargantext.Router (Routes(..))

layoutSpec :: Spec AppState {} Action
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
    -- NP: what is it for ?
    container :: Spec AppState {} Action -> Spec AppState {} Action
    container = over _render \render d p s c ->
      (render d p s c)

pagesComponent :: AppState -> Spec AppState {} Action
pagesComponent s = case s.currentRoute of
    Just route -> selectSpec route
    Nothing    -> selectSpec Home -- TODO add Error page here: url requested does not exist (with funny Garg image)
  where
    selectSpec :: Routes -> Spec AppState {} Action
    selectSpec Home              = layout0 $ noState (L.layoutLanding EN)
    selectSpec Login             = focus _loginState _loginAction LN.renderSpec
    selectSpec (Folder i)        = layout0 $ noState F.layoutFolder

    selectSpec (Corpus   i)      = layout0 $ cmapProps (const {nodeId: i}) $ noState Corpus.layout
    selectSpec AddCorpus         = layout0 $ focus _addCorpusState _addCorpusAction AC.layoutAddcorpus
    selectSpec SearchView        = layout0 $ focus _searchState _searchAction  S.searchSpec
    selectSpec (Document i)      = layout0 $ focus _documentViewState _documentViewAction  Annotation.docview
    selectSpec (PGraphExplorer i)= layout1  $ focus _graphExplorerState _graphExplorerAction  GE.specOld
    selectSpec Dashboard         = layout0 $ noState Dsh.layoutDashboard

    selectSpec (Annuaire i)      = layout0 $ cmapProps (const {annuaireId: i}) $ noState A.layout
    selectSpec (UserPage i)      = layout0 $ cmapProps (const {nodeId: i}) $ noState C.layoutUser
    selectSpec (ContactPage i)   = layout0 $ cmapProps (const {nodeId: i}) $ noState C.layoutUser

    -- selectSpec _ = simpleSpec defaultPerformAction defaultRender

routingSpec :: Spec AppState {} Action
routingSpec = simpleSpec performAction defaultRender

layout0 :: Spec AppState {} Action
        -> Spec AppState {} Action
layout0 layout =
  fold
  [ layoutSidebar divSearchBar
  , outerLayout
  , layoutFooter
  ]
  where
    outerLayout1 = simpleSpec defaultPerformAction defaultRender
    outerLayout :: Spec AppState {} Action
    outerLayout =
      cont $ fold
      [ withState \st ->
          case st.loginState.authData of
            Just (AuthData {tree_id}) ->
              ls $ cmapProps (const {root: tree_id}) as
            Nothing ->
              outerLayout1
      , rs bs
      ]
    ls   = over _render \render d p s c -> [
         div [ className "col-md-2"] (render d p s c)
      ]
    rs   = over _render \render d p s c -> [ div [ className "col-md-10"] (render d p s c) ]
    cont = over _render \render d p s c -> [ div [className "row"      ] (render d p s c) ]

    as = noState Tree.treeview

    bs = innerLayout $ layout

    innerLayout :: Spec AppState {} Action
                -> Spec AppState {} Action
    innerLayout = over _render \render d p s c ->
      [  div [_id "page-wrapper"]
        [
          div [className "container-fluid"]  (render d p s c)
        ]
      ]

-- TODO avoid code duplication with layout0
layout1 :: Spec AppState {} Action
        -> Spec AppState {} Action
layout1 layout =
  fold
  [ layoutSidebar divSearchBar
  , layout
  -- , outerLayout
  , layoutFooter
  ]
  where
    outerLayout1 = simpleSpec defaultPerformAction defaultRender
    outerLayout :: Spec AppState {} Action
    outerLayout =
      cont $ fold
      [ withState \st ->
          case st.loginState.authData of
            Just (AuthData {tree_id}) ->
              ls $ cmapProps (const {root: tree_id}) as
            Nothing ->
              outerLayout1
      , rs bs
      ]
    ls   = over _render \render d p s c -> [

        button [onClick $ \e -> d ToggleTree, className "btn btn-primary",style {position:"relative", top: "99px",left:"-264px",zIndex : "1000"}] [text "ShowTree"]

        , div [if (s.showTree) then className "col-md-2" else className "col-md-2"] if (s.showTree) then (render d p s c) else []
      ]
    rs   = over _render \render d p s c -> [ div [if (s.showTree) then className "col-md-10" else className "col-md-12"] (render d p s c) ]
    cont = over _render \render d p s c -> [ div [className "row"      ] (render d p s c) ]

    as = noState Tree.treeview

    bs = innerLayout $ layout

    innerLayout :: Spec AppState {} Action
                -> Spec AppState {} Action
    innerLayout = over _render \render d p s c ->
      [  div [_id "page-wrapper"]
        [
          div [className "container-fluid"]  (render d p s c)
        ]
      ]


layoutSidebar :: Spec AppState {} Action
              -> Spec AppState {} Action
layoutSidebar = over _render \render d p s c ->
      [ div [ _id "dafixedtop"
            , className "navbar navbar-inverse navbar-fixed-top"
            , role "navigation"
            ] [ div [className "container-fluid"]
                    [ div [ className "navbar-inner" ]
                          [ divLogo
                          ,  div [ className "collapse navbar-collapse"]
                             $ [ divDropdownLeft]
                             <> render d p s c <>
                             [ divDropdownRight d s ]
                          ]
                    ]
              ]
      ]


divLogo :: ReactElement
divLogo = a [ className "navbar-brand logoSmall"
            , href "#/"
            ] [ img [ src "images/logoSmall.png"
                    , title "Back to home."
                    ]
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
divSearchBar :: Spec AppState {} Action
divSearchBar = simpleSpec performAction render
  where
    render :: Render AppState {} Action
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
                                    ]
                            ,  button [onClick \e -> dispatch Go, className "btn btn-primary"] [text "Enter"]
                            ]
                  ]

divDropdownRight :: (Action -> Effect Unit) -> AppState -> ReactElement
divDropdownRight d s =
  ul [className "nav navbar-nav pull-right"]
     [ li [className "dropdown"]
       [ case s.loginState.authData of
           Nothing -> loginLink
           Just _  -> logoutLink
       ]
     ]
  where
    loginLink =
      a [ aria {hidden : true}
        , className "glyphicon glyphicon-log-in"
        , onClick $ \e -> d ShowLogin
        , style {color:"white"}
        , title "Log in and save your time"
        -- TODO hover: bold
        ]
        [text " Login / Signup"]
    -- TODO dropdown to logout
    logoutLink =
      a [ aria {hidden : true}
        , className "glyphicon glyphicon-log-out"
        , onClick $ \e -> d Logout
        , style {color:"white"}
        , title "Log out" -- TODO
        -- TODO hover: bold
        ]
        [text " Logout"]

layoutFooter :: Spec AppState {} Action
layoutFooter = simpleSpec performAction render
  where
    render :: Render AppState {} Action
    render dispatch _ state _ = [div [ className "container1" ] [ hr', footerLegalInfo']]
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
