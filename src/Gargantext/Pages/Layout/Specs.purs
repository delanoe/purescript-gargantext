module Gargantext.Pages.Layout.Specs where

import Data.Foldable (fold, intercalate)
import Data.Lens (over)
import Data.Maybe (Maybe(Nothing, Just))
import Effect (Effect)
import React.DOM (button, div, text)
import React.DOM.Props (_id, className, onClick, role, style)
import Reactix as R
import Reactix.DOM.HTML as H
import Thermite (Spec, _render, defaultPerformAction, defaultRender, focus, simpleSpec, withState, noState, cmapProps)
-- import Unsafe.Coerce (unsafeCoerce)

import Gargantext.Prelude
import Gargantext.Components.Data.Lang (Lang(..))
import Gargantext.Components.GraphExplorer as GE
import Gargantext.Components.Login.Types (AuthData(..))
import Gargantext.Components.Login as LN
import Gargantext.Components.Tree  as Tree
import Gargantext.Folder           as F
import Gargantext.Pages.Annuaire   as A
import Gargantext.Pages.Annuaire.User.Contacts as C
import Gargantext.Pages.Corpus as Corpus
import Gargantext.Pages.Corpus.Document as Annotation
import Gargantext.Pages.Corpus.Dashboard as Dsh
import Gargantext.Pages.Lists as Lists
import Gargantext.Pages.Texts as Texts
import Gargantext.Pages.Home as L
import Gargantext.Pages.Layout.Actions (Action(..), _graphExplorerAction, _loginAction, performAction)
import Gargantext.Pages.Layout.Specs.SearchBar as SB
import Gargantext.Pages.Layout.States (AppState, _loginState, _graphExplorerState)
import Gargantext.Router (Routes(..))
import Gargantext.Utils.Reactix as R2

layoutSpec :: Spec AppState {} Action
layoutSpec =
  fold
  [ routingSpec
  , container $ withState pagesComponent
  , withState \st ->
     fold [ focus _loginState _loginAction (LN.modalSpec st.showLogin "Login" LN.renderSpec)
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
    selectSpec Home              = layout0 $ noState $ L.layoutLanding EN
    selectSpec Login             = focus _loginState _loginAction LN.renderSpec
    selectSpec (Folder i)        = layout0 $ noState F.layoutFolder

    selectSpec (Corpus   i)      = layout0 $ cmapProps (const {nodeId: i}) $ noState Corpus.layout
    selectSpec (CorpusDocument c l i) = layout0 $ cmapProps (const {nodeId: i, listId: l, corpusId: Just c}) $ noState Annotation.layout
    selectSpec (Document l i)    = layout0 $ cmapProps (const {nodeId: i, listId: l, corpusId: Nothing}) $ noState Annotation.layout
    selectSpec (PGraphExplorer i) = graphSpec
    selectSpec (Texts i)         = layout0 $ cmapProps (const {nodeId: i}) $ noState Texts.layout
    selectSpec (Lists i)         = layout0 $ cmapProps (const {nodeId: i}) $ noState Lists.layout
    selectSpec Dashboard         = layout0 $ noState Dsh.layoutDashboard
    selectSpec (Annuaire i)      = layout0 $ cmapProps (const {annuaireId: i}) $ noState A.layout
    selectSpec (UserPage i)      = layout0 $ cmapProps (const {nodeId: i}) $ noState C.layoutUser
    selectSpec (ContactPage i)   = layout0 $ cmapProps (const {nodeId: i}) $ noState C.layoutUser

    graphSpec = layout1 $ withState \st ->
          cmapProps (const {
              mCurrentRoute: st.currentRoute
            , treeId: case st.loginState.authData of
                Nothing -> Nothing
                Just (AuthData ad) -> Just ad.tree_id
          }) $ focus _graphExplorerState _graphExplorerAction $ GE.spec

    -- selectSpec _ = simpleSpec defaultPerformAction defaultRender

routingSpec :: Spec AppState {} Action
routingSpec = simpleSpec performAction defaultRender

layout0 :: Spec AppState {} Action
        -> Spec AppState {} Action
layout0 layout =
  fold
  [ searchBar
  , outerLayout
  , noState layoutFooter
  ]
  where
    outerLayout1 = simpleSpec defaultPerformAction defaultRender
    outerLayout :: Spec AppState {} Action
    outerLayout =
      cont $ fold
      [ -- over _render \render d p s c ->  [logLinks d s] ,
       withState \st ->
          case st.loginState.authData of
            Just (AuthData {tree_id}) ->
              ls $ cmapProps (const {root: tree_id, mCurrentRoute: st.currentRoute}) $ noState $ Tree.treeview
            Nothing ->
              outerLayout1
      , rs bs
      ]
    ls   = over _render \render d p s c -> [
         div [ className "col-md-2", style {paddingTop: "60px"} ] $ render d p s c
      ]
    rs   = over _render \render d p s c -> [
          div [ case (s.loginState.authData) of
          Just a ->
            className "col-md-10"
          Nothing ->
            className "col-md-12"
        ] (render d p s c) ]
    cont = over _render \render d p s c -> [ div [className "row"      ] (render d p s c) ]

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
  [ searchBar
  , layout
  -- , outerLayout
  , noState layoutFooter
  ]
  where
    outerLayout1 = simpleSpec defaultPerformAction defaultRender
    outerLayout :: Spec AppState {} Action
    outerLayout =
      cont $ fold
      [ withState \st ->
          case st.loginState.authData of
            Just (AuthData {tree_id}) ->
              ls $ cmapProps (const {root: tree_id, mCurrentRoute: st.currentRoute}) $ noState $ Tree.treeview
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

    bs = innerLayout $ layout

    innerLayout :: Spec AppState {} Action
                -> Spec AppState {} Action
    innerLayout = over _render \render d p s c ->
      [  div [_id "page-wrapper"]
        [
          div [className "container-fluid"]  (render d p s c)
        ]
      ]


searchBar :: Spec AppState {} Action
searchBar = simpleSpec defaultPerformAction render
  where
    render d p s c =
      [ div [ _id "dafixedtop"
            , className "navbar navbar-inverse navbar-fixed-top"
            , role "navigation"
            ] [ div [className "container-fluid"
                    ]
                    [ div [ className "navbar-inner" ]
                          [ R2.scuff divLogo
                          ,  div [ className "collapse navbar-collapse"
                                 ]
                             $  [ R2.scuff divDropdownLeft ]
                             <> [ R2.scuff (SB.searchBar SB.defaultProps) ]
                             <> [ R2.scuff $ divDropdownRight d s ]
                          ]
                    ]
              ]
      ]


divLogo :: R.Element
divLogo = H.a { className: "navbar-brand logoSmall"
              , href: "#/"
              } [ H.img { src: "images/logoSmall.png"
                        , title: "Back to home."
                        , width:  "30"
                        , height: "28"
                        }
                ]

divDropdownLeft :: R.Element
divDropdownLeft = divDropdownLeft' (LiNav { title : "About Gargantext"
                                          , href  : "#"
                                          , icon  : "glyphicon glyphicon-info-sign"
                                          , text  : "Info"
                                          }
                                    )

divDropdownLeft' :: LiNav -> R.Element
divDropdownLeft' mb =  H.ul {className: "nav navbar-nav"}
                       [ H.ul {className: "nav navbar-nav pull-left"}
                         [ H.li {className: "dropdown"}
                           [ menuButton mb
                           , menuElements'
                           ]
                         ]
                       ]

menuButton :: LiNav -> R.Element
menuButton (LiNav { title : title'
                  , href : href'
                  , icon : icon'
                  , text : text'
                  }) = H.a { className: "dropdown-toggle navbar-text"
                           , data: {toggle: "dropdown"}
                           , href: href'
                           , role: "button"
                           , title: title'
                           } [ H.span { aria: {hidden : true}
                                      , className: icon'
                                      } []
                             , H.text (" " <> text')
                             ]

menuElements' :: R.Element
menuElements' = menuElements-- title, icon, text
  [ -- ===========================================================
    [ LiNav { title : "Quick start, tutorials and methodology"
            , href  : "https://iscpif.fr/gargantext/your-first-map/"
            , icon  : "glyphicon glyphicon-book"
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
            , icon  : "glyphicon glyphicon-question-sign"
            , text  : "About"
            }
    ]
  ] -- ===========================================================

-- | Menu in the sidebar, syntactic sugar
menuElements :: Array (Array LiNav) -> R.Element
menuElements ns = dropDown $ intercalate divider $ map (map liNav) ns
  where
    dropDown :: Array R.Element -> R.Element
    dropDown = H.ul {className: "dropdown-menu"}

    divider :: Array R.Element
    divider = [H.li {className: "divider"} []]

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

liNav :: LiNav -> R.Element
liNav (LiNav { title : title'
             , href  : href'
             , icon  : icon'
             , text  : text'
             }
      ) = H.li {} [ H.a { tabIndex: (-1)
                        , target: "blank"
                        , title: title'
                        , href: href'
                        } [ H.span { className: icon' } []
                          , H.text $ " " <> text'
                          ]
                  ]


logLinks :: (Action -> Effect Unit) -> AppState -> R.Element
logLinks d s = case s.loginState.authData of
           Nothing -> loginLink
           Just _  -> logoutLink
  where
    loginLink =
      H.a { aria: {hidden : true}
          , className: "glyphicon glyphicon-log-in"
          , on: {click:  \e -> d ShowLogin}
          , style: {color:"white"}
          , title: "Log in and save your time"
          -- TODO hover: bold
          }
          [H.text " Login / Signup"]
    -- TODO dropdown to logout
    logoutLink =
      H.a { aria: {hidden : true}
          , className: "glyphicon glyphicon-log-out"
          , on: {click: \e -> d Logout}
          , style: {color:"white"}
          , title: "Log out" -- TODO
          -- TODO hover: bold
          }
          [H.text " Logout"]


divDropdownRight :: (Action -> Effect Unit) -> AppState -> R.Element
divDropdownRight d s =
  H.ul {className: "nav navbar-nav pull-right"}
  [ H.li {className: "dropdown"}
    [ logLinks d s ]
  ]

layoutFooter :: Spec {} {} Void
layoutFooter = R2.elSpec $ R.hooksComponent "LayoutFooter" cpt
  where
    cpt {} _children = do
      pure $ H.div { className: "container" } [ H.hr {}, footerLegalInfo']

    footerLegalInfo' = H.footer {}
                        [ H.p {} [ H.text "Gargantext "
                                , H.span {className: "glyphicon glyphicon-registration-mark"} []
                                , H.text ", version 4.0"
                                , H.a { href: "http://www.cnrs.fr"
                                      , target: "blank"
                                      , title: "Project hosted by CNRS."
                                      }
                                  [ H.text ", Copyrights "
                                  , H.span { className: "glyphicon glyphicon-copyright-mark" } []
                                  , H.text " CNRS 2017-Present"
                                  ]
                                , H.a { href: "http://gitlab.iscpif.fr/humanities/gargantext/blob/stable/LICENSE"
                                      , target: "blank"
                                      , title: "Legal instructions of the project."
                                      }
                                  [ H.text ", Licences aGPLV3 and CECILL variant Affero compliant" ]
                                , H.text "."
                                ]
                        ]
