module Navigation where

import DOM
import Gargantext.Data.Lang
import Prelude hiding (div)

import AddCorpusview as AC
import AnnotationDocumentView as D
import Control.Monad.Eff.Console (CONSOLE)
import Data.Array (concat)
import Data.Either (Either(..))
import Data.Foldable (fold, intercalate)
import Data.Lens (Lens', Prism', lens, over, prism)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(..))
import DocView as DV
import Landing as L
import Login as LN
import NTree as NT
import Network.HTTP.Affjax (AJAX)
import PageRouter (Routes(..))
import React (ReactElement)
import React.DOM (a, button, div, footer, form, hr, i, img, input, li, p, span, text, ul)
import React.DOM.Props (Props, _data, _id, _type, aria, className, href, name, onClick, placeholder, role, src, style, tabIndex, target, title)
import React.DOM.Props as RP
import SearchForm as S
import Tabview as TV
import Thermite (PerformAction, Render, Spec, _render, cotransform, defaultRender, focus, modifyState, simpleSpec, withState)
import UserPage as UP


type E e = (dom :: DOM, ajax :: AJAX, console :: CONSOLE | e)

type AppState =
  { currentRoute   :: Maybe Routes
  , landingState   :: L.State
  , loginState     :: LN.State
  , addCorpusState :: AC.State
  , docViewState   :: DV.State
  , searchState    :: S.State
  , userPage       :: UP.State
  , annotationdocumentView   :: D.State
  , ntreeView   :: NT.State
  , tabview :: TV.State
  }

initAppState :: AppState
initAppState =
  { currentRoute   : Just Home
  , landingState   : L.initialState
  , loginState     : LN.initialState
  , addCorpusState : AC.initialState
  , docViewState   : DV.tdata
  , searchState    : S.initialState
  , userPage       : UP.initialState
  , annotationdocumentView   : D.initialState
  , ntreeView : NT.exampleTree
  , tabview : TV.initialState
  }

data Action
  = Initialize
  | LandingA   L.Action
  | LoginA     LN.Action
  | SetRoute   Routes
  | AddCorpusA AC.Action
  | DocViewA   DV.Action
  | SearchA    S.Action
  | UserPageA  UP.Action
  | AnnotationDocumentViewA  D.Action
  | TreeViewA  NT.Action
  | TabViewA TV.Action


performAction :: forall eff props. PerformAction ( dom :: DOM
                                                 | eff
                                                 ) AppState props Action
performAction (SetRoute route)  _ _ = void do
  modifyState $ _ {currentRoute = pure route}


performAction _  _ _ = void do
  modifyState id



---- Lens and Prism
_landingState :: Lens' AppState L.State
_landingState = lens (\s -> s.landingState) (\s ss -> s{landingState = ss})


_landingAction :: Prism' Action L.Action
_landingAction = prism LandingA \action ->
  case action of
    LandingA caction -> Right caction
    _-> Left action


_loginState :: Lens' AppState LN.State
_loginState = lens (\s -> s.loginState) (\s ss -> s{loginState = ss})


_loginAction :: Prism' Action LN.Action
_loginAction = prism LoginA \action ->
  case action of
    LoginA caction -> Right caction
    _-> Left action


_addCorpusState :: Lens' AppState AC.State
_addCorpusState = lens (\s -> s.addCorpusState) (\s ss -> s{addCorpusState = ss})


_addCorpusAction :: Prism' Action AC.Action
_addCorpusAction = prism AddCorpusA \action ->
  case action of
    AddCorpusA caction -> Right caction
    _-> Left action


_docViewState :: Lens' AppState DV.State
_docViewState = lens (\s -> s.docViewState) (\s ss -> s{docViewState = ss})


_docViewAction :: Prism' Action DV.Action
_docViewAction = prism DocViewA \action ->
  case action of
    DocViewA caction -> Right caction
    _-> Left action


_searchState :: Lens' AppState S.State
_searchState = lens (\s -> s.searchState) (\s ss -> s{searchState = ss})


_searchAction :: Prism' Action S.Action
_searchAction = prism SearchA \action ->
  case action of
    SearchA caction -> Right caction
    _-> Left action


_userPageState :: Lens' AppState UP.State
_userPageState = lens (\s -> s.userPage) (\s ss -> s{userPage = ss})


_userPageAction :: Prism' Action UP.Action
_userPageAction = prism UserPageA \action ->
  case action of
    UserPageA caction -> Right caction
    _-> Left action


_annotationdocumentviewState :: Lens' AppState D.State
_annotationdocumentviewState = lens (\s -> s.annotationdocumentView) (\s ss -> s{annotationdocumentView = ss})


_annotationdocumentviewAction :: Prism' Action D.Action
_annotationdocumentviewAction = prism AnnotationDocumentViewA \action ->
  case action of
    AnnotationDocumentViewA caction -> Right caction
    _-> Left action


_treeState :: Lens' AppState NT.State
_treeState = lens (\s -> s.ntreeView) (\s ss -> s {ntreeView = ss})


_treeAction :: Prism' Action NT.Action
_treeAction = prism TreeViewA \action ->
  case action of
    TreeViewA caction -> Right caction
    _-> Left action


_tabviewState :: Lens' AppState TV.State
_tabviewState = lens (\s -> s.tabview) (\s ss -> s {tabview = ss})


_tabviewAction :: Prism' Action TV.Action
_tabviewAction = prism TabViewA \action ->
  case action of
    TabViewA caction -> Right caction
    _-> Left action


pagesComponent :: forall props eff. AppState -> Spec (E eff) AppState props Action
pagesComponent s =
  case s.currentRoute of
    Just route ->
      selectSpec route
    Nothing ->
      selectSpec Home
  where
    selectSpec :: Routes -> Spec ( ajax    :: AJAX
                                 , console :: CONSOLE
                                 , dom     :: DOM
                                 | eff
                                 ) AppState props Action
    selectSpec Login      = focus _loginState _loginAction LN.renderSpec
    selectSpec Home        = layout0 $ focus _landingState   _landingAction   (L.layoutLanding EN)
    selectSpec AddCorpus  = layout0 $ focus _addCorpusState _addCorpusAction AC.layoutAddcorpus
    selectSpec DocView    = layout0 $ focus _docViewState   _docViewAction   DV.layoutDocview
    selectSpec UserPage   = layout0 $ focus _userPageState  _userPageAction  UP.layoutUser
    selectSpec (AnnotationDocumentView i)   = layout0 $ focus _annotationdocumentviewState  _annotationdocumentviewAction  D.docview
    selectSpec Tabview   = layout0 $ focus _tabviewState  _tabviewAction  TV.tabSpec

    -- To be removed
    selectSpec SearchView = layout0 $ focus _searchState    _searchAction    S.searchSpec

routingSpec :: forall props eff. Spec (dom :: DOM |eff) AppState props Action
routingSpec = simpleSpec performAction defaultRender



layout0 :: forall eff props. Spec (E eff) AppState props Action
                          -> Spec (E eff) AppState props Action
layout0 layout =
  fold
  [ layoutSidebar
  , outerLayout
  , layoutFooter
  ]
  where
    outerLayout :: Spec (E eff) AppState props Action
    outerLayout =
      cont $ fold
      [ ls a
      , rs b
      ]
    ls = over _render \render d p s c ->
      [div [className "col-md-3"] (render d p s c)]
    rs = over _render \render d p s c ->
      [ div [className "col-md-8"] (render d p s c) ]
    cont = over _render \render d p s c ->
      [ div [ className "row" ] (render d p s c) ]

    a = fold  [ focus _treeState _treeAction NT.treeview
              , divSearchBar
              ]
    b = innerLayout $ layout

    innerLayout :: Spec (E eff) AppState props Action
                -> Spec (E eff) AppState props Action
    innerLayout = over _render \render d p s c ->
      [  div [_id "page-wrapper"]
        [
          div [className "container-fluid"]  (render d p s c)
        ]
      ]


layoutSidebar ::  forall props eff. Spec (dom :: DOM |eff) AppState props Action
layoutSidebar = simpleSpec performAction render
  where
    render :: Render AppState props Action
    render dispatch _ state _ =
      [ div [ _id "dafixedtop"
            , className "navbar navbar-inverse navbar-fixed-top"
            , role "navigation"
            ] [ div [className "container"]
                    [ div [ className "navbar-inner" ]
                          [ divLogo
                          ,  div [ className "collapse navbar-collapse"]
                                 [ divDropdownLeft
                                 , divDropdownRight
                                 ]
                          ]
                    ]
              ]
      ]



divLogo :: ReactElement
divLogo = a [ className "navbar-brand logoSmall"
            , href "/index.html"
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
divSearchBar :: forall props eff. Spec (dom :: DOM |eff) AppState props Action
divSearchBar = simpleSpec performAction render
  where
    render :: Render AppState props Action
    render dispatch _ state _ = [div [ className "" ] [ searchbar']]
      where
        searchbar' = ul [ className "nav navbar-nav"
                        , style { "margin-left" : "0px"}
                        ] [ div [className "navbar-form"]
                            [ input [ className   "search-query"
                                    , placeholder "Query, URL or FILE (works with Firefox or Chromium browsers)"
                                    , _type "text"
                                    , style { height: "35px"

                                              --  , color: "white"
                                              --  , background : "#A1C2D8"
                                            }
                                    ] []
                              -- TODO add button in navbar (and "enter" execution)
                              -- , div [] [button [][]]
                            ]
                          ]




--divDropdownRight :: Render AppState props Action
divDropdownRight :: ReactElement
divDropdownRight =
  ul [className "nav navbar-nav pull-right"]
     [
       -- TODO if logged in : enable dropdown to logout
       li [className "dropdown"]
       [
         a [ aria {hidden : true}
           , className "glyphicon glyphicon-log-in"
           , href "#/login"
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



layoutFooter ::  forall props eff. Spec (dom :: DOM |eff) AppState props Action
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
  ]
  where
    container :: Spec (E eff) AppState props Action -> Spec (E eff) AppState props Action
    container = over _render \render d p s c ->
      (render d p s c)

dispatchAction :: forall t115 t445 t447.
                  Bind t445 => Applicative t445  =>
                  (Action -> t445 t447) -> t115 -> Routes -> t445 Unit
dispatchAction dispatcher _ Home = do
  _ <- dispatcher $ SetRoute $ Home
  _ <- dispatcher $ LandingA $ L.NoOp
  pure unit

dispatchAction dispatcher _ Login = do
  _ <- dispatcher $ SetRoute $ Login
  _ <- dispatcher $ LoginA   $ LN.NoOp
  pure unit

dispatchAction dispatcher _ AddCorpus = do
  _ <- dispatcher $ SetRoute   $ AddCorpus
  _ <- dispatcher $ AddCorpusA $ AC.LoadDatabaseDetails
  pure unit

dispatchAction dispatcher _ DocView = do
  _ <- dispatcher $ SetRoute $ DocView
  _ <- dispatcher $ DocViewA $ DV.LoadData
  pure unit

dispatchAction dispatcher _ SearchView = do
  _ <- dispatcher $ SetRoute $ SearchView
  _ <- dispatcher $ SearchA  $ S.NoOp
  pure unit

dispatchAction dispatcher _ UserPage = do
  _ <- dispatcher $ SetRoute  $ UserPage
  _ <- dispatcher $ UserPageA $ UP.NoOp
  pure unit

dispatchAction dispatcher _ (AnnotationDocumentView i) = do
  _ <- dispatcher $ SetRoute  $ AnnotationDocumentView i
  _ <- dispatcher $ AnnotationDocumentViewA $ D.NoOp
  pure unit


dispatchAction dispatcher _ Tabview = do
  _ <- dispatcher $ SetRoute  $ Tabview
  _ <- dispatcher $ TabViewA $ TV.NoOp
  pure unit
