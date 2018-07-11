module Gargantext.Navigation where

import Prelude hiding (div)

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Gargantext.Pages.Corpus.Doc.Body as CA
import DOM (DOM)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Foldable (fold, intercalate)
import Data.Lens (Lens', Prism', lens, over, prism)
import Data.Maybe (Maybe(Nothing, Just))
import Gargantext.Pages.Corpus.Doc.Annotation as D
import Gargantext.Pages.Corpus.Doc.Document as DV
import Gargantext.Components.Data.Lang (Lang(..))
import Gargantext.Components.Login as LN
import Gargantext.Components.Modals.Modal (modalShow)
import Gargantext.Components.Tree as Tree
import Gargantext.Pages.Corpus.Doc.Facets.Dashboard as Dsh
import Gargantext.Pages.Corpus as AC
import Gargantext.Router (Routes(..))
import Gargantext.Users as U
import Gargantext.Pages.Corpus.Doc.Facets.Graph as GE
import Gargantext.Pages.Home as L
import Network.HTTP.Affjax (AJAX)
import Gargantext.Pages.Corpus.Doc.Facets.Terms.NgramsTable as NG
import React (ReactElement)
import React.DOM (a, button, div, footer, hr, img, input, li, p, span, text, ul)
import React.DOM.Props (_data, _id, _type, aria, className, href, onChange, onClick, placeholder, role, src, style, tabIndex, target, title)
import Gargantext.Pages.Search as S
import Gargantext.Pages.Corpus.Doc.Facets as TV
import Thermite (PerformAction, Render, Spec, _render, defaultPerformAction, defaultRender, focus, modifyState, simpleSpec, withState)
import Unsafe.Coerce (unsafeCoerce)

type E e = (dom :: DOM, ajax :: AJAX, console :: CONSOLE | e)

type AppState =
  { currentRoute   :: Maybe Routes
  , landingState   :: L.State
  , loginState     :: LN.State
  , addCorpusState :: AC.State
  , docViewState   :: DV.State
  , searchState    :: S.State
  , userPage       :: U.State
  , docAnnotationView   :: D.State
  , ntreeView   :: Tree.State
  , tabview :: TV.State
  , search :: String
  , corpusAnalysis :: CA.State
  , showLogin :: Boolean
  , showCorpus :: Boolean
  , graphExplorer :: GE.State
  , initialized :: Boolean
  , ngState :: NG.State
  , dashboard :: Dsh.State
  }

initAppState :: AppState
initAppState =
  { currentRoute   : Just Home
  , landingState   : L.initialState
  , loginState     : LN.initialState
  , addCorpusState : AC.initialState
  , docViewState   : DV.tdata
  , searchState    : S.initialState
  , userPage       : U.initialState
  , docAnnotationView   : D.initialState
  , ntreeView : Tree.exampleTree
  , tabview : TV.initialState
  , search : ""
  , corpusAnalysis : CA.initialState
  , showLogin : false
  , showCorpus : false
  , graphExplorer : GE.initialState
  , initialized : false
  , ngState : NG.initialState
  , dashboard : Dsh.initialState
  }

data Action
  = Initialize
  | LandingA   L.Action
  | LoginA     LN.Action
  | SetRoute   Routes
  | AddCorpusA AC.Action
  | DocViewA   DV.Action
  | SearchA    S.Action
  | UserPageA  U.Action
  | DocAnnotationViewA  D.Action
  | TreeViewA  Tree.Action
  | TabViewA TV.Action
  | GraphExplorerA GE.Action
  | DashboardA Dsh.Action
  | Search String
  | Go
  | CorpusAnalysisA CA.Action
  | ShowLogin
  | ShowAddcorpus
  | NgramsA  NG.Action


performAction :: forall eff props. PerformAction ( dom :: DOM
                                                 , ajax :: AJAX
                                                 , console :: CONSOLE
                                                 | eff
                                                 ) AppState props Action
performAction (SetRoute route)  _ _ = void do
  modifyState $ _ {currentRoute = pure route}
performAction (Search s)  _ _ = void do
  modifyState $ _ {search = s}

performAction (ShowLogin)  _ _ = void do
  liftEff $ modalShow "loginModal"
  modifyState $ _ {showLogin = true}

performAction (ShowAddcorpus)  _ _ = void do
  liftEff $ modalShow "addCorpus"
  modifyState $ _ {showCorpus = true}

performAction Go  _ _ = void do
  liftEff $ modalShow "addCorpus"
  modifyState $ _ {showCorpus = true}
 -- _ <- lift $ setHash "/addCorpus"
  --modifyState id

performAction Initialize  _ state = void do
  _ <- liftEff $ log "loading Initial nodes"
  case state.initialized of
    false -> do

      lnodes <- lift $ Tree.loadDefaultNode

      case lnodes of
        Left err -> do
          modifyState id
        Right d -> do
          page <- lift $ DV.loadPage
          case page of
            Left err -> do
              modifyState id
            Right docs -> do
              modifyState $ _ { initialized = true
                              , ntreeView = if length d > 0
                                            then Tree.exampleTree
                                           --then fnTransform $ unsafePartial $ fromJust $ head d
                                           else Tree.initialState

                              , docViewState = docs
                              }
    _ -> do
      modifyState id

performAction _ _ _ = void do
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

_userPageState :: Lens' AppState U.State
_userPageState = lens (\s -> s.userPage) (\s ss -> s{userPage = ss})

_userPageAction :: Prism' Action U.Action
_userPageAction = prism UserPageA \action ->
  case action of
    UserPageA caction -> Right caction
    _-> Left action

_dashBoardAction :: Prism' Action Dsh.Action
_dashBoardAction = prism DashboardA \action ->
  case action of
    DashboardA caction -> Right caction
    _ -> Left action

_docAnnotationViewState :: Lens' AppState D.State
_docAnnotationViewState = lens (\s -> s.docAnnotationView) (\s ss -> s{docAnnotationView = ss})

_docAnnotationViewAction :: Prism' Action D.Action
_docAnnotationViewAction = prism DocAnnotationViewA \action ->
  case action of
    DocAnnotationViewA caction -> Right caction
    _-> Left action

_treeState :: Lens' AppState Tree.State
_treeState = lens (\s -> s.ntreeView) (\s ss -> s {ntreeView = ss})

_treeAction :: Prism' Action Tree.Action
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

_corpusState :: Lens' AppState CA.State
_corpusState = lens (\s -> s.corpusAnalysis) (\s ss -> s {corpusAnalysis = ss})

_corpusAction :: Prism' Action CA.Action
_corpusAction = prism CorpusAnalysisA \action ->
  case action of
    CorpusAnalysisA caction -> Right caction
    _-> Left action

_dashBoardSate :: Lens' AppState Dsh.State
_dashBoardSate = lens (\s -> s.dashboard) (\s ss -> s {dashboard = ss})

_graphExplorerState :: Lens' AppState GE.State
_graphExplorerState = lens (\s -> s.graphExplorer) (\s ss -> s{graphExplorer = ss})

_graphExplorerAction :: Prism' Action GE.Action
_graphExplorerAction = prism GraphExplorerA \action ->
  case action of
    GraphExplorerA caction -> Right caction
    _-> Left action

_ngState :: Lens' AppState NG.State
_ngState = lens (\s -> s.ngState) (\s ss -> s{ngState = ss})

_ngAction :: Prism' Action NG.Action
_ngAction = prism NgramsA \action ->
  case action of
    NgramsA caction -> Right caction
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
    selectSpec CorpusAnalysis = layout0 $ focus _corpusState  _corpusAction CA.spec'
    selectSpec Login      = focus _loginState _loginAction LN.renderSpec
    selectSpec Home        = layout0 $ focus _landingState   _landingAction   (L.layoutLanding EN)
    selectSpec AddCorpus  = layout0 $ focus _addCorpusState _addCorpusAction AC.layoutAddcorpus
    selectSpec DocView    = layout0 $ focus _docViewState   _docViewAction   DV.layoutDocview
    selectSpec (UserPage i) = layout0 $ focus _userPageState  _userPageAction  U.layoutUser
    selectSpec (DocAnnotation i)   = layout0 $ focus _docAnnotationViewState  _docAnnotationViewAction  D.docview
    selectSpec Tabview   = layout0 $ focus _tabviewState  _tabviewAction  TV.tab1
    -- To be removed
    selectSpec SearchView = layout0 $ focus _searchState _searchAction  S.searchSpec
    selectSpec NGramsTable  = layout0 $ focus _ngState _ngAction  NG.ngramsTableSpec
    selectSpec PGraphExplorer = focus _graphExplorerState _graphExplorerAction  GE.specOld
    selectSpec Dashboard = layout0 $ focus _dashBoardSate _dashBoardAction Dsh.layoutDashboard

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
    ls = over _render \render d p s c ->
      [div [className "col-md-2"] (render d p s c)]
    rs = over _render \render d p s c ->
      [ div [className "col-md-10"] (render d p s c) ]
    cont = over _render \render d p s c ->
      [ div [ className "row" ] (render d p s c) ]

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

dispatchAction :: forall t115 t445 t447.
                  Bind t445 => Applicative t445  =>
                  (Action -> t445 t447) -> t115 -> Routes -> t445 Unit

dispatchAction dispatcher _ Home = do
  _ <- dispatcher Initialize
  _ <- dispatcher $ SetRoute Home
  _ <- dispatcher $ LandingA L.NoOp
  pure unit

dispatchAction dispatcher _ Login = do
  _ <- dispatcher Initialize
  _ <- dispatcher $ SetRoute Login
  _ <- dispatcher $ LoginA LN.NoOp
  pure unit

dispatchAction dispatcher _ AddCorpus = do
  _ <- dispatcher $ SetRoute AddCorpus
  _ <- dispatcher $ AddCorpusA AC.LoadDatabaseDetails
  pure unit

dispatchAction dispatcher _ DocView = do
  _ <- dispatcher $ SetRoute $ DocView
  _ <- dispatcher $ DocViewA $ DV.LoadData
  pure unit

dispatchAction dispatcher _ SearchView = do
  _ <- dispatcher $ SetRoute $ SearchView
  _ <- dispatcher $ SearchA  $ S.NoOp
  pure unit

dispatchAction dispatcher _ (UserPage id) = do
  _ <- dispatcher $ SetRoute  $ UserPage id
  _ <- dispatcher $ UserPageA $ U.NoOp
  _ <- dispatcher $ UserPageA $ U.FetchUser id
  pure unit

dispatchAction dispatcher _ (DocAnnotation i) = do
  _ <- dispatcher $ SetRoute  $ DocAnnotation i
  _ <- dispatcher $ DocAnnotationViewA $ D.NoOp
  pure unit

dispatchAction dispatcher _ Tabview = do
  _ <- dispatcher $ SetRoute  $ Tabview
  _ <- dispatcher $ TabViewA $ TV.NoOp
  pure unit

dispatchAction dispatcher _ CorpusAnalysis = do
  _ <- dispatcher $ SetRoute  $ CorpusAnalysis
  --_ <- dispatcher $ CorpusAnalysisA $ CA.NoOp
  pure unit

dispatchAction dispatcher _ PGraphExplorer = do
  _ <- dispatcher $ SetRoute  $ PGraphExplorer
  _ <- dispatcher $ GraphExplorerA $ GE.LoadGraph "imtNew.json"
  pure unit

dispatchAction dispatcher _ NGramsTable = do
  _ <- dispatcher $ SetRoute  $ NGramsTable
  _ <- dispatcher $ NgramsA $ NG.NoOp
  pure unit

dispatchAction dispatcher _ Dashboard = do
  _ <- dispatcher $ SetRoute $ Dashboard
  pure unit
