module Navigation where

import DOM

import AddCorpusview as AC
import Control.Monad.Eff.Console (CONSOLE)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Lens (Lens', Prism', lens, over, prism)
import Data.Maybe (Maybe(Nothing, Just))
import Landing as L
import Login as LN
import Network.HTTP.Affjax (AJAX)
import PageRouter (Routes(..))
import Prelude (class Applicative, class Bind, Unit, bind, id, map, negate, pure, unit, void, ($), (<>))
import React (ReactElement)
import React.DOM (a, div, img, li, span, text, ul, input, button, footer, p, hr)
import React.DOM.Props (_data, _id, aria, className, href, name, placeholder, _type, role, src, style, tabIndex, target, title)
import Thermite (PerformAction, Render, Spec, _render, defaultRender, focus, modifyState, simpleSpec, withState)
import DocView as DV
import SearchForm as S
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


performAction :: forall eff props. PerformAction ( dom :: DOM
                                                 | eff
                                                 ) AppState props Action
performAction (SetRoute route)  _ _ = void do
  modifyState $ _ {currentRoute = pure route}

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


_userPageState :: Lens' AppState UP.State
_userPageState = lens (\s -> s.userPage) (\s ss -> s{userPage = ss})


_userPageAction :: Prism' Action UP.Action
_userPageAction = prism UserPageA \action ->
  case action of
    UserPageA caction -> Right caction
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
    selectSpec Home       = wrap $ focus _landingState   _landingAction   L.home
    selectSpec AddCorpus  = wrap $ focus _addCorpusState _addCorpusAction AC.addcorpusviewSpec
    selectSpec DocView    = wrap $ focus _docViewState   _docViewAction   DV.spec
    selectSpec SearchView = wrap $ focus _searchState    _searchAction    S.searchSpec
    selectSpec UserPage   = wrap $ focus _userPageState  _userPageAction  UP.userPageSpec

routingSpec :: forall props eff. Spec (dom :: DOM |eff) AppState props Action
routingSpec = simpleSpec performAction defaultRender



wrap :: forall eff props. Spec (E eff) AppState props Action -> Spec (E eff) AppState props Action
wrap spec =
  fold
  [ sidebarnavSpec
--  TODO Add Tree to the template
--, exampleTree'
  , innerContainer $ spec
  , footerLegalInfo
  ]
  where
    innerContainer :: Spec (E eff) AppState props Action -> Spec (E eff) AppState props Action
    innerContainer = over _render \render d p s c ->
      [  div [_id "page-wrapper"]
        [
          div [className "container-fluid"]  (render d p s c)
        ]
      ]
--    TODO Add Tree to the template
--    exampleTree' ::  forall props eff. Spec (dom :: DOM |eff) AppState props Action
--    exampleTree' = simpleSpec performAction render
--      where
--        render :: Render AppState props Action
--        render dispatch _ state _ = DV.toHtml dispatch DV.exampleTree



data LiNav = LiNav { title :: String
                   , href  :: String
                   , icon  :: String
                   , text  :: String
                   }

liNav :: LiNav -> ReactElement
liNav (LiNav { title:tit
             , href :h
             , icon:i
             , text: txt
             }
      ) = li [] [ a [ tabIndex (-1)
                    , target "blank"
                    , title tit
                    , href h
                    ]
                   
                    [ span [ className i ] []
                           , text $ " " <> txt
                    ]
                ]

divLogo :: ReactElement
divLogo = div [ className "navbar-inner" ]
               [ a [ className "navbar-brand logoSmall"
                   , href "/index.html"  
                   ]
                   
                   [ img [ src "images/logoSmall.png"
                         , title "Back to home." ] 
                         []
                   ]
               ]

--divDropdownLeft :: ReactElement
--divDropdownLeft = undefined


sidebarnavSpec ::  forall props eff. Spec (dom :: DOM |eff) AppState props Action
sidebarnavSpec = simpleSpec performAction render
  where
    render :: Render AppState props Action
    render dispatch _ state _ =
      [
        div [ _id "dafixedtop"
            , className "navbar navbar-inverse navbar-fixed-top"
            , role "navigation"
            ]
        
        [ div [className "container"]
          [ divLogo
          -- divDropdownLeft
---------------------------------------------------------------------------
-- here is divDropDowLeft--------------------------------------------------
-- FIXME : divDropDownLeft and divDropDownRight seems to be intricated in dropdown?
---------------------------------------------------------------------------
          ,  div [ className "navbar-collapse collapse"]
             [ ul [className "nav navbar-nav"]
               [ ul [className "nav navbar-nav pull-left"]
                 [ li [className "dropdown"]
                   [ a [ className "dropdown-toggle navbar-text"
                       , _data {toggle: "dropdown"}
                       , href "#", role "button"
                       , title "Informations about Gargantext" 
                       ]
                       [ span [ aria {hidden : true}
                              , className "glyphicon glyphicon-info-sign" 
                              ] []
                       , text " Info"
                       ]
                   , ul [className "dropdown-menu"]
                      (( map liNav [ LiNav { title : "Quick start, tutorials and methodology"
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
                       )
                       <> [li [className "divider"] []] <>
                       (map liNav [ LiNav { title : "Interactive chat"
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
                       )
                       <> [li [className "divider"] []] <>
                               [ liNav (LiNav { title : "More about us (you)"
                                              , href  : "http://iscpif.fr"
                                              , icon  : "fas fa-question-circle"
                                              , text  : "About"
                                              }
                                       )
                               ]
                       )
                   ]
                 ]
               ]
---------------------------------------------------------------------------
-- TODO put the search form in the center of the navBar
             , ul [ className "nav navbar-nav"]
                    [ input [ className "form-control"
                            , placeholder "Query, URL or FILE (works with Firefox or Chromium browsers)"
                            , _type "text"
                            , style { height: "35px"
                                    , width : "450px"
                                  --  , color: "white"
                                  --  , background : "#A1C2D8"
                                  }
                            ] [
                              
                              ]
                    -- TODO add button in navbar (and "enter" execution)
                    -- , div [] [button [][]]
                    ]
             , divDropdownRight
            ]
          ]
        ]
      ]

--divDropdownRight :: Render AppState props Action
divDropdownRight :: ReactElement
divDropdownRight = ul [className "nav navbar-nav pull-right"]
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

layoutSpec :: forall eff props. Spec (E eff) AppState props Action
layoutSpec =
  fold
  [ routingSpec
  , container $
       withState pagesComponent
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




footerLegalInfo ::  forall props eff. Spec (dom :: DOM |eff) AppState props Action
footerLegalInfo = simpleSpec performAction render
  where
    render :: Render AppState props Action
    render dispatch _ state _ = [div [ className "container" ] [ hr [] [], footerLegalInfo']]
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

