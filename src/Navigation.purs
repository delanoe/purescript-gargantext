module Navigation where

import DOM

import AddCorpusview as AC
import Control.Monad.Eff.Console (CONSOLE)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Lens (Lens', Prism', lens, over, prism)
import Data.Maybe (Maybe, Maybe(Nothing, Just))
import Landing as L
import Login as LN
import Network.HTTP.Affjax (AJAX)
import PageRouter (Routes(..))
import Prelude hiding (div)
import React.DOM (a, div, i, img, li, span,  text, ul)
import React.DOM.Props (_data, _id, aria, className, href, role, src, style, tabIndex, target, title)
import Thermite (PerformAction, Render, Spec, _render, defaultRender, focus, modifyState, simpleSpec, withState)

type E e = (dom :: DOM, ajax :: AJAX, console :: CONSOLE | e)

type AppState =
  { currentRoute :: Maybe Routes
  , landingState :: L.State
  , loginState :: LN.State
  , addCorpusState :: AC.State
  }

initAppState :: AppState
initAppState =
  { currentRoute : Just AddCorpus
  , landingState : L.initialState
  , loginState : LN.initialState
  , addCorpusState : AC.initialState
  }


data Action
  = Initialize
  | LandingA L.Action
  | LoginA LN.Action
  | SetRoute Routes
  | AddCorpusA AC.Action


performAction :: forall eff props. PerformAction (dom :: DOM |eff) AppState props Action
performAction (SetRoute route)  _ _ = void do
  modifyState $ _ {currentRoute = pure route}

performAction _ _ _ = void do
  modifyState id



---- Lens and Prism
_landingState:: Lens' AppState L.State
_landingState = lens (\s -> s.landingState) (\s ss -> s{landingState = ss})


_landingAction :: Prism' Action L.Action
_landingAction = prism LandingA \action ->
  case action of
    LandingA caction -> Right caction
    _-> Left action



_loginState:: Lens' AppState LN.State
_loginState = lens (\s -> s.loginState) (\s ss -> s{loginState = ss})


_loginAction :: Prism' Action LN.Action
_loginAction = prism LoginA \action ->
  case action of
    LoginA caction -> Right caction
    _-> Left action



_addCorpusState:: Lens' AppState AC.State
_addCorpusState = lens (\s -> s.addCorpusState) (\s ss -> s{addCorpusState = ss})


_addCorpusAction :: Prism' Action AC.Action
_addCorpusAction = prism AddCorpusA \action ->
  case action of
    AddCorpusA caction -> Right caction
    _-> Left action



pagesComponent :: forall props eff. AppState -> Spec (E eff) AppState props Action
pagesComponent s =
  case s.currentRoute of
    Just route ->
      selectSpec route
    Nothing ->
      selectSpec Home
  where
    selectSpec :: Routes -> Spec (ajax :: AJAX, console :: CONSOLE, dom :: DOM | eff) AppState props Action
    selectSpec Home = wrap $ focus _landingState _landingAction L.loginSpec
    selectSpec Login   =  focus _loginState _loginAction LN.renderSpec
    selectSpec AddCorpus   = wrap $ focus _addCorpusState _addCorpusAction AC.addcorpusviewSpec

routingSpec :: forall props eff. Spec (dom :: DOM |eff) AppState props Action
routingSpec = simpleSpec performAction defaultRender

wrap :: forall eff props. Spec (E eff) AppState props Action -> Spec (E eff) AppState props Action
wrap spec =
  fold
  [ sidebarnavSpec
  , innerContainer $ spec
  ]
  where
    innerContainer :: Spec (E eff) AppState props Action -> Spec (E eff) AppState props Action
    innerContainer = over _render \render d p s c ->
      [  div [_id "page-wrapper"]
        [
          div[className "container-fluid"]  (render d p s c)
        ]
      ]


sidebarnavSpec ::  forall props eff. Spec (dom :: DOM |eff) AppState props Action
sidebarnavSpec = simpleSpec performAction render
  where
    render :: Render AppState props Action
    render dispatch _ state _ =
      [
        div [ _id "dafixedtop", className "navbar navbar-inverse navbar-fixed-top", role "navigation"]
        [ div [className "container"]
          [
            div [ className "navbar-inner" ]
            [ a [ className "navbar-brand logoSmall", href "/" ]
              [ img [ src "images/logoSmall.png", title "Back to home." ]
                []
              ]
            ]
          ,  div [className "navbar-collapse collapse"]
             [

             ul [className "nav navbar-nav"]
               [
                 ul [className "nav navbar-nav pull-left"] [
                 li [className "dropdown"]
                 [
                   a [
                      className "dropdown-toggle navbar-text", _data {toggle: "dropdown"}, href "#", role "button", title "Informations about Gargantext" ]
                   [ span [ aria {hidden : true}, className "glyphicon glyphicon-info-sign" ]
                     []
                   , text "Info"
                   , i [ className "caret" ]
                     []
                   ]

               , ul [className "dropdown-menu"]
                 [ li []
                   [ a [tabIndex (-1),  target "blank", title "Documentation and tutorials", href "https://iscpif.fr/gargantext/your-first-map/"]
                     [text "Documentation"]
                   ]
                 , li [className "divider"] []
                 , li []
                   [
                     a [ tabIndex (-1), target "blank", title "About", href "/about/", title "More informations about the project, its sponsors and its authors"]
                     [ text "About"]

                   ]
                 ]
                 ]

                 ]
              ]
             , ul [className "nav navbar-nav pull-right"]
               [
                 li [className "dropdown"]
                 [
                   a [ className "dropdown-toggle navbar-text", _data {toggle : "dropdown"}, href "#",  role "button", title "That is your username" ]
                   [ i [ className "" ]
                     []
                   , span [ aria {hidden : true}, className "glyphicon glyphicon-user", style {color:"white"} ]
                     []
                   , i [ className "caret" ]
                     []
                   ]
                 , ul [className "dropdown-menu"]
                   [
                    li []
                     [ a [tabIndex (-1), target "blank", title "Send us a message (bug, thanks, congrats...)", href "https://www.iscpif.fr/gargantext/feedback-and-bug-reports/"]
                       [
                         span [ className "glyphicon glyphicon-bullhorn" ,aria {hidden : true}] []
                        , text "Report Feedback"
                       ]
                     ]
                   , li [ className"divider"]
                     []
                   , li []
                     [ a [tabIndex (-1), href "/auth/login" ]
                       [ span [className "glyphicon glyphicon-log-in",aria {hidden : true}] []
                       , text "login"
                       ]
                     ]
                   ]
                 ]
               ]
             ]

          ]
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

dispatchAction :: forall t115 t445 t447.  Bind t445 =>  Applicative t445 => (Action -> t445 t447) -> t115 -> Routes -> t445 Unit
dispatchAction dispatcher _ Home = do
  _ <- dispatcher $ SetRoute $ Home
  _ <- dispatcher $ LandingA $ L.NoOp
  pure unit
dispatchAction dispatcher _ Login = do
  _ <- dispatcher $ SetRoute $ Login
  _ <- dispatcher $ LoginA $ LN.NoOp
  pure unit

dispatchAction dispatcher _ AddCorpus = do
  _ <- dispatcher $ SetRoute $ AddCorpus
  _ <- dispatcher $ AddCorpusA $ AC.NoOp
  pure unit
