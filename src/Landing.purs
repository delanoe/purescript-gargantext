module Landing where

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Prelude hiding (div)
import React.DOM (a, button, div, footer, h1, h3, hr, i, img, li, p, span, text, ul)
import React.DOM.Props (_data, _id, aria, className, href, onClick, role, src, style, tabIndex, target, title)
import Routing.Hash.Aff (setHash)
import Thermite (PerformAction, Render, Spec, simpleSpec)
import Thermite as T

newtype State = State
  { userName :: String
  , password :: String
  }


initialState :: State
initialState = State
  {userName : ""
 , password : ""
  }

data Action
  = NoOp
  | Documentation
  | Submit
  | SignUp


performAction :: forall eff props. PerformAction (console :: CONSOLE, ajax :: AJAX,dom::DOM | eff) State props Action
performAction NoOp _ _ = void do
  T.modifyState \state -> state

performAction Documentation _ _ = void do
  T.modifyState \state -> state

performAction Submit _ _ = void do
  lift $ setHash "/login"
  T.modifyState \state -> state

performAction SignUp _ _ = void do
  T.modifyState \state -> state



loginSpec :: forall props eff . Spec (console::CONSOLE, ajax::AJAX, dom::DOM | eff) State props Action
loginSpec = simpleSpec performAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      [
        div [ _id "dafixedtop", className "navbar navbar-inverse navbar-fixed-top", role "navigation"]
        [ div [className "container"]
          [
            div [ className "navbar-inner" ]
            [ a [ className "navbar-brand logoSmall", href "/" ]
              [ img [ src "dist/images/logoSmall.png", title "Back to home." ]
                []
              ]
            ]
          ,  div [className "navbar-collapse collapse"]
             [

             ul [className "nav navbar-nav"]
               [
                 li [className "dropdown"]
                 [
                   a [ className "dropdown-toggle navbar-text", _data {toggle: "dropdown"}, href "#", role "button", title "Informations about Gargantext" ]
                   [ span [ aria {hidden : true}, className "glyphicon glyphicon-info-sign" ]
                     []
                   , text "Info"
                   , i [ className "caret" ]
                     []
                   ]
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

      , div [className "container"]
        [
          div [className "jumbotron"]
          [
            div [className "row"]
            [
              div [className "col-md-8 content"]
              [
                h1 []
                [ text "Gargantext" ]
              , p []
                [ text "A web platform to explore text-mining" ]
              , p []
                [ button [ className "btn btn-primary btn-lg",onClick \_ -> dispatch $ Submit , title "Click and test by yourself" ]
                  [ span [ className "glyphicon glyphicon-hand-right" ]
                    []
                  , text "Login"
                  ]
                , a [ className "btn btn-warning btn-lg", href "https://iscpif.fr/services/applyforourservices/", target "blank", title "Fill the form to sign up" ]
                  [ span [ aria {hidden : true}, className "glyphicon glyphicon-hand-right" ]
                    []
                  , text "Sign Up"
                  ]
                , a [ className "btn btn-success btn-lg", href "https://iscpif.fr/gargantext/your-first-map/", target "blank", title "Fill the form to sign up" ]
                  [ span [ aria {hidden : true}, className "glyphicon glyphicon-hand-right" ]
                    []
                  , text "Documentation"
                  ]
                ]
              , span [ aria {hidden : true}, className "glyphicon glyphicon-warning-sign" ]
                []
              , i []
                [ text "Some features may not work without a javascript optimized browser (Chromium for instance).                        " ]

              ]
            , div [className "col-md-2 content"]
              [
                p [ className "right" ]
                [ div [_id "logo-designed" ]
                  [ img [ src "dist/images/logo.png", title "Logo designed by dacha and anoe" ]
                    []
                  ]
                ]
              ]
            ]
          ]
        ]

      , div [className "container"]
        [ div [className "row"]
          [ div [className "col-md-offset-5 col-md-6 content"]
            [ img [ src "dist/images/Gargantextuel-212x300.jpg", title "Gargantextuel drawn by Cecile Meadel", _id "funnyimg"] []
            ]
          ]
        ]


      , div [ className "container" ]
        [ div [ className "row" ]
          [ div [ className "col-md-4 content" ]
            [ h3 []
              [ a [ href "#", title "Random sentences in Gargantua's Books chapters, historically true" ]
                [ text "Historic" ]
              ]
            , p []
              [ text "Chapter 1.XV. How Gargantua was put under other schoolmasters. Chapter 2.XXII. How Panurge served a Parisian lady a trick that pleased her not very well. Chapter 3.XXXVII. How Pantagruel persuaded Panurge to take counsel of a fool. Chapter 4.LXI. How Gaster invented means to get and preserve corn. Chapter 5.XXXVIII. Of the temple's admirable pavement." ]
            ]
          , div [ className "col-md-4 content" ]
            [ h3 []
              [ a [ href "#", title "Randomized words, semantically and syntaxically falses." ]
                [ text "Presentation" ]
              ]
            , p []
              [ text "Autem nascetur iaculis, sedfusce enimsed cursus posuere consectetuer eu justo aliquammauris. Phasellus vero nisi porttitor elit quod, leo feliscras ultricies non tempor sagittis. Liberoduis facilisinam erat dapibusnam, lacus dui duis tristique volutpatut quis vestibulum magna. Nobis faucibusvestibulum dolores minim. Bibendumin malesuada adipiscing ante, mattis fames nequeetiam lorem. No diam id. Litora quisaenean commodo lobortisetiam neque, libero mollis scelerisque inceptos ullamcorper sea congue delenit possim.            " ]
            ]
          , div [ className "col-md-4 content" ]
            [ h3 []
              [ a [ href "#", title "Randomized letters, true or false ?" ]
                [ text "Tutoreil" ]
              ]
            , p []
              [ text "Il paraît que l'rdore des lettres dans un mot n'a pas d'imtraopnce. La première et la dernière lettre doeivnt être à la bonne place. Le reste peut être dans un désordre total et on peut touojurs lire sans prolèbme. On ne lit donc pas chaque lettre en ellêem-me, mais le mot comme un tout. Un chaegmnent de référentiel et nous tranpossons ce résultat au texte luimê-me: l'rdore des mots est failbement important copamré au contexte du texte qui, lui, est copmté: comptexter avec Gargantext.                "
              , text " "
              , text ""
              ]
            ]
          ]
        ]

      , div [className "container"]
        [
           hr [] []
      , footer []
        [ p []
          [ text "Gargantext"
          , span [className "glyphicon glyphicon-registration-mark" ]
            []
          , text ", version 3.0.6.9.4,"
          , a [ href "http://www.cnrs.fr", target "blank", title "Institution that enables this project." ]
            [ text "Copyrights"
            , span [ className "glyphicon glyphicon-copyright-mark" ]
              []
            , text "CNRS 2017"
            ]
          , a [ href "http://gitlab.iscpif.fr/humanities/gargantext/blob/stable/LICENSE", target "blank", title "Legal instructions of the project." ]
            [ text ", Licences aGPLV3 and CECILL variant Affero compliant" ]
          , text "."
          ]
        ]
        ]

      ]
