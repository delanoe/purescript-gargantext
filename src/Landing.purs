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
  | Enter
  | Login
  | SignUp


performAction :: forall eff props. PerformAction (console :: CONSOLE, ajax :: AJAX,dom::DOM | eff) State props Action
performAction NoOp _ _ = void do
  T.modifyState \state -> state

performAction Documentation _ _ = void do
  T.modifyState \state -> state

performAction Enter _ _ = void do
  lift $ setHash "/search"
  T.modifyState \state -> state

performAction Login _ _ = void do
  lift $ setHash "/login"
  T.modifyState \state -> state

performAction SignUp _ _ = void do
  T.modifyState \state -> state



loginSpec :: forall props eff . Spec (console::CONSOLE, ajax::AJAX, dom::DOM | eff) State props Action
loginSpec = simpleSpec performAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      [ div [className "container"       ]
        [ div [className "jumbotron"       ]
          [ div [className "row"             ]
            [ div [className "col-md-8 content"]
              [ h1 [] [ text "Gargantext"]
              , p  [] [ text "Collaborative knowledge mapping experience" ]
              , p  [] [ a [ className "btn btn-success btn-lg spacing-class"
                          , href "https://iscpif.fr/gargantext/your-first-map/"
                          , target "blank"
                          , title "Your first map in less than 5 minutes" 
                          ]
                          [ span [ aria {hidden : true}
                                 , className "glyphicon glyphicon-hand-right" 
                                 ]  []
                          , text " Get's started"
                          ]
                       ]
              ]
            , div [ className "col-md-2 content"]
                  [p [ className "right" ]
                     [ div [_id "logo-designed" ]
                     [ img [ src "images/logo.png", title "Logo designed by dacha and anoe" ]
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
            [ img [ src "images/Gargantextuel-212x300.jpg"
                  , title "Gargantextuel drawn by Cecile Meadel"
                  , _id "funnyimg"
                  , onClick \_ -> dispatch $ Enter , title "Click and test by yourself"
                  ] []
            ]
          ]
        ]


      , div [ className "container" ]
        [ div [ className "row" ]
          [ div [ className "col-md-4 content" ]
            [ h3 []
              [ a [ href "#", title "Random sentences in Gargantua's Books chapters, historically true" ]
              -- TODO click on icon and randomize the text below
                [ i [className "fas fa-random"] []
                , text "   Historic" 
                ]
              ]
            , p []
            -- TODO use RandomText.randomSentences on this text (should be editable by user later)
              [ text "Chapter 1.XV. How Gargantua was put under other schoolmasters. Chapter 2.XXII. How Panurge served a Parisian lady a trick that pleased her not very well. Chapter 3.XXXVII. How Pantagruel persuaded Panurge to take counsel of a fool. Chapter 4.LXI. How Gaster invented means to get and preserve corn. Chapter 5.XXXVIII. Of the temple's admirable pavement." ]
            ]
          , div [ className "col-md-4 content" ]
            [ h3 []
              [ a [ href "#", title "Randomized words, semantically and syntaxically falses." ]
              -- TODO click on icon and randomize the text below
                [ i [className "fas fa-random"] []
                , text "   Presentation" 
                ]
              ]
            , p []
            -- TODO use RandomText.randomWords on this text (should be editable by user later)
              [ text "Autem nascetur iaculis, sedfusce enimsed cursus posuere consectetuer eu justo aliquammauris. Phasellus vero nisi porttitor elit quod, leo feliscras ultricies non tempor sagittis. Liberoduis facilisinam erat dapibusnam, lacus dui duis tristique volutpatut quis vestibulum magna. Nobis faucibusvestibulum dolores minim. Bibendumin malesuada adipiscing ante, mattis fames nequeetiam lorem. No diam id. Litora quisaenean commodo lobortisetiam neque, libero mollis scelerisque inceptos ullamcorper sea congue delenit possim.            " ]
            ]
          , div [ className "col-md-4 content" ]
            [ h3 []
              [ a [ href "#", title "Randomized letters, true or false ?" ]
              -- TODO click on icon and randomize the text below
                [ i [className "fas fa-random"] []
                , text "   Tutoreil" 
                ]
              ]
            , p []
            -- TODO use RandomText.randomChars on this text (should be editable by user later)
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
          [ text "Gargantext "
          , span [className "glyphicon glyphicon-registration-mark" ]
            []
          , text ", version 4.0"
          , a [ href "http://www.cnrs.fr", target "blank", title "Institution that enables this project." ]
            [ text ", Copyrights "
            , span [ className "glyphicon glyphicon-copyright-mark" ]
              []
            , text " CNRS 2017-Present"
            ]
          , a [ href "http://gitlab.iscpif.fr/humanities/gargantext/blob/stable/LICENSE", target "blank", title "Legal instructions of the project." ]
            [ text ", Licences aGPLV3 and CECILL variant Affero compliant" ]
          , text "."
          ]
        ]
        ]

      ]
