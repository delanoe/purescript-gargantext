module Landing where

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Prelude hiding (div)
import React.DOM (a, button, div, footer, h1, h3, hr, i, img, li, p, span, text, ul)
import React.DOM.Props (Props, _data, _id, aria, className, href, onClick, role, src, style, tabIndex, target, title)
import React (ReactElement)
import Routing.Hash.Aff (setHash)
import Thermite (PerformAction, Render, Spec, simpleSpec)
import Thermite as T

import Gargantext.Data.Landing
import Gargantext.Data.Lang

import Gargantext.Lang.Landing.EnUS as En
import Gargantext.Lang.Landing.FrFR as Fr

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


performAction :: forall eff props. PerformAction ( console :: CONSOLE
                                                 , ajax    :: AJAX
                                                 , dom     :: DOM
                                                 | eff
                                                 ) State props Action
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


-- Layout |

layoutLanding :: forall props eff . Lang -> Spec ( console :: CONSOLE
                                              , ajax    :: AJAX
                                              , dom     :: DOM
                                              | eff
                                              ) State props Action
layoutLanding FR = layoutLanding' Fr.landingData
layoutLanding EN = layoutLanding' En.landingData

------------------------------------------------------------------------

layoutLanding' :: forall props eff . LandingData -> Spec ( console :: CONSOLE
                                                   , ajax    :: AJAX
                                                   , dom     :: DOM
                                                   | eff
                                                   ) State props Action
layoutLanding' hd = simpleSpec performAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      [ div [ className "container1" ] [ jumboTitle hd false                 ]
      , div [ className "container1" ] [] -- put research here
      , div [ className "container1" ] [ blocksRandomText' hd                ]
      ]
------------------------------------------------------------------------

blocksRandomText' :: LandingData -> ReactElement
blocksRandomText' (LandingData hd) = blocksRandomText hd.blockTexts


blocksRandomText :: BlockTexts -> ReactElement
blocksRandomText (BlockTexts bt) =
  div [ className "row" ] ( map showBlock bt.blocks )
    where
      showBlock :: BlockText -> ReactElement
      showBlock (BlockText b) =
        div [ className "col-md-4 content" ]
              [ h3 [] [ a [ href b.href, title b.title]
                          [ i [className b.icon] []
                          , text ("   " <> b.titleText)
                          ]
                      ]
              , p [] [ text b.text ]
              , p [] [ docButton b.docButton ]
              ]

docButton :: Button -> ReactElement
docButton (Button b) = a [ className "btn btn-outline-primary btn-sm spacing-class"
              , href b.href
              , target "blank"
              , title b.title
              ] [ span [ aria {hidden : true}
                       , className "glyphicon glyphicon-hand-right"
                       ]  []
                , text b.text
                ]

jumboTitle :: LandingData -> Boolean -> ReactElement
jumboTitle (LandingData hd) b = div jumbo
                   [ div [className "row"             ]
                     [ div [ className "col-md-8 content"]
                           [ p [ className "left" ]
                               [ div [_id "logo-designed" ]
                                 [ img [ src "images/logo.png"
                                       , title hd.logoTitle
                                       ] []
                                 ]
                               ]
                           ]
                     , div [ className "col-md-4 content"]
                           [ img [ src "images/Gargantextuel.jpg"
                                   , _id "funnyimg"
                                   , title hd.imageTitle
                                   ]
                                   []
                           ]
                     ]
                   ]
                  where
                    jumbo = case b of
                                 true  -> [className "jumbotron"]
                                 false -> []

imageEnter :: LandingData -> Props -> ReactElement
imageEnter (LandingData hd) action =  div [className "row"]
                           [ div [className "col-md-offset-5 col-md-6 content"]
                             [ img [ src "images/Gargantextuel-212x300.jpg"
                                   , _id "funnyimg"
                                   , title hd.imageTitle
                                   , action
                                   ]
                                   []
                             ]
                           ]
