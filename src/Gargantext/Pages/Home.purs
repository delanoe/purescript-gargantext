module Landing where

import Prelude hiding (div)

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Gargantext.Components.Lang.Landing.EnUS as En
import Gargantext.Components.Lang.Landing.FrFR as Fr
import Gargantext.Components.Data.Landing (BlockText(..), BlockTexts(..), Button(..), LandingData(..))
import Gargantext.Components.Data.Lang (Lang(..))
import Network.HTTP.Affjax (AJAX)
import React (ReactElement)
import React.DOM (a, div, h3, i, img, p, span, text)
import React.DOM.Props (Props, _id, aria, className, href, src, target, title)
import Routing.Hash.Aff (setHash)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)

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
  modifyState \state -> state

performAction Documentation _ _ = void do
  modifyState \state -> state

performAction Enter _ _ = void do
  lift $ setHash "/search"
  modifyState \state -> state

performAction Login _ _ = void do
  lift $ setHash "/login"
  modifyState \state -> state

performAction SignUp _ _ = void do
  modifyState \state -> state


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
