module Gargantext.Pages.Home.Specs where

import Prelude hiding (div)

import Data.Lens (re)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (unwrap)

import Gargantext.Components.Lang.Landing.EnUS as En
import Gargantext.Components.Lang.Landing.FrFR as Fr
import Gargantext.Components.Data.Landing (BlockText(..), BlockTexts(..), Button(..), LandingData(..))
import Gargantext.Components.Data.Lang (Lang(..))
import Gargantext.Pages.Home.States (State, initialState)
import Gargantext.Pages.Home.Actions (Action, performAction)

import React (ReactElement)
import React.DOM (a, div, h3, i, img, p, span, text)
import React.DOM.Props (Props, _id, aria, className, href, src, target, title)
import Thermite (Render, Spec, simpleSpec, hideState, focusState)


-- Layout |

landingData :: Lang -> LandingData
landingData FR = Fr.landingData
landingData EN = En.landingData

layoutLanding :: Lang -> Spec {} {} Void
layoutLanding = hideState (unwrap initialState)
            <<< focusState (re _Newtype)
            <<< layoutLanding' <<< landingData

------------------------------------------------------------------------

layoutLanding' :: LandingData -> Spec State {} Action
layoutLanding' hd = simpleSpec performAction render
  where
    render :: Render State {} Action
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
                           [ div [ className "left" ]
                               [ div [_id "logo-designed" ]
                                 [ img [ src "images/logo.png"
                                       , title hd.logoTitle
                                       ]
                                 ]
                               ]
                           ]
                     , div [ className "col-md-4 content"]
                           [ img [ src "images/Gargantextuel.jpg"
                                   , _id "funnyimg"
                                   , title hd.imageTitle
                                   ]

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
                             ]
                           ]
