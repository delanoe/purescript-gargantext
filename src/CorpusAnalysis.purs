module CorpusAnalysis where

import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Array (fold)
import Network.HTTP.Affjax (AJAX)
import Prelude (id, void)
import Prelude hiding (div)
import React.DOM (div, h3, hr, i, p, span, text, input)
import React.DOM.Props (className, style)
import Tabview as Tab
import Thermite (PerformAction, Render, Spec, defaultPerformAction, modifyState, simpleSpec)

import Gargantext.Charts.ECharts (chart)
import Gargantext.Dashboard (globalPublis)

type State = Tab.State

type Action = Tab.Action

initialState :: State
initialState = Tab.initialState

spec' :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) Tab.State props Tab.Action
spec' = fold [ corpusAnalysisSpec
             , Tab.tab1
             ]

corpusAnalysisSpec :: forall props eff . Spec eff Tab.State props Tab.Action
corpusAnalysisSpec = simpleSpec defaultPerformAction render
  where
    render :: Render Tab.State props Tab.Action
    render dispatch _ state _ =
      [ div [className "row"]
        [ div [className "col-md-3"]
          [ h3 [] [text "IMT Global publications"]

          ]
        , div [className "col-md-9"]
          [ hr [style {height : "2px",backgroundColor : "black"}] []
          ]
        ]
      , div [className "row"]
          [ div [className "jumbotron1", style {padding : "12px 0px 20px 12px"}]
            [ div [ className "col-md-8 content"]
                  [ p [] [ i [className "fa fa-globe"] []
                         , text " Hal Database"
                         ]
                  , p [] [ i [className "fab fa-searchengin"] []
                         , text " Query: all publications with all schools ids"
                         ]
                  ]
            , div [ className "col-md-4 content"] 
                  [ p [] [ i [className "fa fa-calendar"] []
                         , text " June. 26 2018, 10:59 am"
                         ]
                  , p [] [ i [className "fa fa-user"] []
                         , text " Author(s): françois.pineau"
                         ]
                  ]
            ]
        ]
        , chart globalPublis
      ]
