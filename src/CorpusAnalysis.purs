module CorpusAnalysis where
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Prelude (id, void)
import Prelude hiding (div)
import React.DOM (div, h3, hr, i, p, span, text)
import React.DOM.Props (className, style)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)


type State = String

initialState :: State
initialState = ""

data Action = NoOp

performAction :: forall eff props. PerformAction ( console :: CONSOLE
                                                 , ajax    :: AJAX
                                                 , dom     :: DOM
                                                 | eff
                                                 ) State props Action
performAction NoOp _ _ = void do
  modifyState id


corpusAnalysisSpec :: forall props eff . Spec ( console :: CONSOLE
                                        , ajax    :: AJAX
                                        , dom     :: DOM
                                        | eff
                                        ) State props Action
corpusAnalysisSpec = simpleSpec performAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      [ div [className "row"]
        [ div [className "col-md-3"]
          [ h3 [] [text "Bisphenol A"]

          ]
        , div [className "col-md-9"]
          [ hr [style {height : "2px",backgroundColor : "black"}] []
          ]
        ]
      , div [className "row"]
        [ div [className "col-md-5"]
          [ div [className "jumbotron1", style {padding : "12px 0px 20px 12px"}]
            [ p []
              [ i [className "fa fa-globe"] []
              , text "IS Tex"
              ]
            , p []
              [ i [className "fa fa-file-archive-o"] []
              , text "bisphenol + A OR bpa"
              ]
            , p []
              [ i [className "fa fa-calendar"] []
              , text "Sept. 11 2017, 10:59 am,"
              ]
            , p []
              [ i [className "fa fa-user"] []
              , text "Authors (S): gargantext,"
              ]
            ]
          ]
        , div [className "col-md-6"]
          []
        ]
      ]
