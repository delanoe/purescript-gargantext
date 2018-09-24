module Gargantext.Pages.Corpus where

import Prelude hiding (div)

import Gargantext.Components.Charts.Options.ECharts (chart)
import Gargantext.Pages.Corpus.Doc.Facets.Dashboard (globalPublis)
import Gargantext.Pages.Corpus.Doc.Facets as Tab
import React.DOM (div, h3, hr, i, p, text)
import React.DOM.Props (className, style)
import Thermite (Render, Spec, defaultPerformAction, simpleSpec)

type Corpus = { title :: String
              , desc  :: String
              , query :: String
              , date  :: String
              , authors :: String
              }

spec' :: Spec {} {} Void
spec' = corpusSpec <> Tab.pureTab1

corpusSpec :: Spec {} {} Void
corpusSpec = simpleSpec defaultPerformAction render
  where
    render :: Render {} {} Void
    render _ _ _ _ =
      [ div [className "row"]
        [ div [className "col-md-3"] [ h3 [] [text corpus.title] ]
        , div [className "col-md-9"] [ hr [style {height : "2px",backgroundColor : "black"}] ]
        ]
      , div [className "row"] [ div [className "jumbotron1", style {padding : "12px 0px 20px 12px"}]
            [ div [ className "col-md-8 content"]
                  [ p [] [ i [className "fa fa-globe"] []
                         , text corpus.desc
                         ]
                  , p [] [ i [className "fab fa-searchengin"] []
                         , text corpus.query
                         ]
                  ]
            , div [ className "col-md-4 content"]
                  [ p [] [ i [className "fa fa-calendar"] []
                         , text corpus.date
                         ]
                  , p [] [ i [className "fa fa-user"] []
                         , text corpus.authors
                         ]
                  ]
            ]
        ]
        , chart globalPublis
      ]
        where
          corpus :: Corpus
          corpus = { title : "IMT Global Publications"
                   , desc  : " Hal Database"
                   , query : " Query: all publications"
                   , date  : " June. 26 2018, 10:59 am"
                   , authors : " Author(s): françois.pineau"
                   }
