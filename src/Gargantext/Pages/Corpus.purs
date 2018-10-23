module Gargantext.Pages.Corpus where


import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, prism)
import Data.Maybe (maybe)
import Effect.Aff (Aff)
import React as React
import React (ReactClass, ReactElement)
import React.DOM (div, h3, hr, i, p, text)
import React.DOM.Props (className, style)
import Thermite ( Render, Spec, createClass, defaultPerformAction, focus
                , simpleSpec, noState )
--------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Loader as Loader
import Gargantext.Components.Loader (createLoaderClass)
import Gargantext.Config      (toUrl, NodeType(..), End(..))
import Gargantext.Config.REST (get)
import Gargantext.Pages.Corpus.Tabs.Types (CorpusInfo(..), corpusInfoDefault)
import Gargantext.Pages.Corpus.Tabs.Types (Props) as Tabs
import Gargantext.Pages.Corpus.Tabs.States (State, initialState) as Tabs
import Gargantext.Pages.Corpus.Tabs.Actions (Action) as Tabs
import Gargantext.Pages.Corpus.Tabs.Specs (statefulTabs) as Tabs
-------------------------------------------------------------------
type Props = Tabs.Props

type State = { tabsView    :: Tabs.State
             }

initialState :: State
initialState = { tabsView    : Tabs.initialState
               }

------------------------------------------------------------------------
_tabsView :: forall a b. Lens' { tabsView :: a | b } a
_tabsView = lens (\s -> s.tabsView) (\s ss -> s{tabsView = ss})
------------------------------------------------------------------------

data Action
  = TabsA   Tabs.Action

_tabsAction :: Prism' Action Tabs.Action
_tabsAction = prism TabsA \ action ->
  case action of
    TabsA taction -> Right taction
    -- _-> Left action

------------------------------------------------------------------------
layout :: Spec {} {nodeId :: Int} Void
layout = simpleSpec defaultPerformAction render
  where
    render :: Render {} {nodeId :: Int} Void
    render _ {nodeId} _ _ =
      [ corpusLoader { path: nodeId
                     , component: createClass "Layout" layout' initialState
                     } ]

layout' :: Spec State Props Action
layout' = noState corpusHeaderSpec
       <> focus _tabsView _tabsAction Tabs.statefulTabs

corpusHeaderSpec :: Spec {} Props Void
corpusHeaderSpec = simpleSpec defaultPerformAction render
  where
    render :: Render {} Props Void
    render dispatch {loaded} _ _ =
        [ div [className "row"]
          [ div [className "col-md-3"] [ h3 [] [text "Corpus " <> text title] ]
          , div [className "col-md-9"] [ hr [style {height : "2px",backgroundColor : "black"}] ]
          ]
        , div [className "row"] [ div [className "jumbotron1", style {padding : "12px 0px 20px 12px"}]
              [ div [ className "col-md-8 content"]
                    [ p [] [ i [className "fa fa-globe"] []
                           , text $ " " <> corpus.desc
                           ]
                    , p [] [ i [className "fab fa-searchengin"] []
                           , text $ " " <> corpus.query
                           ]
                    ]
              , div [ className "col-md-4 content"]
                    [ p [] [ i [className "fa fa-calendar"] []
                           , text $ " " <> date'
                           ]
                    , p [] [ i [className "fa fa-user"] []
                           , text $ " " <> corpus.authors
                           ]
                    ]
              ]
          ]
        ]
          where
            NodePoly { name: title
                     , date: date'
                     , hyperdata : CorpusInfo corpus
                   }
              = maybe corpusInfoDefault identity loaded

------------------------------------------------------------------------

getCorpus :: Int -> Aff (NodePoly CorpusInfo)
getCorpus = get <<< toUrl Back Corpus

corpusLoaderClass :: ReactClass (Loader.Props Int (NodePoly CorpusInfo))
corpusLoaderClass = createLoaderClass "CorpusLoader" getCorpus

corpusLoader :: Loader.Props Int (NodePoly CorpusInfo) -> ReactElement
corpusLoader = React.createLeafElement corpusLoaderClass
