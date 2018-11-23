module Gargantext.Pages.Corpus where


import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, prism)
import Data.Maybe (Maybe(..),maybe)
import Effect.Aff (Aff)
import React as React
import React (ReactClass, ReactElement)
import Thermite ( Render, Spec, createClass, defaultPerformAction, focus
                , simpleSpec, noState )
--------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Loader as Loader
import Gargantext.Components.Loader (createLoaderClass)
import Gargantext.Components.Table as Table
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

initialState :: Props -> State
initialState _props =
  { tabsView    : Tabs.initialState {} }

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
      Table.renderTableHeaderLayout
        { title: "Corpus " <> title
        , desc:  corpus.desc
        , query: corpus.query
        , date:  date'
        , user:  corpus.authors
        }
      where
        NodePoly { name: title
                 , date: date'
                 , hyperdata : CorpusInfo corpus
                 }
          = maybe corpusInfoDefault identity loaded

------------------------------------------------------------------------

getCorpus :: Int -> Aff (NodePoly CorpusInfo)
getCorpus = get <<< toUrl Back Corpus <<< Just

corpusLoaderClass :: ReactClass (Loader.Props Int (NodePoly CorpusInfo))
corpusLoaderClass = createLoaderClass "CorpusLoader" getCorpus

corpusLoader :: Loader.Props' Int (NodePoly CorpusInfo) -> ReactElement
corpusLoader props = React.createElement corpusLoaderClass props []
