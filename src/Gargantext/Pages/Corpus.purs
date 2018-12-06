module Gargantext.Pages.Corpus where


import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, prism)
import Data.Maybe (Maybe(..))
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
import Gargantext.Pages.Corpus.Tabs.Types (CorpusInfo(..))
import Gargantext.Pages.Corpus.Tabs.Types (Props) as Tabs
import Gargantext.Pages.Corpus.Tabs.Specs (pureTabs) as Tabs
-------------------------------------------------------------------
type Props = Tabs.Props

------------------------------------------------------------------------
layout :: Spec {} {nodeId :: Int} Void
layout = simpleSpec defaultPerformAction render
  where
    render :: Render {} {nodeId :: Int} Void
    render _ {nodeId} _ _ =
      [ corpusLoader { path: nodeId
                     , component: createClass "Layout" layout' (const {})
                     } ]

layout' :: Spec {} Props Void
layout' = corpusHeaderSpec <> Tabs.pureTabs

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
          = loaded

------------------------------------------------------------------------

getCorpus :: Int -> Aff (NodePoly CorpusInfo)
getCorpus = get <<< toUrl Back Corpus <<< Just

corpusLoaderClass :: ReactClass (Loader.Props Int (NodePoly CorpusInfo))
corpusLoaderClass = createLoaderClass "CorpusLoader" getCorpus

corpusLoader :: Loader.Props' Int (NodePoly CorpusInfo) -> ReactElement
corpusLoader props = React.createElement corpusLoaderClass props []
