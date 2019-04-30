module Gargantext.Pages.Corpus where


import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import React as React
import React (ReactClass, ReactElement)
import Thermite (Spec, Render, simpleSpec, createClass, defaultPerformAction)
--------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Components.Node (NodePoly(..), HyperdataList(..))
import Gargantext.Components.Loader as Loader
import Gargantext.Components.Loader (createLoaderClass)
import Gargantext.Components.Table as Table
import Gargantext.Config      (toUrl, Path(..), NodeType(..), End(..))
import Gargantext.Config.REST (get)
import Gargantext.Pages.Corpus.Tabs.Types (CorpusData, CorpusInfo(..))
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
    render dispatch {loaded: {corpusNode}} _ _ =
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
          = corpusNode

------------------------------------------------------------------------

getCorpus :: Int -> Aff CorpusData
getCorpus corpusId = do
  corpusNode     <- get $ toUrl Back Corpus $ Just corpusId
  defaultListIds <- get $ toUrl Back (Children NodeList 0 1 Nothing) $ Just corpusId
  case (head defaultListIds :: Maybe (NodePoly HyperdataList)) of
    Just (NodePoly { id: defaultListId }) ->
      pure {corpusNode, defaultListId}
    Nothing ->
      throwError $ error "Missing default list"

corpusLoaderClass :: ReactClass (Loader.Props Int CorpusData)
corpusLoaderClass = createLoaderClass "CorpusLoader" getCorpus

corpusLoader :: Loader.Props' Int CorpusData -> ReactElement
corpusLoader props = React.createElement corpusLoaderClass props []
