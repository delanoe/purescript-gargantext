module Gargantext.Pages.Lists.Tabs where

import Data.Argonaut (class DecodeJson, decodeJson, (.?), (.??))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Reactix as R
import Reactix.DOM.HTML as H
import Unsafe.Coerce (unsafeCoerce)
import Gargantext.Prelude
import Gargantext.Components.Charts.Options.ECharts (chart) as ECharts
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.Loader as Loader
import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Tab as Tab
import Gargantext.Pages.Corpus.Chart.Histo (histo)
import Gargantext.Pages.Corpus.Chart.Metrics (metrics)
import Gargantext.Pages.Corpus.Chart.Pie  (pie, bar)
import Gargantext.Pages.Corpus.Chart.Tree (tree)
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType(..), TabType(..), TabSubType(..))
import Gargantext.Utils.Reactix as R2

data Mode = Authors | Sources | Institutes | Terms

derive instance genericMode :: Generic Mode _

instance showMode :: Show Mode where
  show = genericShow

derive instance eqMode :: Eq Mode

modeTabType :: Mode -> CTabNgramType
modeTabType Authors    = CTabAuthors
modeTabType Sources    = CTabSources
modeTabType Institutes = CTabInstitutes
modeTabType Terms      = CTabTerms

type Props =
  ( session :: Session
  , corpusId :: Int
  , corpusData :: CorpusData )

tabs :: Record Props -> R.Element
tabs props = R.createElement tabsCpt props []

tabsCpt :: R.Component Props
tabsCpt = R.hooksComponent "CorpusTabs" cpt
  where
    cpt {session, corpusId, corpusData: corpusData@{defaultListId}} _ = do
      (selected /\ setSelected) <- R.useState' 0
      pure $ Tab.tabs { tabs: tabs', selected }
      where
        tabs' = [ "Sources"    /\ view Sources
                , "Authors"    /\ view Authors
                , "Institutes" /\ view Institutes
                , "Terms"      /\ view Terms ]
        view mode = ngramsView {mode, session, corpusId, corpusData}

type NgramsViewProps = ( mode :: Mode | Props )

ngramsView :: Record NgramsViewProps -> R.Element
ngramsView props = R.createElement ngramsViewCpt props []

ngramsViewCpt :: R.Component NgramsViewProps
ngramsViewCpt = R.staticComponent "ListsNgramsView" cpt
  where
    cpt {mode, session, corpusId, corpusData: {defaultListId}} _ =
      NT.mainNgramsTable
      {session, defaultListId, nodeId: corpusId, tabType, tabNgramType}
      where
        tabNgramType = modeTabType mode
        tabType = TabCorpus (TabNgramType tabNgramType)
        listId = 0 -- TODO!
        path = {corpusId, tabType}
        path2 = {corpusId, listId, tabType, limit: (Just 1000)} -- todo
        chart Authors = pie {session, path}
        chart Sources = bar {session, path}
        chart Institutes = tree {session, path: path2}
        chart Terms = metrics {session, path: path2}

newtype CorpusInfo =
  CorpusInfo
  { title        :: String
  , desc         :: String
  , query        :: String
  , authors      :: String
  , chart        :: (Maybe (Array Number))
  , totalRecords :: Int }
  
hyperdataDefault =
  CorpusInfo
  { title : "Default title"
  , desc  : " Default desc"
  , query : " Default Query"
  , authors : " Author(s): default"
  , chart   : Nothing
  , totalRecords : 0 }

corpusInfoDefault :: NodePoly CorpusInfo
corpusInfoDefault =
  NodePoly
  { id : 0
  , typename : 0
  , userId : 0
  , parentId : 0
  , name : "Default name"
  , date  : " Default date"
  , hyperdata : hyperdataDefault }

instance decodeCorpusInfo :: DecodeJson CorpusInfo where
  decodeJson json = do
    obj <- decodeJson json
    title <- obj .? "title"
    desc  <- obj .? "desc"
    query <- obj .? "query"
    authors <- obj .? "authors"
    chart   <- obj .?? "chart"
    let totalRecords = 47361 -- TODO
    pure $ CorpusInfo {title, desc, query, authors, chart, totalRecords}

type CorpusData = { corpusId :: Int
                  , corpusNode :: NodePoly CorpusInfo
                  , defaultListId :: Int}


