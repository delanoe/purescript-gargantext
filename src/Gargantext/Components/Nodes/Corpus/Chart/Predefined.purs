module Gargantext.Components.Nodes.Corpus.Chart.Predefined where

import Gargantext.Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Gargantext.Components.Nodes.Corpus.Chart.Histo (histo)
import Gargantext.Components.Nodes.Corpus.Chart.Metrics (metrics)
import Gargantext.Components.Nodes.Corpus.Chart.Pie (pie)
import Gargantext.Components.Nodes.Corpus.Chart.Tree (tree)
import Gargantext.Sessions (Session)
import Gargantext.Types (NodeID, Mode(..), TabSubType(..), TabType(..), modeTabType)
import Reactix as R
import Simple.JSON as JSON
import Simple.JSON.Generics as JSONG


data PredefinedChart =
    CDocsHistogram
  | CAuthorsPie
  | CSourcesBar
  | CInstitutesTree
  | CTermsMetrics

derive instance Generic PredefinedChart _
instance JSON.ReadForeign PredefinedChart where readImpl = JSONG.enumSumRep
instance JSON.WriteForeign PredefinedChart where writeImpl = JSON.writeImpl <<< show
instance Show PredefinedChart where
  show = genericShow
derive instance Eq PredefinedChart
instance Ord PredefinedChart where
  compare = genericCompare

instance Read PredefinedChart where
  read "CDocsHistogram"  = Just CDocsHistogram
  read "CAuthorsPie"     = Just CAuthorsPie
  read "CSourcesBar"     = Just CSourcesBar
  read "CInstitutesTree" = Just CInstitutesTree
  read "CTermsMetrics"   = Just CTermsMetrics
  read _                 = Nothing


allPredefinedCharts :: Array PredefinedChart
allPredefinedCharts =
  [ CDocsHistogram
  , CAuthorsPie
--  , CTermsMetrics
  , CInstitutesTree
  , CSourcesBar
  ]


type Params =
  ( corpusId :: NodeID
  , session  :: Session
  -- optinal params
  , limit    :: Maybe Int
  , listId   :: Maybe Int
  )

render :: PredefinedChart -> Record Params -> R.Element
render CDocsHistogram { corpusId, listId, session } = histo { path, session }
  where
    path = { corpusId
           , listId: fromMaybe 0 listId
           , limit: Nothing
           , tabType: TabCorpus TabDocs
           }
render CAuthorsPie { corpusId, listId, session } = pie { path, session }
  where
    path = { corpusId
           , listId: fromMaybe 0 listId
           , limit: Nothing
           , tabType: TabCorpus (TabNgramType $ modeTabType Authors)
           }
render CInstitutesTree { corpusId, limit, listId, session } = tree { path, session }
  where
    path = { corpusId
           , limit
           , listId: fromMaybe 0 listId
           , tabType: TabCorpus (TabNgramType $ modeTabType Institutes)
           }
render CTermsMetrics { corpusId, limit, listId, session } = metrics { path, session }
  where
    path = { corpusId
           , limit
           , listId: fromMaybe 0 listId
           , tabType: TabCorpus (TabNgramType $ modeTabType Terms)
           }
render CSourcesBar { corpusId, limit, listId, session } = metrics { path, session }
  where
    path = { corpusId
           , limit
           , listId: fromMaybe 0 listId
           , tabType: TabCorpus (TabNgramType $ modeTabType Sources)
           }




