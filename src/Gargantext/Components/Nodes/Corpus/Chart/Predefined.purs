module Gargantext.Components.Nodes.Corpus.Chart.Predefined where

import Gargantext.Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.Charts.Options.Type (EChartsInstance, MouseEvent)
import Gargantext.Components.Nodes.Corpus.Chart.Histo (histo)
import Gargantext.Components.Nodes.Corpus.Chart.Metrics (metrics)
import Gargantext.Components.Nodes.Corpus.Chart.Pie (bar, pie)
import Gargantext.Components.Nodes.Corpus.Chart.Tree (tree)
import Gargantext.Sessions (Session)
import Gargantext.Types (Mode(..), NodeID, TabSubType(..), TabType(..), modeTabType)
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
  ( boxes    :: Boxes
  , corpusId :: NodeID
  -- optinal params
  , limit    :: Maybe Int
  , listId   :: Maybe Int
  , onClick  :: Maybe (MouseEvent -> Effect Unit)
  , onInit   :: Maybe (EChartsInstance -> Effect Unit)
  , session  :: Session
  )

render :: PredefinedChart -> Record Params -> R.Element
render CDocsHistogram { boxes, corpusId, listId, session, onClick, onInit } =
  histo { boxes, path, session, onClick, onInit }
  where
    path = { corpusId
           , listId: fromMaybe 0 listId
           , limit: Nothing
           , tabType: TabCorpus TabDocs
           }
render CAuthorsPie { boxes, corpusId, listId, session, onClick, onInit } =
  pie { boxes, path, session, onClick, onInit }
  where
    path = { corpusId
           , listId: fromMaybe 0 listId
           , limit: Nothing
           , tabType: TabCorpus (TabNgramType $ modeTabType Authors)
           }
render CInstitutesTree { boxes, corpusId, limit, listId, session, onClick, onInit } =
  tree { boxes, path, session, onClick, onInit }
  where
    path = { corpusId
           , limit
           , listId: fromMaybe 0 listId
           , tabType: TabCorpus (TabNgramType $ modeTabType Institutes)
           }
render CTermsMetrics { boxes, corpusId, limit, listId, session, onClick, onInit } =
  metrics { boxes, path, session, onClick, onInit }
  where
    path = { corpusId
           , limit
           , listId: fromMaybe 0 listId
           , tabType: TabCorpus (TabNgramType $ modeTabType Terms)
           }
render CSourcesBar { boxes, corpusId, limit, listId, session, onClick, onInit } =
  bar { boxes, path, session, onClick, onInit }
  where
    path = { corpusId
           , limit
           , listId: fromMaybe 0 listId
           , tabType: TabCorpus (TabNgramType $ modeTabType Sources)
           }
