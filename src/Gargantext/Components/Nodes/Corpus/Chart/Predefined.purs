module Gargantext.Components.Nodes.Corpus.Chart.Predefined where

import Gargantext.Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable)
import Effect (Effect)
import Gargantext.Components.Charts.Options.Type (EChartsInstance, MouseEvent)
import Gargantext.Components.Nodes.Corpus.Chart.Histo (histo)
import Gargantext.Components.Nodes.Corpus.Chart.Metrics (metrics)
import Gargantext.Components.Nodes.Corpus.Chart.Pie (pie)
import Gargantext.Components.Nodes.Corpus.Chart.Tree (tree)
import Gargantext.Sessions (Session)
import Gargantext.Types (NodeID, Mode(..), TabSubType(..), TabType(..), modeTabType)
import Reactix as R


data PredefinedChart =
    CDocsHistogram
  | CAuthorsPie
  | CSourcesBar
  | CInstitutesTree
  | CTermsMetrics

derive instance genericPredefinedChart :: Generic PredefinedChart _

instance showPredefinedChart :: Show PredefinedChart where
  show = genericShow

derive instance eqPredefinedChart :: Eq PredefinedChart

instance ordPredefinedChart :: Ord PredefinedChart where
  compare = genericCompare

instance decodePredefinedChart :: DecodeJson PredefinedChart where
  decodeJson json = do
    obj <- decodeJson json
    pure $ fromMaybe CDocsHistogram $ read obj

instance encodePredefinedChart :: EncodeJson PredefinedChart where
  encodeJson c = encodeJson $ show c

instance readPredefinedChart :: Read PredefinedChart where
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
  , onClick  :: Maybe (MouseEvent -> Effect Unit)
  , onInit   :: Maybe (EChartsInstance -> Effect Unit)
  )

render :: PredefinedChart -> Record Params -> R.Element
render CDocsHistogram { corpusId, listId, session, onClick, onInit } = histo { path, session, onClick, onInit }
  where
    path = { corpusId
           , listId: fromMaybe 0 listId
           , limit: Nothing
           , tabType: TabCorpus TabDocs
           }
render CAuthorsPie { corpusId, listId, session, onClick, onInit } = pie { path, session, onClick, onInit }
  where
    path = { corpusId
           , listId: fromMaybe 0 listId
           , limit: Nothing
           , tabType: TabCorpus (TabNgramType $ modeTabType Authors)
           }
render CInstitutesTree { corpusId, limit, listId, session, onClick, onInit } = tree { path, session, onClick, onInit }
  where
    path = { corpusId
           , limit
           , listId: fromMaybe 0 listId
           , tabType: TabCorpus (TabNgramType $ modeTabType Institutes)
           }
render CTermsMetrics { corpusId, limit, listId, session, onClick, onInit } = metrics { path, session, onClick, onInit }
  where
    path = { corpusId
           , limit
           , listId: fromMaybe 0 listId
           , tabType: TabCorpus (TabNgramType $ modeTabType Terms)
           }
render CSourcesBar { corpusId, limit, listId, session, onClick, onInit } = metrics { path, session, onClick, onInit }
  where
    path = { corpusId
           , limit
           , listId: fromMaybe 0 listId
           , tabType: TabCorpus (TabNgramType $ modeTabType Sources)
           }
