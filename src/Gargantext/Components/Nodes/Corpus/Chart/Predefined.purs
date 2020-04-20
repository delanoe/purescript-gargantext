module Gargantext.Components.Nodes.Corpus.Chart.Predefined where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe, fromMaybe)
import Reactix as R
import Gargantext.Prelude

import Gargantext.Components.Nodes.Corpus.Chart.Histo (histo)
import Gargantext.Components.Nodes.Corpus.Chart.Metrics (metrics)
import Gargantext.Components.Nodes.Corpus.Chart.Pie (pie)
import Gargantext.Components.Nodes.Corpus.Chart.Tree (tree)
import Gargantext.Sessions (Session)
import Gargantext.Types (NodeID, Mode(..), TabSubType(..), TabType(..), modeTabType)


data PredefinedChart =
    CDocsHistogram
  | CAuthorsPie
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
    pure $ readChart' obj
instance encodePredefinedChart :: EncodeJson PredefinedChart where
  encodeJson c = encodeJson $ show c

readChart' :: String -> PredefinedChart
readChart' "CDocsHistogram" = CDocsHistogram
readChart' "CAuthorsPie" = CAuthorsPie
readChart' "CInstitutesTree" = CInstitutesTree
readChart' "CTermsMetrics" = CTermsMetrics
readChart' _ = CDocsHistogram


allPredefinedCharts :: Array PredefinedChart
allPredefinedCharts = [
    CDocsHistogram
  , CAuthorsPie
  , CTermsMetrics
  , CInstitutesTree
  ]


type Params =
  (
    corpusId :: NodeID
  , session :: Session
  -- optinal params
  , limit :: Maybe Int
  , listId :: Maybe Int
  )


render :: PredefinedChart -> Record Params -> R.Element
render CDocsHistogram { corpusId, session } = histo { path, session }
  where
    path = { corpusId
           , tabType: TabCorpus TabDocs
           }
render CAuthorsPie { corpusId, session } = pie { path, session }
  where
    path = { corpusId
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
           , tabType: TabCorpus (TabNgramType $ modeTabType Authors)
           }
