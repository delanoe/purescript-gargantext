module Gargantext.Components.Forest.Tree.Node.Action.Update.Types where

import Data.Argonaut as Argonaut
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Gargantext.Utils.Argonaut (genericSumDecodeJson, genericSumEncodeJson, genericEnumDecodeJson, genericEnumEncodeJson)
import Data.Maybe (Maybe(..))
import Gargantext.Prelude (class Eq, class Read, class Show)


data UpdateNodeParams = UpdateNodeParamsList  { methodList  :: Method      }
                      | UpdateNodeParamsGraph { methodGraph :: GraphMetric }
                      | UpdateNodeParamsTexts { methodTexts :: Granularity }
                      | UpdateNodeParamsBoard { methodBoard :: Charts      }

derive instance eqUpdateNodeParams :: Eq UpdateNodeParams

derive instance genericUpdateNodeParams :: Generic UpdateNodeParams _

instance showUpdateNodeParams :: Show UpdateNodeParams where
  show = genericShow

instance decodeJsonUpdateNodeParams :: Argonaut.DecodeJson UpdateNodeParams where
  decodeJson = genericSumDecodeJson

instance encodeJsonUpdateNodeParams :: Argonaut.EncodeJson UpdateNodeParams where
  encodeJson = genericSumEncodeJson

----------------------------------------------------------------------
data Method = Basic | Advanced | WithModel

derive instance genericMethod :: Generic Method _

derive instance eqMethod :: Eq Method

instance showMethod :: Show Method where
  show = genericShow

instance readMethod :: Read Method where
  read "Basic"    = Just Basic
  read "Advanced" = Just Advanced
  read "WithModel" = Just WithModel
  read _           = Nothing

instance decodeJsonMethod :: Argonaut.DecodeJson Method where
  decodeJson = genericEnumDecodeJson

instance encodeJsonMethod :: Argonaut.EncodeJson Method where
  encodeJson = genericEnumEncodeJson

----------------------------------------------------------------------
data GraphMetric = Order1 | Order2

derive instance genericGraphMetric :: Generic GraphMetric _

derive instance eqGraphMetric :: Eq GraphMetric

instance showGraphMetric :: Show GraphMetric where
  show = genericShow

instance readGraphMetric :: Read GraphMetric where
  read "Order1"    = Just Order1
  read "Order2"    = Just Order2
  read _           = Nothing

instance decodeJsonGraphMetric :: Argonaut.DecodeJson GraphMetric where
  decodeJson = genericEnumDecodeJson

instance encodeJsonGraphMetric :: Argonaut.EncodeJson GraphMetric where
  encodeJson = genericEnumEncodeJson

----------------------------------------------------------------------

data Granularity = NewNgrams | NewTexts | Both

derive instance genericGranularity :: Generic Granularity _

derive instance eqGranularity :: Eq Granularity

instance showGranularity :: Show Granularity where
  show = genericShow

instance readGranularity :: Read Granularity where
  read "NewNgrams" = Just NewNgrams
  read "NewTexts"  = Just NewTexts
  read "Both"      = Just Both
  read _           = Nothing

instance decodeJsonGranularity :: Argonaut.DecodeJson Granularity where
  decodeJson = genericEnumDecodeJson

instance encodeJsonGranularity :: Argonaut.EncodeJson Granularity where
  encodeJson = genericEnumEncodeJson


----------------------------------------------------------------------
data Charts = Sources | Authors | Institutes | Ngrams | All

derive instance genericChart :: Generic Charts _

derive instance eqChart :: Eq Charts

instance showChart :: Show Charts where
  show = genericShow

instance readChart :: Read Charts where
  read "Sources "   = Just Sources
  read "Authors"    = Just Authors
  read "Institutes" = Just Institutes
  read "Ngrams"     = Just Ngrams
  read "AllCharts"  = Just All
  read _            = Nothing

instance decodeJsonChart :: Argonaut.DecodeJson Charts where
  decodeJson = genericEnumDecodeJson

instance encodeJsonChart :: Argonaut.EncodeJson Charts where
  encodeJson = genericEnumEncodeJson


