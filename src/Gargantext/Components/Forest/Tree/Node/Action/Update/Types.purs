module Gargantext.Components.Forest.Tree.Node.Action.Update.Types where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))
import Simple.JSON as JSON
import Simple.JSON.Generics as JSONG

import Gargantext.Prelude


data UpdateNodeParams = UpdateNodeParamsList  { methodList  :: Method      }
                      | UpdateNodeParamsGraph { methodGraph :: GraphMetric }
                      | UpdateNodeParamsTexts { methodTexts :: Granularity }
                      | UpdateNodeParamsBoard { methodBoard :: Charts      }
derive instance Eq UpdateNodeParams
derive instance Generic UpdateNodeParams _
instance Show UpdateNodeParams where show = genericShow
instance JSON.ReadForeign UpdateNodeParams where readImpl = JSONG.untaggedSumRep
instance JSON.WriteForeign UpdateNodeParams where
  writeImpl (UpdateNodeParamsList { methodList }) =
    JSON.writeImpl { type: "UpdateNodeParamsList"
                   , values: methodList }
  writeImpl (UpdateNodeParamsGraph { methodGraph }) =
    JSON.writeImpl { type: "UpdateNodeParamsGraph"
                   , methodGraph }
  writeImpl (UpdateNodeParamsTexts { methodTexts }) =
    JSON.writeImpl { type: "UpdateNodeParamsTexts"
                   , methodTexts }
  writeImpl (UpdateNodeParamsBoard { methodBoard }) =
    JSON.writeImpl { type: "UpdateNodeParamsBoard"
                   , methodBoard }

----------------------------------------------------------------------
data Method = Basic | Advanced | WithModel
derive instance Generic Method _
derive instance Eq Method
instance Show Method where show = genericShow
instance JSON.ReadForeign Method where readImpl = JSONG.enumSumRep
instance JSON.WriteForeign Method where writeImpl = JSON.writeImpl <<< show
instance Read Method where
  read "Basic"    = Just Basic
  read "Advanced" = Just Advanced
  read "WithModel" = Just WithModel
  read _           = Nothing

----------------------------------------------------------------------
data GraphMetric = Order1 | Order2
derive instance Generic GraphMetric _
derive instance Eq GraphMetric
instance Show GraphMetric where show = genericShow
instance Read GraphMetric where
  read "Order1"    = Just Order1
  read "Order2"    = Just Order2
  read _           = Nothing
instance JSON.ReadForeign GraphMetric where readImpl = JSONG.enumSumRep
instance JSON.WriteForeign GraphMetric where writeImpl = JSON.writeImpl <<< show
                                        
----------------------------------------------------------------------

data Granularity = NewNgrams | NewTexts | Both
derive instance Generic Granularity _
derive instance Eq Granularity
instance Show Granularity where show = genericShow
instance Read Granularity where
  read "NewNgrams" = Just NewNgrams
  read "NewTexts"  = Just NewTexts
  read "Both"      = Just Both
  read _           = Nothing
instance JSON.ReadForeign Granularity where readImpl = JSONG.enumSumRep
instance JSON.WriteForeign Granularity where writeImpl = JSON.writeImpl <<< show

----------------------------------------------------------------------
data Charts = Sources | Authors | Institutes | Ngrams | All
derive instance Generic Charts _
derive instance Eq Charts
instance Show Charts where show = genericShow
instance Read Charts where
  read "Sources "   = Just Sources
  read "Authors"    = Just Authors
  read "Institutes" = Just Institutes
  read "Ngrams"     = Just Ngrams
  read "AllCharts"  = Just All
  read _            = Nothing
instance JSON.ReadForeign Charts where readImpl = JSONG.enumSumRep
instance JSON.WriteForeign Charts where
  writeImpl All = JSON.writeImpl $ "AllCharts"
  writeImpl f = JSON.writeImpl $ show f

