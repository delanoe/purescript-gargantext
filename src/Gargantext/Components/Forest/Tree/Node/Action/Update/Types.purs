module Gargantext.Components.Forest.Tree.Node.Action.Update.Types where

import Gargantext.Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Gargantext.Components.PhyloExplorer.API as Phylo
import Gargantext.Types as GT
import Simple.JSON as JSON
import Simple.JSON.Generics as JSONG


data UpdateNodeParams
  = UpdateNodeParamsList  { methodList  :: Method      }
  | UpdateNodeParamsGraph { methodGraphMetric :: GraphMetric
                          , methodGraphEdgesStrength :: Strength
                          , methodGraphClustering :: PartitionMethod
                          , methodGraphBridgeness :: BridgenessMethod
                          , methodGraphNodeType1  :: GT.CTabNgramType
                          , methodGraphNodeType2  :: GT.CTabNgramType
                          }
  | UpdateNodeParamsTexts { methodTexts :: Granularity }
  | UpdateNodeParamsBoard { methodBoard :: Charts      }
  | UpdateNodeParamsPhylo { methodPhylo :: Phylo.UpdateData }
  | UpdateNodeParamsLink  { methodLink  :: LinkNodeReq }
derive instance Eq UpdateNodeParams
derive instance Generic UpdateNodeParams _
instance Show UpdateNodeParams where show = genericShow
instance JSON.ReadForeign UpdateNodeParams where readImpl = JSONG.untaggedSumRep
instance JSON.WriteForeign UpdateNodeParams where
  writeImpl (UpdateNodeParamsList { methodList }) =
    JSON.writeImpl { type: "UpdateNodeParamsList"
                   , methodList }
  writeImpl (UpdateNodeParamsGraph { methodGraphMetric, methodGraphClustering, methodGraphBridgeness, methodGraphEdgesStrength, methodGraphNodeType1, methodGraphNodeType2}) =
    JSON.writeImpl { type: "UpdateNodeParamsGraph"
                   , methodGraphMetric, methodGraphClustering, methodGraphBridgeness, methodGraphEdgesStrength, methodGraphNodeType1, methodGraphNodeType2}
  writeImpl (UpdateNodeParamsTexts { methodTexts }) =
    JSON.writeImpl { type: "UpdateNodeParamsTexts"
                   , methodTexts }
  writeImpl (UpdateNodeParamsBoard { methodBoard }) =
    JSON.writeImpl { type: "UpdateNodeParamsBoard"
                   , methodBoard }
  writeImpl (UpdateNodeParamsPhylo { methodPhylo }) =
    JSON.writeImpl { type: "UpdateNodePhylo"
                   , config: methodPhylo }
  writeImpl (UpdateNodeParamsLink { methodLink: LinkNodeReq { id, nodeType }}) =
    JSON.writeImpl { type: "LinkNodeReq"
                   , id
                   , nodeType
                   }

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

data Strength = Strong | Weak
derive instance Generic Strength _
derive instance Eq Strength
instance Show Strength where show = genericShow
instance Read Strength where
  read "Strong"  = Just Strong
  read "Weak"    = Just Weak
  read _         = Nothing
instance JSON.ReadForeign  Strength where readImpl = JSONG.enumSumRep
instance JSON.WriteForeign Strength where writeImpl = JSON.writeImpl <<< show



data PartitionMethod = Spinglass | Infomap | Confluence
derive instance Generic PartitionMethod _
derive instance Eq PartitionMethod
instance Show PartitionMethod where show = genericShow
instance Read PartitionMethod where
  read "Spinglass"  = Just Spinglass
  read "Confluence" = Just Confluence
  read "Infomap"    = Just Infomap
  read _           = Nothing
instance JSON.ReadForeign PartitionMethod where readImpl = JSONG.enumSumRep
instance JSON.WriteForeign PartitionMethod where writeImpl = JSON.writeImpl <<< show


data BridgenessMethod = BridgenessMethod_Basic
                      | BridgenessMethod_Advanced

derive instance Generic BridgenessMethod _
derive instance Eq BridgenessMethod
instance Show BridgenessMethod where show = genericShow
instance Read BridgenessMethod where
  read "BridgenessMethod_Basic"     = Just BridgenessMethod_Basic
  read "BridgenessMethod_Advanced"  = Just BridgenessMethod_Advanced
  read _           = Nothing
instance JSON.ReadForeign BridgenessMethod where readImpl = JSONG.enumSumRep
instance JSON.WriteForeign BridgenessMethod where writeImpl = JSON.writeImpl <<< show



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

----------------------------------------------------------------------

newtype LinkNodeReq = LinkNodeReq { nodeType :: GT.NodeType, id :: GT.ID }
derive instance Eq LinkNodeReq
derive instance Generic LinkNodeReq _
instance Show LinkNodeReq where show = genericShow
derive newtype instance JSON.ReadForeign LinkNodeReq
derive newtype instance JSON.WriteForeign LinkNodeReq
