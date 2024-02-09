module Gargantext.Components.PhyloExplorer.JSON
  ( PhyloJSON(..)
  , GraphData(..)
  , NodeData(..), RawObject(..)
  , EdgeData(..), RawEdge(..)
  , Cluster(..), PhyloLabel(..)
  , Sort(..), Synchrony(..)
  , SeaElevation(..), PhyloSimilarity(..)
  , TimeUnit(..), ConfigData(..)
  , Quality(..)
  ) where

import Gargantext.Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep as GR
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Gargantext.Utils.SimpleJSON (untaggedSumRep)
import Simple.JSON as JSON


newtype PhyloJSON = PhyloJSON
  { pd_corpusId       :: Int
  , pd_listId         :: Int
  , pd_data           ::
      { _subgraph_cnt     :: Int
      , directed          :: Boolean
      , edges             :: Array RawEdge
      , objects           :: Array RawObject
      , strict            :: Boolean
      | GraphData
      }
  , pd_config         :: 
      { clique            :: Cluster
      , exportLabel       :: Array PhyloLabel
      , exportSort        :: Sort
      , phyloSynchrony    :: Synchrony
      , phyloQuality      :: Quality
      , seaElevation      :: SeaElevation
      , similarity        :: PhyloSimilarity
      , timeUnit          :: TimeUnit
      | ConfigData
      }
  }

derive instance Generic PhyloJSON _
derive instance Eq PhyloJSON
instance Show PhyloJSON where show = genericShow
derive newtype instance JSON.ReadForeign PhyloJSON

--------------------------------------------------

type GraphData =
  ( bb                :: String
  , color             :: String
  , fontsize          :: String
  , label             :: String
  , labelloc          :: String
  , lheight           :: String
  , lp                :: String
  , lwidth            :: String
  , name              :: String
  , nodesep           :: String
  , overlap           :: String
  , phyloBranches     :: String
  , phyloDocs         :: String
  , phyloFoundations  :: String
  , phyloGroups       :: String
  , phyloPeriods      :: String
  , phyloSources      :: String
  , phyloTerms        :: String
  , phyloTimeScale    :: String
  , rank              :: String
  , ranksep           :: String
  , ratio             :: String
  , splines           :: String
  , style             :: String
  )

--------------------------------------------------

type NodeData =
  ( height            :: String
  , label             :: String
  , name              :: String
  , nodeType          :: String
  , pos               :: String
  , shape             :: String
  , width             :: String
  )

data RawObject
  = GroupToNode
    { _gvid           :: Int
    , bId             :: String
    , branchId        :: String
    , fontname        :: String
    , foundation      :: String
    , frequence       :: String
    , from            :: String
    , lbl             :: String
    , penwidth        :: String
    , role            :: String
    -- @NOTE #219: not in API; but present in certain data (eg. "Knowledge
    --             visualisation")
    , seaLvl          :: Maybe String
    , source          :: String
    , strFrom         :: Maybe String
    , strTo           :: Maybe String
    , support         :: String
    , to              :: String
    , weight          :: String
    | NodeData
    }
  | BranchToNode
    { _gvid           :: Int
    , age             :: String
    , bId             :: String
    , birth           :: String
    , branchId        :: String
    , branch_x        :: String
    , branch_y        :: String
    , fillcolor       :: String
    , fontname        :: String
    , fontsize        :: String
    , size            :: String
    , style           :: String
    | NodeData
    }
  | PeriodToNode
    { _gvid           :: Int
    , fontsize        :: String
    , from            :: String
    , strFrom         :: Maybe String
    , strTo           :: Maybe String
    , to              :: String
    | NodeData
    }
  | Layer
    { _gvid           :: Int
    , nodes           :: Array Int
    | GraphData
    }


derive instance Generic RawObject _
derive instance Eq RawObject
instance Show RawObject where show = genericShow
instance JSON.ReadForeign RawObject where
  readImpl f = GR.to <$> untaggedSumRep f

--------------------------------------------------

type EdgeData =
  ( color           :: String
  , head            :: Int
  , pos             :: String
  , tail            :: Int
  , width           :: String
  )

data RawEdge
  = GroupToAncestor
    { _gvid         :: Int
    , arrowhead     :: String
    , edgeType      :: String
    , lbl           :: String
    , penwidth      :: String
    , style         :: String
    | EdgeData
    }
  | GroupToGroup
    { _gvid         :: Int
    , constraint    :: String
    , edgeType      :: String
    , lbl           :: String
    , penwidth      :: String
    | EdgeData
    }
  | BranchToGroup
    { _gvid         :: Int
    , arrowhead     :: String
    , edgeType      :: String
    | EdgeData
    }
  | BranchToBranch
    { _gvid         :: Int
    , arrowhead     :: String
    , style         :: String
    | EdgeData
    }
  | PeriodToPeriod
    { _gvid         :: Int
    | EdgeData
    }

derive instance Generic RawEdge _
derive instance Eq RawEdge
instance Show RawEdge where show = genericShow
instance JSON.ReadForeign RawEdge where
  readImpl f = GR.to <$> untaggedSumRep f

--------------------------------------------------

type ConfigData =
  ( corpusPath    :: String
  , defaultMode   :: Boolean
  , findAncestors :: Boolean
  , listParser    :: String
  , listPath      :: String
  , outputPath    :: String
  , phyloName     :: String
  , phyloScale    :: Int
  )

data Cluster 
  = Fis
    { _fis_support :: Int
    , _fis_size    :: Int 
    }
  | MaxClique
    { _mcl_size      :: Int
    , _mcl_threshold :: Number
    , _mcl_filter    :: String 
    }

derive instance Generic Cluster _
derive instance Eq Cluster 
instance Show Cluster  where show = genericShow
instance JSON.ReadForeign Cluster where
  readImpl f = GR.to <$> untaggedSumRep f

data PhyloLabel 
  = BranchLabel
    { _branch_labelTagger :: String
    , _branch_labelSize   :: Int 
    }
  | GroupLabel
    { _group_labelTagger  :: String
    , _group_labelSize    :: Int 
    }

derive instance Generic PhyloLabel _
derive instance Eq PhyloLabel 
instance Show PhyloLabel  where show = genericShow
instance JSON.ReadForeign PhyloLabel  where
  readImpl f = GR.to <$> untaggedSumRep f

data Sort = ByBirthDate { _sort_order :: String } | ByHierarchy {_sort_order :: String }

derive instance Generic Sort _
derive instance Eq Sort 
instance Show Sort  where show = genericShow
instance JSON.ReadForeign Sort  where
  readImpl f = GR.to <$> untaggedSumRep f

data Synchrony 
  = ByProximityThreshold
    { _bpt_threshold   :: Number
    , _bpt_sensibility :: Number
    , _bpt_scope       :: String
    , _bpt_strategy    :: String
    }
  | ByProximityDistribution
    { _bpd_sensibility :: Number
    , _bpd_strategy    :: String
    }

derive instance Generic Synchrony _
derive instance Eq Synchrony  
instance Show Synchrony where show = genericShow
instance JSON.ReadForeign Synchrony where
  readImpl f = GR.to <$> untaggedSumRep f


data SeaElevation 
  = Constante
    { _cons_start :: Number
    , _cons_gap   :: Number
    }
  | Adaptative
    { _adap_steps :: Number }
  | Evolving
    { _evol_neighborhood :: Boolean }

derive instance Generic SeaElevation _
derive instance Eq SeaElevation  
instance Show SeaElevation where show = genericShow
instance JSON.ReadForeign SeaElevation where
  readImpl f = GR.to <$> untaggedSumRep f

data PhyloSimilarity 
  = WeightedLogJaccard
    { _wlj_sensibility     :: Number
    , _wlj_minSharedNgrams :: Int
    }
  | WeightedLogSim
    { _wls_sensibility     :: Number
    , _wls_minSharedNgrams :: Int
    }
  | Hamming
    { _hmg_sensibility     :: Number
    , _hmg_minSharedNgrams :: Int
    }

derive instance Generic PhyloSimilarity _
derive instance Eq PhyloSimilarity  
instance Show PhyloSimilarity where show = genericShow
instance JSON.ReadForeign PhyloSimilarity where
  readImpl f = GR.to <$> untaggedSumRep f

data TimeUnit 
  = Epoch
    { _epoch_period :: Int
    , _epoch_step   :: Int
    , _epoch_matchingFrame :: Int
    }
  | Year
    { _year_period :: Int
    , _year_step   :: Int
    , _year_matchingFrame :: Int
    }
  | Month
    { _month_period :: Int
    , _month_step   :: Int
    , _month_matchingFrame :: Int
    }
  | Week
    { _week_period :: Int
    , _week_step   :: Int
    , _week_matchingFrame :: Int
    }
  | Day
    { _day_period :: Int
    , _day_step   :: Int
    , _day_matchingFrame :: Int
    }

derive instance Generic TimeUnit _
derive instance Eq TimeUnit  
instance Show TimeUnit where show = genericShow
instance JSON.ReadForeign TimeUnit where
  readImpl f = GR.to <$> untaggedSumRep f

data Quality 
  = Quality 
    { _qua_granularity :: Number
    , _qua_minBranch   :: Int 
    }

derive instance Generic Quality _
derive instance Eq Quality 
instance Show Quality where show = genericShow
instance JSON.ReadForeign Quality where
  readImpl f = GR.to <$> untaggedSumRep f
