module Gargantext.Components.PhyloExplorer.JSON where

import Gargantext.Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep as GR
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Gargantext.Utils.SimpleJSON (untaggedSumRep)
import Simple.JSON as JSON


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

newtype PhyloJSONSet = PhyloJSONSet
  { _subgraph_cnt     :: Int
  , directed          :: Boolean
  , edges             :: Array RawEdge
  , objects           :: Array RawObject
  , strict            :: Boolean
  | GraphData
  }

derive instance Generic PhyloJSONSet _
derive instance Eq PhyloJSONSet
instance Show PhyloJSONSet where show = genericShow
derive newtype instance JSON.ReadForeign PhyloJSONSet

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
    , seaLvl          :: String
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
