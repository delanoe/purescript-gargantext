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
  , edges             :: Array Edge
  , objects           :: Array PhyloObject
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

data PhyloObject
  = Layer
    { _gvid           :: Int
    , nodes           :: Array Int
    | GraphData
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
  | GroupToNode
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
  | PeriodToNode
    { _gvid           :: Int
    , fontsize        :: String
    , from            :: String
    , strFrom         :: Maybe String
    , strTo           :: Maybe String
    , to              :: String
    | NodeData
    }

derive instance Generic PhyloObject _
derive instance Eq PhyloObject
instance Show PhyloObject where show = genericShow
instance JSON.ReadForeign PhyloObject where
  readImpl f = GR.to <$> untaggedSumRep f


--------------------------------------------------

type EdgeData =
  ( color           :: String
  , head            :: Int
  , pos             :: String
  , tail            :: Int
  , width           :: String
  )

data Edge
  = GroupToGroup
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
  | GroupToAncestor
    { _gvid         :: Int
    , arrowhead     :: String
    , lbl           :: String
    , penwidth      :: String
    , style         :: String
    | EdgeData
    }
  | PeriodToPeriod
    { _gvid         :: Int
    | EdgeData
    }

derive instance Generic Edge _
derive instance Eq Edge
instance Show Edge where show = genericShow
instance JSON.ReadForeign Edge where
  readImpl f = GR.to <$> untaggedSumRep f
