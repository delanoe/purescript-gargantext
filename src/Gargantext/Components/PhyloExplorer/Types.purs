module Gargantext.Components.PhyloExplorer.Types where

import Gargantext.Prelude
import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Show.Generic (genericShow)
import Simple.JSON as JSON
import Simple.JSON.Generics (enumSumRep)
import Simple.JSON.Generics as JSONG

-- // Generics


--------------------------------------------------

-- type PhyloJSON =
--   { name              :: String
--   , phyloDocs         :: String
--   , phyloFoundations  :: String
--   , phyloPeriods      :: String
--   , phyloTerms        :: String
--   , phyloGroups       :: String
--   , phyloBranches     :: String
--   , objects           :: Array PhyloObject
--   , edges             :: Array PhyloEdge
--   }

newtype PhyloDataSet = PhyloDataSet
  { name :: String
  , edges :: Array PhyloEdge
  }


derive instance Generic PhyloDataSet _
derive instance Newtype PhyloDataSet _
instance Show PhyloDataSet where show = genericShow
derive newtype instance JSON.ReadForeign PhyloDataSet
--------------------------------------------------

-- data PhyloObject =
--     PhyloBranch
--     { _gvid       :: Int
--     , bId         :: String
--     , branch_x    :: String
--     , branch_y    :: String
--     , label       :: String
--     , nodeType    :: String
--     , pos         :: String
--     }
--   | PhyloGroup
--     { _gvid       :: Int
--     , bId         :: String
--     , foundation  :: String
--     , from        :: String
--     , lbl         :: String
--     , nodeType    :: String
--     , pos         :: String
--     , role        :: String
--     , support     :: String
--     , to          :: String
--     , weight      :: String
--     }
--   | PhyloPeriod
--     { _gvid       :: Int
--     , nodeType    :: String
--     }
--   | DefaultObject
--     { _gvid       :: Int
--     }

-- derive instance Generic PhyloObject _
-- instance Eq PhyloObject where eq = genericEq

-- instance showPhyloObject :: Show PhyloObject where
--   show (PhyloBranch         { nodeType }) = nodeType
--   show (PhyloGroup          { nodeType }) = nodeType
--   show (PhyloPeriod         { nodeType }) = nodeType
--   show (DefaultObject       { _gvid })    = "DefaultNode"

--------------------------------------------------

data EdgeType = Link | BranchLink | AncestorLink | DefaultEdge

derive instance Generic EdgeType _
instance Eq EdgeType where eq = genericEq
instance Show EdgeType where show = genericShow
-- @TODO use `enumSumRep` -> input value with lowercased first char
instance JSON.ReadForeign EdgeType where
  readImpl v = JSON.readImpl v >>= pure <<< case _ of
      Just "link"         -> Link
      Just "branchLink"   -> BranchLink
      Just "ancestorLink" -> AncestorLink
      _                   -> DefaultEdge

newtype PhyloEdge = PhyloEdge
  { _gvid    :: Int
  , color :: String
  , edgeType :: EdgeType
  , head :: Int
  , pos :: String
  , tail :: Int
  , width :: String

  , arrowhead :: Maybe String
  , constraint :: Maybe String
  , lbl :: Maybe String
  , penwidth :: Maybe String
  }

derive instance Generic PhyloEdge _
derive instance Newtype PhyloEdge _
instance Show PhyloEdge where show = genericShow
derive newtype instance JSON.ReadForeign PhyloEdge



-- arrowhead: "rdot"
-- color: "black"
-- edgeType: "branchLink"
-- head: 309
-- pos: "e,63066,5862.7 62942,34687 62953,34667 62967,34640 62975,34615 63059,34351 63066,34275 63066,33998 63066,33998 63066,33998 63066,7485.5 63066,6917.8 63066,6256.7 63066,5870.9"
-- tail: 86
-- width: "3"
-- _gvid: 49

-- color: "black"
-- head: 90
-- pos: "e,64286,35436 64286,35508 64286,35489 64286,35466 64286,35446"
-- tail: 87
-- width: "5"
-- _gvid: 50

-- color: "black"
-- constraint: "true"
-- edgeType: "link"
-- head: 101
-- lbl: "1.0"
-- penwidth: "4"
-- pos: "e,22565,33307 22565,33379 22565,33358 22565,33338 22565,33317"
-- tail: 89
-- width: "3"
-- _gvid: 52
