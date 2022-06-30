module Gargantext.Components.PhyloExplorer.Types
  ( PhyloSet(..), parseToPhyloSet
  , CorpusId, ListId, DocId
  , PhyloData(..)
  , Branch(..), Period(..), Group(..)
  , Link(..), AncestorLink(..), BranchLink(..)
  , Term(..)
  , Source(..)
  , sortSources
  , DisplayView(..)
  , TabView(..)
  , ExtractedTerm(..)
  , ExtractedCount(..)
  , FrameDoc(..)
  , CacheParams(..), defaultCacheParams
  ) where

import Gargantext.Prelude

import Data.Array as Array
import Data.Date as Date
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Number as Number
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String.Extra (camelCase)
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Gargantext.Components.PhyloExplorer.JSON (PhyloJSON(..), RawEdge(..), RawObject(..))
import Simple.JSON as JSON

-- @NOTE #219: PureScript Date or stick to JavaScript foreign?
foreign import yearToDate       :: String -> Date.Date
foreign import stringToDate     :: String -> Date.Date
foreign import utcStringToDate  :: String -> Date.Date

type CorpusId = Int
type ListId   = Int
type DocId    = Int

------------------------------------------------------------------

data PhyloSet = PhyloSet
  { corpusId  :: CorpusId
  , listId    :: ListId
  , phyloData :: PhyloData
  }

derive instance Generic PhyloSet _
derive instance Eq PhyloSet
instance Show PhyloSet where show = genericShow

parseToPhyloSet :: PhyloJSON -> PhyloSet
parseToPhyloSet (PhyloJSON o) = PhyloSet
  { corpusId  : o.pd_corpusId
  , listId    : o.pd_listId
  , phyloData : PhyloData
      { ancestorLinks
      , bb            : parseBB p.bb
      , branchLinks
      , branches
      , groups
      , links
      , name          : p.name
      , nbBranches    : parseInt p.phyloBranches
      -- @NOTE #219: remotely stringify as a Double instead of an Int (reason?)
      , nbDocs        : (parseFloat >>> parseInt') p.phyloDocs
      , nbFoundations : parseInt p.phyloFoundations
      , nbGroups      : parseInt p.phyloGroups
      , nbPeriods     : parseInt p.phyloPeriods
      , nbTerms       : parseInt p.phyloTerms
      , periods
      , sources       : parseSources p.phyloSources
      , timeScale     : p.phyloTimeScale
      , weighted      : getGlobalWeightedValue groups
      }
  }

  where
    p             = o.pd_data

    epochTS       = p.phyloTimeScale == "epoch"

    ancestorLinks = parseAncestorLinks p.edges
    branchLinks   = parseBranchLinks p.edges
    branches      = parseBranches p.objects
    groups        = parseGroups epochTS p.objects
    links         = parseLinks p.edges
    periods       = parsePeriods epochTS p.objects

----------------------------------------------------------------------

data PhyloData = PhyloData
  { ancestorLinks :: Array AncestorLink
  , bb            :: Array Number
  , branchLinks   :: Array BranchLink
  , branches      :: Array Branch
  , groups        :: Array Group
  , links         :: Array Link
  , name          :: String
  , nbBranches    :: Int
  , nbDocs        :: Int
  , nbFoundations :: Int
  , nbGroups      :: Int
  , nbPeriods     :: Int
  , nbTerms       :: Int
  , periods       :: Array Period
  , sources       :: Array String
  , timeScale     :: String
  , weighted      :: Boolean
  }

derive instance Generic PhyloData _
derive instance Eq PhyloData
instance Show PhyloData where show = genericShow

-----------------------------------------------------------

newtype Branch = Branch
  { bId     :: Int
  , gvid    :: Int
  , label   :: String
  , x1      :: String
  , x2      :: Number
  , y       :: String
  }

derive instance Generic Branch _
derive instance Eq Branch
instance Show Branch where show = genericShow

parseBranches :: Array RawObject -> Array Branch
parseBranches
  =   map parse
  >>> Array.catMaybes

  where
    parse :: RawObject -> Maybe Branch
    parse (BranchToNode o) = Just $ Branch
      { bId   : parseInt o.bId
      , gvid  : o._gvid
      , label : parseLabel o.label
      , x1    : o.branch_x
      , x2    : Tuple.fst $ parsePos o.pos
      , y     : o.branch_y
      }
    parse _                = Nothing

    parseLabel :: String -> String
    parseLabel =
      String.replaceAll (String.Pattern "\"") (String.Replacement "")

-----------------------------------------------------------

newtype Period = Period
  { from    :: Date.Date
  , to      :: Date.Date
  , y       :: Number
  }

derive instance Generic Period _
derive instance Eq Period
instance Show Period where show = genericShow

parsePeriods :: Boolean -> Array RawObject -> Array Period
parsePeriods epoch
  =   map parse
  >>> Array.catMaybes

  where
    parse :: RawObject -> Maybe Period
    parse (PeriodToNode o) = Just $ Period
      { from  : parseNodeDate o.strFrom o.from epoch
      , to    : parseNodeDate o.strTo   o.to   epoch
      , y     : Tuple.snd $ parsePos o.pos
      }
    parse _                = Nothing

-----------------------------------------------------------

newtype Group = Group
  { bId           :: Int
  , foundation    :: Array Int -- @NOTE #219: Array String ???
  , from          :: Date.Date
  , gId           :: Int
  , label         :: Array String
  , role          :: Array Int
  , size          :: Int
  , source        :: Array String
  , to            :: Date.Date
  , weight        :: Number
  , x             :: Number
  , y             :: Number
  }

derive instance Generic Group _
derive instance Eq Group
instance Show Group where show = genericShow

parseGroups :: Boolean -> Array RawObject -> Array Group
parseGroups epoch
  =   map parse
  >>> Array.catMaybes

  where
    parse :: RawObject -> Maybe Group
    parse (GroupToNode o) = Just $ Group
      { bId         : parseInt o.bId
      , foundation  : stringedArrayToArray' o.foundation
      , from        : parseNodeDate o.strFrom o.from epoch
      , gId         : o._gvid
      , label       : stringedArrayToArray o.lbl
      , role        : stringedArrayToArray_ o.role
      , size        : parseInt o.support
      , source      : parseSources o.source
      , to          : parseNodeDate o.strTo   o.to   epoch
      , weight      : stringedMaybeToNumber o.weight
      , x           : Tuple.fst $ parsePos o.pos
      , y           : Tuple.snd $ parsePos o.pos
      }
    parse _               = Nothing

-----------------------------------------------------------

newtype Link = Link
  { from    :: Int
  , lId     :: Int
  , label   :: String -- @NOTE #219: undefined in Mèmiescape v2, still needed?
  , to      :: Int
  }

derive instance Generic Link _
derive instance Eq Link
instance Show Link where show = genericShow

parseLinks :: Array RawEdge -> Array Link
parseLinks
  =   Array.filter filter
  >>> map parse
  >>> Array.catMaybes

  where
    -- @NOTE #219: necessary?
    --             bc. GroupToGroup as 1-1 relation with "edgeType=link"
    filter :: RawEdge -> Boolean
    filter (GroupToGroup o) = o.edgeType == "link"
    filter _                = false

    parse :: RawEdge -> Maybe Link
    parse (GroupToGroup o) = Just $ Link
      { from  : o.tail
      , lId   : o._gvid
      , label : ""
      , to    : o.head
      }
    parse _                = Nothing

-----------------------------------------------------------

newtype AncestorLink = AncestorLink
  { from    :: Int
  , lId     :: Int
  , label   :: String -- @NOTE #219: undefined in Mèmiescape v2, still needed?
  , to      :: Int
  }

derive instance Generic AncestorLink _
derive instance Eq AncestorLink
instance Show AncestorLink where show = genericShow

parseAncestorLinks :: Array RawEdge -> Array AncestorLink
parseAncestorLinks
  =   Array.filter filter
  >>> map parse
  >>> Array.catMaybes

  where
    -- @NOTE #219: necessary?
    --             bc. GroupToAncestor as 1-1 relation
    --             with "edgeType=ancestorLink"
    filter :: RawEdge -> Boolean
    filter (GroupToAncestor o) = o.edgeType == "ancestorLink"
    filter _                   = false

    parse :: RawEdge -> Maybe AncestorLink
    parse (GroupToAncestor o) = Just $ AncestorLink
      { from  : o.tail
      , lId   : o._gvid
      , label : ""
      , to    : o.head
      }
    parse _                   = Nothing

-----------------------------------------------------------

newtype BranchLink = BranchLink
  { from    :: Int
  , to      :: Int
  }

derive instance Generic BranchLink _
derive instance Eq BranchLink
instance Show BranchLink where show = genericShow

parseBranchLinks :: Array RawEdge -> Array BranchLink
parseBranchLinks
  =   Array.filter filter
  >>> map parse
  >>> Array.catMaybes

  where
    -- @NOTE #219: necessary?
    --             bc. BranchToGroup as 1-1 relation
    --             with "edgeType=branchLink"
    filter :: RawEdge -> Boolean
    filter (BranchToGroup o) = o.edgeType == "branchLink"
    filter _                 = false

    parse :: RawEdge -> Maybe BranchLink
    parse (BranchToGroup o) = Just $ BranchLink
      { from  : o.tail
      , to    : o.head
      }
    parse _                 = Nothing

-----------------------------------------------------------

newtype Term = Term
  { label :: String
  , fdt   :: String
  }

derive instance Newtype Term _
derive instance Generic Term _
derive instance Eq Term
instance Show Term where show = genericShow

-----------------------------------------------------------

newtype ExtractedTerm = ExtractedTerm
 { label :: String
 , freq  :: Number
 , ratio :: Number
 }

derive instance Newtype ExtractedTerm _
derive instance Generic ExtractedTerm _
derive instance Eq ExtractedTerm
instance Show ExtractedTerm where show = genericShow

-----------------------------------------------------------

newtype Source = Source
  { label :: String
  , id    :: Int
  }

derive instance Newtype Source _
derive instance Generic Source _
derive instance Eq Source
instance Show Source where show = genericShow

parseSources :: String -> Array String
parseSources
  =   String.replaceAll (String.Pattern "[") (String.Replacement "")
  >>> String.replaceAll (String.Pattern "]") (String.Replacement "")
  >>> String.replaceAll (String.Pattern "\"") (String.Replacement "")
  >>> String.split (String.Pattern ",")
  >>> Array.filter (\s -> not eq 0 $ String.length s)

-- @NOTE #219: `Resources.js` business's methods still use `source` as
--             an unsorted `Array String`, we have to dissociate the parsing
--             and sorting computation
--
--                ↳ Hence the below additionnal method to use for
--                  sorting purpose only
sortSources :: Array String -> Array Source
sortSources
  =   Array.mapWithIndex setSource
  >>> Array.sortWith getLabel

  where

    setSource :: Int -> String -> Source
    setSource id label = Source { id, label }

    getLabel :: Source -> String
    getLabel (Source { label }) = label

-----------------------------------------------------------

parseInt :: String -> Int
parseInt s = maybe 0 identity $ Int.fromString s

parseInt' :: Number -> Int
parseInt' n = maybe 0 identity $ Int.fromNumber n

parseFloat :: String -> Number
parseFloat s = maybe 0.0 identity $ Number.fromString s

parseBB :: String -> Array Number
parseBB
  =   String.split (String.Pattern ",")
  >>> map parseFloat

parseNodeDate :: Maybe String -> String -> Boolean -> Date.Date
-- parseNodeDate Nothing    year _     = yearToDate(year)
-- parseNodeDate (Just str) _    true  = utcStringToDate(str)
-- parseNodeDate (Just str) _    false = stringToDate(str)
-- @NOTE #219 ^ as soon as the issue regarding `Date` (< 1970) is resolved
--              please uncomment above lines + delete below one
parseNodeDate _ y _ = yearToDate(y)

parsePos :: String -> Tuple.Tuple Number Number
parsePos
  =   String.split (String.Pattern ",")
  >>> \a -> (p $ Array.index a 0) /\
            (p $ Array.index a 1)

  where
    p = case _ of
      Nothing -> 0.0
      Just s  -> parseFloat s


-- @NOTE #219: why taking last value? use `any`?
getGlobalWeightedValue :: Array Group -> Boolean
getGlobalWeightedValue
  =   Array.last
  >>> case _ of
        Nothing         -> false
        Just (Group o)  -> o.weight > 0.0

stringedMaybeToNumber :: String -> Number
stringedMaybeToNumber "Nothing" = 0.0
stringedMaybeToNumber s         =
      s # String.replace (String.Pattern "Just ") (String.Replacement "")
  >>> parseFloat

-- | From "\"user | sentiment analysis\"" :: String
-- |
-- | To   ["user", "sentiment analysis"] :: Array String
stringedArrayToArray :: String -> Array String
stringedArrayToArray str
  =   str # String.length
  >>> (\length     -> String.splitAt (length - 1) str)
  >>> (\{ before } -> String.splitAt 1 before)
  >>> (\{ after }  -> String.split (String.Pattern "|") after)
  >>> map String.trim

-- | From "\"97 | 257 | 542 | 574 | 577 | 597 | 785\"" :: String
-- |
-- | To   [97, 257, 542, 574, 577, 597, 785] :: Array Int
stringedArrayToArray' :: String -> Array Int
stringedArrayToArray'
  =   stringedArrayToArray
  >>> map parseInt

-- | From "\"3.0 | 3.0 | 3.0 | 3.0 | 1.0 | 3.0 | 3.0\"" :: String
-- |
-- | To   [3, 3, 3, 3, 1, 3, 3] :: Array Int
stringedArrayToArray_ :: String -> Array Int
stringedArrayToArray_
  =   stringedArrayToArray
  >>> map parseFloat
  >>> map parseInt'

-----------------------------------------------------------

data DisplayView
  = LabelMode
  | HeadingMode
  | LandingMode

derive instance Generic DisplayView _
derive instance Eq DisplayView
instance Show DisplayView where
  show = camelCase <<< genericShow

instance Read DisplayView where
  read :: String -> Maybe DisplayView
  read "labelMode"   = Just LabelMode
  read "headingMode" = Just HeadingMode
  read "landingMode" = Just LandingMode
  read _             = Nothing

-----------------------------------------------------------

data TabView
  = DetailsTab
  | SelectionTab

derive instance Generic TabView _
derive instance Eq TabView
instance Show TabView where
  show DetailsTab   = "Legend"
  show SelectionTab = "Data"

-----------------------------------------------------------

newtype ExtractedCount = ExtractedCount
 { groupCount   :: Int
 , branchCount  :: Int
 , termCount    :: Int
 }

derive instance Generic ExtractedCount _
derive instance Eq ExtractedCount
derive newtype instance JSON.ReadForeign ExtractedCount

-----------------------------------------------------------

newtype FrameDoc = FrameDoc
  { docId     :: DocId
  , corpusId  :: CorpusId
  , listId    :: ListId
  }

derive instance Newtype FrameDoc _
derive instance Generic FrameDoc _
derive instance Eq FrameDoc

----------------------------------------------------------------

newtype CacheParams = CacheParams
  { expandSelection     :: Boolean
  , expandNeighborhood  :: Boolean
  }

derive instance Newtype CacheParams _
derive instance Generic CacheParams _
derive instance Eq CacheParams
instance Show CacheParams where show = genericShow
derive newtype instance JSON.ReadForeign CacheParams
derive newtype instance JSON.WriteForeign CacheParams

-- (!) in case cache storage (ie. JavaScript Local Storage) returns an invalid
--     objects (eg. possible data migration), this will safely set new default
--     values
defaultCacheParams :: CacheParams
defaultCacheParams = CacheParams
  { expandSelection   : true
  , expandNeighborhood: true
  }
