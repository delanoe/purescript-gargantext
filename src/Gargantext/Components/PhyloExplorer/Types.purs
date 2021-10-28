module Gargantext.Components.PhyloExplorer.Types
  ( PhyloDataSet(..)
  , Branch(..), Period(..), Group(..)
  , GlobalTerm(..)
  , parsePhyloJSONSet
  ) where

import Gargantext.Prelude

import Data.Array as Array
import Data.Date as Date
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Number as Number
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Gargantext.Components.PhyloExplorer.JSON (PhyloJSONSet(..), RawObject(..))


-- @WIP Date or foreign?
foreign import yearToDate       :: String -> Date.Date
foreign import stringToDate     :: String -> Date.Date
foreign import utcStringToDate  :: String -> Date.Date


newtype PhyloDataSet = PhyloDataSet
  { bb            :: Array Number
  , branches      :: Array Branch
  , groups        :: Array Group
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

derive instance Generic PhyloDataSet _
derive instance Eq PhyloDataSet
instance Show PhyloDataSet where show = genericShow

parsePhyloJSONSet :: PhyloJSONSet -> PhyloDataSet
parsePhyloJSONSet (PhyloJSONSet o) = PhyloDataSet
  { bb            : parseBB o.bb
  , branches
  , groups
  , nbBranches    : parseInt o.phyloBranches
  , nbDocs        : parseInt o.phyloDocs
  , nbFoundations : parseInt o.phyloFoundations
  , nbGroups      : parseInt o.phyloGroups
  , nbPeriods     : parseInt o.phyloPeriods
  , nbTerms       : parseInt o.phyloTerms
  , periods
  , sources       : parseSources o.phyloSources
  , timeScale     : o.phyloTimeScale
  , weighted      : getGlobalWeightedValue groups
  }

  where
    epochTS   = o.phyloTimeScale == "epoch"
    branches  = parseBranches o.objects
    groups    = parseGroups epochTS o.objects
    periods   = parsePeriods epochTS o.objects

-----------------------------------------------------------

data Branch = Branch
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
      , label : o.label
      , x1    : o.branch_x
      , x2    : Tuple.fst $ parsePos o.pos
      , y     : o.branch_y
      }
    parse _                = Nothing

-----------------------------------------------------------

data Period = Period
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

data Group = Group
  { bId           :: Int
  , foundation    :: Array Int -- @WIP: Array String ???
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
      , role        : stringedArrayToArray' o.role
      , size        : parseInt o.support
      , source      : parseSources o.source
      , to          : parseNodeDate o.strTo   o.to   epoch
      , weight      : stringedMaybeToNumber o.weight
      , x           : Tuple.fst $ parsePos o.pos
      , y           : Tuple.snd $ parsePos o.pos
      }
    parse _               = Nothing

-----------------------------------------------------------

data GlobalTerm = GlobalTerm
  { label :: String
  , fdt   :: String
  }

derive instance Generic GlobalTerm _
derive instance Eq GlobalTerm
instance Show GlobalTerm where show = genericShow

-----------------------------------------------------------

parseInt :: String -> Int
parseInt s = maybe 0 identity $ Int.fromString s

parseFloat :: String -> Number
parseFloat s = maybe 0.0 identity $ Number.fromString s

parseSources :: String -> Array String
parseSources
  =   String.replace (String.Pattern "[") (String.Replacement "")
  >>> String.replace (String.Pattern "]") (String.Replacement "")
  >>> String.split (String.Pattern ",")
  >>> Array.filter (\s -> not eq 0 $ String.length s)
  >>> Array.sort

parseBB :: String -> Array Number
parseBB
  =   String.split (String.Pattern ",")
  >>> map parseFloat

parseNodeDate :: Maybe String -> String -> Boolean -> Date.Date
parseNodeDate Nothing    year _     = yearToDate(year)
parseNodeDate (Just str) _    true  = utcStringToDate(str)
parseNodeDate (Just str) _    false = stringToDate(str)

parsePos :: String -> Tuple.Tuple Number Number
parsePos
  =   String.split (String.Pattern ",")
  >>> \a -> (p $ Array.index a 0) /\
            (p $ Array.index a 1)

  where
    p = case _ of
      Nothing -> 0.0
      Just s  -> parseFloat s


-- @WIP: why taking last value? use `any`?
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

stringedArrayToArray :: String -> Array String
stringedArrayToArray str
  =   str # String.length
  >>> (\length    -> String.splitAt (length - 1) str)
  >>> (\{ after } -> String.splitAt 1 after)
  >>> (\{ after } -> String.split (String.Pattern "|") after)
  >>> map String.trim

stringedArrayToArray' :: String -> Array Int
stringedArrayToArray'
  =   stringedArrayToArray
  >>> map parseInt
