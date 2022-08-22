module Gargantext.Components.PhyloExplorer.API
  ( get
  , UpdateData(..)
  , TimeUnit(..), ReflexiveTimeUnit(..), TimeUnitCriteria(..)
  , Clique(..), ReflexiveClique(..), CliqueFilter(..)
  , toReflexiveTimeUnit, fromReflexiveTimeUnit, extractCriteria
  , toReflexiveClique
  , update, updateProgress
  ) where

import Gargantext.Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Symbol (SProxy(..))
import Gargantext.Components.PhyloExplorer.JSON (PhyloJSON)
import Gargantext.Components.PhyloExplorer.Types (PhyloSet, parseToPhyloSet)
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Routes as GR
import Gargantext.Sessions (Session)
import Gargantext.Sessions as S
import Gargantext.Types (NodeID)
import Gargantext.Types as GT
import Record as Record
import Simple.JSON as JSON
import Simple.JSON.Generics as JSONG


get :: S.Session -> NodeID -> AffRESTError (PhyloSet)
get session nodeId = request >>= (_ <#> parseToPhyloSet) >>> pure
  where
    request :: AffRESTError (PhyloJSON)
    request = S.get session $ PhyloAPI nodeId

----------------------------------------------------------

newtype UpdateData = UpdateData
  { proximity     :: Number
  , synchrony     :: Number
  , quality       :: Number
  , timeUnit      :: TimeUnit
  , clique        :: Clique
  , exportFilter  :: Number
  }

derive instance Generic UpdateData _
derive instance Eq UpdateData
derive instance Newtype UpdateData _
instance Show UpdateData where show = genericShow
derive newtype instance JSON.ReadForeign UpdateData
instance JSON.WriteForeign UpdateData where
  writeImpl (UpdateData o) = (JSON.writeImpl <<< rename) o
    where
      rename
          = Record.rename
            (SProxy :: SProxy "proximity")
            (SProxy :: SProxy "_sc_phyloProximity")
        >>> Record.rename
            (SProxy :: SProxy "synchrony")
            (SProxy :: SProxy "_sc_phyloSynchrony")
        >>> Record.rename
            (SProxy :: SProxy "quality")
            (SProxy :: SProxy "_sc_phyloQuality")
        >>> Record.rename
            (SProxy :: SProxy "timeUnit")
            (SProxy :: SProxy "_sc_timeUnit")
        >>> Record.rename
            (SProxy :: SProxy "clique")
            (SProxy :: SProxy "_sc_clique")
        >>> Record.rename
            (SProxy :: SProxy "exportFilter")
            (SProxy :: SProxy "_sc_exportFilter")

data TimeUnit
  = Epoch TimeUnitCriteria
  | Year  TimeUnitCriteria
  | Month TimeUnitCriteria
  | Week  TimeUnitCriteria
  | Day   TimeUnitCriteria

derive instance Generic TimeUnit _
derive instance Eq TimeUnit
instance Show TimeUnit where show = genericShow
instance JSON.ReadForeign TimeUnit where readImpl = JSONG.untaggedSumRep
instance JSON.WriteForeign TimeUnit where
  writeImpl = case _ of
    Epoch (TimeUnitCriteria o) -> (JSON.writeImpl <<< parseEpoch) o
    Year  (TimeUnitCriteria o) -> (JSON.writeImpl <<< parseYear) o
    Month (TimeUnitCriteria o) -> (JSON.writeImpl <<< parseMonth) o
    Week  (TimeUnitCriteria o) -> (JSON.writeImpl <<< parseWeek) o
    Day   (TimeUnitCriteria o) -> (JSON.writeImpl <<< parseDay) o
    where
      parseEpoch
          = Record.rename
            (SProxy :: SProxy "period")
            (SProxy :: SProxy "_epoch_period")
        >>> Record.rename
            (SProxy :: SProxy "step")
            (SProxy :: SProxy "_epoch_step")
        >>> Record.rename
            (SProxy :: SProxy "matchingFrame")
            (SProxy :: SProxy "_epoch_matchingFrame")
        >>> Record.insert
            (SProxy :: SProxy "tag")
            "Epoch"
      parseYear
          = Record.rename
            (SProxy :: SProxy "period")
            (SProxy :: SProxy "_year_period")
        >>> Record.rename
            (SProxy :: SProxy "step")
            (SProxy :: SProxy "_year_step")
        >>> Record.rename
            (SProxy :: SProxy "matchingFrame")
            (SProxy :: SProxy "_year_matchingFrame")
        >>> Record.insert
            (SProxy :: SProxy "tag")
            "Year"
      parseMonth
          = Record.rename
            (SProxy :: SProxy "period")
            (SProxy :: SProxy "_month_period")
        >>> Record.rename
            (SProxy :: SProxy "step")
            (SProxy :: SProxy "_month_step")
        >>> Record.rename
            (SProxy :: SProxy "matchingFrame")
            (SProxy :: SProxy "_month_matchingFrame")
        >>> Record.insert
            (SProxy :: SProxy "tag")
            "Month"
      parseWeek
          = Record.rename
            (SProxy :: SProxy "period")
            (SProxy :: SProxy "_week_period")
        >>> Record.rename
            (SProxy :: SProxy "step")
            (SProxy :: SProxy "_week_step")
        >>> Record.rename
            (SProxy :: SProxy "matchingFrame")
            (SProxy :: SProxy "_week_matchingFrame")
        >>> Record.insert
            (SProxy :: SProxy "tag")
            "Week"
      parseDay
          = Record.rename
            (SProxy :: SProxy "period")
            (SProxy :: SProxy "_day_period")
        >>> Record.rename
            (SProxy :: SProxy "step")
            (SProxy :: SProxy "_day_step")
        >>> Record.rename
            (SProxy :: SProxy "matchingFrame")
            (SProxy :: SProxy "_day_matchingFrame")
        >>> Record.insert
            (SProxy :: SProxy "tag")
            "Day"


data ReflexiveTimeUnit
  = Epoch_
  | Year_
  | Month_
  | Week_
  | Day_

derive instance Generic ReflexiveTimeUnit _
derive instance Eq ReflexiveTimeUnit
instance Show ReflexiveTimeUnit where show = genericShow
instance Read ReflexiveTimeUnit where
  read :: String -> Maybe ReflexiveTimeUnit
  read = case _ of
    "Epoch_" -> Just Epoch_
    "Year_"  -> Just Year_
    "Month_" -> Just Month_
    "Week_"  -> Just Week_
    "Day_"   -> Just Day_
    _        -> Nothing


newtype TimeUnitCriteria = TimeUnitCriteria
  { period        :: Int
  , step          :: Int
  , matchingFrame :: Int
  }

derive instance Generic TimeUnitCriteria _
derive instance Eq TimeUnitCriteria
derive instance Newtype TimeUnitCriteria _
instance Show TimeUnitCriteria where show = genericShow
derive newtype instance JSON.ReadForeign TimeUnitCriteria


data Clique
  = FIS
    { support   :: Int
    , size      :: Int
    }
  | MaxClique
    { size      :: Int
    , threshold :: Number
    , filter    :: CliqueFilter
    }

derive instance Eq Clique
derive instance Generic Clique _
instance Show Clique where show = genericShow
instance JSON.ReadForeign Clique where readImpl = JSONG.untaggedSumRep
instance JSON.WriteForeign Clique where
  writeImpl = case _ of
    FIS o       -> (JSON.writeImpl <<< parseFIS) o
    MaxClique o -> (JSON.writeImpl <<< parseMaxClique) o
    where
      parseFIS
          = Record.insert
            (SProxy :: SProxy "tag")
            "Fis"
        >>> Record.rename
            (SProxy :: SProxy "support")
            (SProxy :: SProxy "_fis_support")
        >>> Record.rename
            (SProxy :: SProxy "size")
            (SProxy :: SProxy "_fis_size")
      parseMaxClique
          = Record.insert
            (SProxy :: SProxy "tag")
            "MaxClique"
        >>> Record.rename
            (SProxy :: SProxy "size")
            (SProxy :: SProxy "_mcl_size")
        >>> Record.rename
            (SProxy :: SProxy "threshold")
            (SProxy :: SProxy "_mcl_threshold")
        >>> Record.rename
            (SProxy :: SProxy "filter")
            (SProxy :: SProxy "_mcl_filter")


data ReflexiveClique
  = FIS_
  | MaxClique_

derive instance Generic ReflexiveClique _
derive instance Eq ReflexiveClique
instance Show ReflexiveClique where show = genericShow
instance Read ReflexiveClique where
  read :: String -> Maybe ReflexiveClique
  read = case _ of
    "FIS_"       -> Just FIS_
    "MaxClique_" -> Just MaxClique_
    _            -> Nothing


data CliqueFilter = ByThreshold | ByNeighbours

derive instance Eq CliqueFilter
derive instance Generic CliqueFilter _
instance Show CliqueFilter where show = genericShow
instance JSON.ReadForeign CliqueFilter where readImpl = JSONG.enumSumRep
instance JSON.WriteForeign CliqueFilter where writeImpl = JSON.writeImpl <<< show
instance Read CliqueFilter where
  read :: String -> Maybe CliqueFilter
  read = case _ of
    "ByThreshold"  -> Just ByThreshold
    "ByNeighbours" -> Just ByNeighbours
    _              -> Nothing


toReflexiveTimeUnit :: TimeUnit -> ReflexiveTimeUnit
toReflexiveTimeUnit (Epoch _) = Epoch_
toReflexiveTimeUnit (Year _)  = Year_
toReflexiveTimeUnit (Month _) = Month_
toReflexiveTimeUnit (Week _)  = Week_
toReflexiveTimeUnit (Day _)   = Day_

fromReflexiveTimeUnit :: ReflexiveTimeUnit -> TimeUnitCriteria -> TimeUnit
fromReflexiveTimeUnit Epoch_ c = Epoch c
fromReflexiveTimeUnit Year_  c = Year c
fromReflexiveTimeUnit Month_ c = Month c
fromReflexiveTimeUnit Week_  c = Week c
fromReflexiveTimeUnit Day_   c = Day c

extractCriteria :: TimeUnit -> TimeUnitCriteria
extractCriteria (Epoch (o :: TimeUnitCriteria)) = o
extractCriteria (Year  (o :: TimeUnitCriteria)) = o
extractCriteria (Month (o :: TimeUnitCriteria)) = o
extractCriteria (Week  (o :: TimeUnitCriteria)) = o
extractCriteria (Day   (o :: TimeUnitCriteria)) = o


toReflexiveClique :: Clique -> ReflexiveClique
toReflexiveClique (FIS _)       = FIS_
toReflexiveClique (MaxClique _) = MaxClique_



update ::
     Session
  -> NodeID
  -> Unit
  -> AffRESTError GT.AsyncTaskWithType
update session nodeId _
    = S.post session request {}
  >>= case _ of
    Left err   -> pure $ Left err
    Right task -> pure $ Right $ GT.AsyncTaskWithType
      { task
      , typ: GT.UpdateNode
      }

  where

    request = GR.NodeAPI GT.Node (Just nodeId)
      (GT.asyncTaskTypePath GT.UpdateNode)


updateProgress ::
     Session
  -> NodeID
  -> GT.AsyncTaskWithType
  -> AffRESTError GT.AsyncProgress
updateProgress
  session
  nodeId
  (GT.AsyncTaskWithType { task: GT.AsyncTask { id } })
  =
    S.get session request

  where

    request = GR.NodeAPI GT.Node (Just nodeId)
      (GT.asyncTaskTypePath GT.UpdateNode <> pollParams)

    pollParams = "/" <> id <> "/poll?limit1"
