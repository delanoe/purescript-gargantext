module Gargantext.Components.Nodes.Dashboard.Types where

import Gargantext.Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Simple.JSON as JSON

import Gargantext.Components.Nodes.Corpus.Chart.Predefined as P

import Gargantext.Components.Nodes.Types (FTFieldList)
import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, get, put)
import Gargantext.Types (NodeType(..))

type Preferences = Maybe String

newtype Hyperdata =
  Hyperdata
  { charts      :: Array P.PredefinedChart
  , fields      :: FTFieldList
  , preferences :: Preferences
  }
derive instance Generic Hyperdata _
derive instance Newtype Hyperdata _
derive newtype instance JSON.ReadForeign Hyperdata
derive newtype instance JSON.WriteForeign Hyperdata
-- instance JSON.WriteForeign Hyperdata where
--   writeImpl (Hyperdata h) = JSON.writeImpl h'
--     where
--       h' = { charts: h.charts
--            , fields: List.toUnfoldable h.fields :: Array FTField
--            , preferences: h.preferences }
instance Eq Hyperdata where
  eq = genericEq


type LoadProps = ( nodeId  :: Int, session :: Session )

loadDashboard' :: Record LoadProps -> Aff DashboardData
loadDashboard' {nodeId, session} = get session $ NodeAPI Node (Just nodeId) ""

-- Just to make reloading effective
loadDashboardWithReload :: {reload :: Int  | LoadProps} -> Aff DashboardData
loadDashboardWithReload {nodeId, session} = loadDashboard' {nodeId, session}

type SaveProps = ( hyperdata :: Hyperdata | LoadProps )

saveDashboard :: Record SaveProps -> Aff Unit
saveDashboard {hyperdata, nodeId, session} = do
  _id <- (put session (NodeAPI Node (Just nodeId) "") hyperdata) :: Aff Int
  pure unit

newtype DashboardData =
  DashboardData
  { id        :: Int
  , hyperdata :: Hyperdata
  , parentId  :: Int
  }
derive instance Generic DashboardData _
derive instance Newtype DashboardData _
instance JSON.ReadForeign DashboardData where
  readImpl f = do
    inst :: { id :: Int, hyperdata :: Hyperdata, parent_id :: Int } <- JSON.readImpl f
    pure $ DashboardData { id: inst.id
                         , hyperdata: inst.hyperdata
                         , parentId: inst.parent_id }
instance JSON.WriteForeign DashboardData where
  writeImpl (DashboardData { id, hyperdata, parentId }) =
    JSON.writeImpl { id, hyperdata, parent_id: parentId }
instance Eq DashboardData where
  eq = genericEq
