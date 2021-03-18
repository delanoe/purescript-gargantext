module Gargantext.Components.Nodes.Dashboard.Types where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, (.:), (.:?), (:=), (~>), jsonEmptyObject)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)

import Gargantext.Components.Nodes.Corpus.Chart.Predefined as P
import Gargantext.Components.Nodes.Types (FTField)
import Gargantext.Prelude
import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, get, put)
import Gargantext.Types (NodeType(..))

type Preferences = Maybe String

newtype Hyperdata =
  Hyperdata
  { charts :: Array P.PredefinedChart
  , fields :: List.List FTField
  , preferences :: Preferences
  }
derive instance genericHyperdata :: Generic Hyperdata _
instance eqHyperdata :: Eq Hyperdata where
  eq = genericEq
instance decodeHyperdata :: DecodeJson Hyperdata where
  decodeJson json = do
    obj <- decodeJson json
    charts <- obj .: "charts"
    fields <- obj .: "fields"
    preferences <- obj .:? "preferences"
    pure $ Hyperdata {charts, fields, preferences}
instance encodeHyperdata :: EncodeJson Hyperdata where
  encodeJson (Hyperdata {charts, fields, preferences}) = do
       "charts"  := charts
    ~> "fields"  := fields
    ~> "preferences"  := preferences
    ~> jsonEmptyObject


type LoadProps = ( nodeId  :: Int, session :: Session )

loadDashboard' :: Record LoadProps -> Aff DashboardData
loadDashboard' {nodeId, session} = get session $ NodeAPI Node (Just nodeId) ""

-- Just to make reloading effective
loadDashboardWithReload :: {reload :: Int  | LoadProps} -> Aff DashboardData
loadDashboardWithReload {nodeId, session} = loadDashboard' {nodeId, session}

type SaveProps = ( hyperdata :: Hyperdata | LoadProps )

saveDashboard :: Record SaveProps -> Aff Unit
saveDashboard {hyperdata, nodeId, session} = do
  id_ <- (put session (NodeAPI Node (Just nodeId) "") hyperdata) :: Aff Int
  pure unit

type DashboardData =
  { id :: Int
  , hyperdata :: Hyperdata
  , parentId :: Int
  }
