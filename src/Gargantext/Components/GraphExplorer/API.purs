module Gargantext.Components.GraphExplorer.API where

import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Core.NgramsTable.Types as CNT
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, get, post)
import Gargantext.Types as GT
import Gargantext.Types as Types
import Gargantext.Utils.Toestand as T2

type GraphAsyncUpdateParams =
  ( graphId :: Int
  , listId :: Int
  , nodes :: Array (Record SigmaxT.Node)
  , session :: Session
  , termList :: GT.TermList
  , version :: CNT.Version
  )

graphAsyncUpdate :: Record GraphAsyncUpdateParams -> AffRESTError GT.AsyncTaskWithType
graphAsyncUpdate { graphId, listId, nodes, session, termList, version } = do
  eTask <- post session p q
  pure $ (\task -> GT.AsyncTaskWithType { task, typ: GT.GraphRecompute }) <$> eTask
  where
    p = GR.GraphAPI graphId $ GT.asyncTaskTypePath GT.GraphRecompute
    q = { listId
        , nodes
        , termList
        , version
        }

type GraphAsyncRecomputeParams =
  ( graphId :: Int
  , session :: Session
  )

graphAsyncRecompute :: Record GraphAsyncRecomputeParams -> AffRESTError GT.AsyncTaskWithType
graphAsyncRecompute { graphId, session } = do
  eTask <- post session p q
  pure $ (\task -> GT.AsyncTaskWithType { task, typ: GT.GraphRecompute }) <$> eTask
  where
    p = GR.GraphAPI graphId $ GT.asyncTaskTypePath GT.GraphRecompute
    q = {}

type QueryProgressParams =
  ( graphId :: Int
  , session :: Session
  , taskId  :: String
  )

queryProgress :: Record QueryProgressParams -> AffRESTError GT.AsyncProgress
queryProgress { graphId, session, taskId } = do
  get session $ GR.GraphAPI graphId $ "async/" <> taskId <> "/poll"

type GraphVersions =
  ( gv_graph :: Maybe Int
  , gv_repo :: Int
  )

type GraphVersionsParams =
  ( graphId :: Int
  , session :: Session
  )

graphVersions :: Record GraphVersionsParams -> AffRESTError (Record GraphVersions)
graphVersions { graphId, session }  = get session $ GR.GraphAPI graphId $ "versions"

type UpdateGraphVersionsParams =
  ( graphId :: Int
  , session :: Session
  )

updateGraphVersions :: Record UpdateGraphVersionsParams -> AffRESTError GET.GraphData
updateGraphVersions { graphId, session } = post session (GR.GraphAPI graphId $ "versions") {}

type CloneGraphParams =
  ( hyperdataGraph :: GET.HyperdataGraph
  , id :: Int
  , session :: Session
  )

cloneGraph :: Record CloneGraphParams -> AffRESTError Int
cloneGraph { hyperdataGraph, id, session } = post session (GR.GraphAPI id $ "clone") hyperdataGraph

-----------------------------------------------

getNodes :: Session -> T2.Reload -> GET.GraphId -> AffRESTError GET.HyperdataGraph
getNodes session graphVersion graphId =
  get session $ NodeAPI Types.Graph
                        (Just graphId)
                        ("?version=" <> (show graphVersion))
