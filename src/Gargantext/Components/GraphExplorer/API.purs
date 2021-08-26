module Gargantext.Components.GraphExplorer.API where

import Gargantext.Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.NgramsTable.Core as NTC
import Gargantext.Config.REST (RESTError)
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, get, post)
import Gargantext.Types as GT

type GraphAsyncUpdateParams =
  ( graphId :: Int
  , listId :: Int
  , nodes :: Array (Record SigmaxT.Node)
  , session :: Session
  , termList :: GT.TermList
  , version :: NTC.Version
  )

graphAsyncUpdate :: Record GraphAsyncUpdateParams -> Aff (Either RESTError GT.AsyncTaskWithType)
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

graphAsyncRecompute :: Record GraphAsyncRecomputeParams -> Aff (Either RESTError GT.AsyncTaskWithType)
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

queryProgress :: Record QueryProgressParams -> Aff (Either RESTError GT.AsyncProgress)
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

graphVersions :: Record GraphVersionsParams -> Aff (Either RESTError (Record GraphVersions))
graphVersions { graphId, session }  = get session $ GR.GraphAPI graphId $ "versions"

type UpdateGraphVersionsParams =
  ( graphId :: Int
  , session :: Session
  )

updateGraphVersions :: Record UpdateGraphVersionsParams -> Aff (Either RESTError GET.GraphData)
updateGraphVersions { graphId, session } = post session (GR.GraphAPI graphId $ "versions") {}

type CloneGraphParams =
  ( hyperdataGraph :: GET.HyperdataGraph
  , id :: Int
  , session :: Session
  )

cloneGraph :: Record CloneGraphParams -> Aff (Either RESTError Int)
cloneGraph { hyperdataGraph, id, session } = post session (GR.GraphAPI id $ "clone") hyperdataGraph
