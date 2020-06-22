module Gargantext.Components.GraphExplorer.API where

import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.NgramsTable.Core as NTC
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Prelude
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, get, post)
import Gargantext.Types as GT

type GraphAsyncUpdateParams =
  (
    graphId :: Int
  , listId :: Int
  , nodes :: Array (Record SigmaxT.Node)
  , session :: Session
  , termList :: GT.TermList
  , version :: NTC.Version
  )

graphAsyncUpdate :: Record GraphAsyncUpdateParams -> Aff GT.AsyncTaskWithType
graphAsyncUpdate {graphId, listId, nodes, session, termList, version} = do
  task <- post session p q
  pure $ GT.AsyncTaskWithType { task, typ: GT.GraphT }
  where
    p = GR.GraphAPI graphId $ GT.asyncTaskTypePath GT.GraphT
    q = { listId
        , nodes
        , termList
        , version
        }

type GraphAsyncRecomputeParams =
  (
    graphId :: Int
  , session :: Session
  )

graphAsyncRecompute :: Record GraphAsyncRecomputeParams -> Aff GT.AsyncTaskWithType
graphAsyncRecompute { graphId, session } = do
  task <- post session p q
  pure $ GT.AsyncTaskWithType { task, typ: GT.GraphT }
  where
    p = GR.GraphAPI graphId $ GT.asyncTaskTypePath GT.GraphT
    q = {}

type QueryProgressParams =
  (
    graphId :: Int
  , session :: Session
  , taskId  :: String
  )

queryProgress :: Record QueryProgressParams -> Aff GT.AsyncProgress
queryProgress { graphId, session, taskId } = do
  get session $ GR.GraphAPI graphId $ "async/" <> taskId <> "/poll"

type GraphVersions =
  (
    gv_graph :: Maybe Int
  , gv_repo :: Int
  )

type GraphVersionsParams =
  (
    graphId :: Int
  , session :: Session
  )

graphVersions :: Record GraphVersionsParams -> Aff (Record GraphVersions)
graphVersions { graphId, session }  = get session $ GR.GraphAPI graphId $ "versions"

type UpdateGraphVersionsParams =
  (
    graphId :: Int
  , session :: Session
  )

updateGraphVersions :: Record UpdateGraphVersionsParams -> Aff GET.GraphData
updateGraphVersions { graphId, session } = post session (GR.GraphAPI graphId $ "versions") {}
