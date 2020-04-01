module Gargantext.Components.GraphExplorer.API where

import Gargantext.Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)

import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.NgramsTable.Core as NTC
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, post)
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
