module Gargantext.Components.NgramsTable.API where

import Data.Either (Either)
import Effect.Aff (Aff)

import Gargantext.Config.REST (RESTError)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, post)
import Gargantext.Types as GT

type UpdateNodeListParams =
  (
    listId :: Int
  , nodeId :: Int
  , nodeType :: GT.TabSubType GT.CTabNgramType
  , session :: Session
  )

updateNodeList :: Record UpdateNodeListParams -> Aff (Either RESTError Int)
updateNodeList { listId, nodeId, nodeType, session } =
  post session (GR.RecomputeNgrams nodeType nodeId listId) {}
