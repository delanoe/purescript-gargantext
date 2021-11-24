module Gargantext.Components.NgramsTable.API where

import Gargantext.Config.REST (AffRESTError)
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

updateNodeList :: Record UpdateNodeListParams -> AffRESTError Int
updateNodeList { listId, nodeId, nodeType, session } =
  post session (GR.RecomputeNgrams nodeType nodeId listId) {}
