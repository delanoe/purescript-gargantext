module Gargantext.Components.Forest.Tree.Node.Action.Update where


import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))
import Effect.Aff (Aff)
import Gargantext.Components.Forest.Tree.Node.Action (Action)
import Gargantext.Types (NodeType)
import Gargantext.Types  as GT
import Gargantext.Prelude (Unit)

{-
updateNode :: Session -> ID -> UpdateNodeParams -> Aff (Array ID)
updateNode session nodeId params = post session $ GR.NodeAPI GT.Node (Just nodeId) ""
-}

data UpdateNodeParams = UpdateNodeParamsList { method :: Int }
                      | UpdateNodeParamsGraph { method :: String }
                      | UpdateNodeParamsTexts { method :: Int }

instance encodeJsonUpdateNodeParams :: EncodeJson UpdateNodeParams
  where
    encodeJson (UpdateNodeParamsList { method })
      = "method" := method
      ~> jsonEmptyObject
    encodeJson (UpdateNodeParamsGraph { method })
      = "method" := method
      ~> jsonEmptyObject
    encodeJson (UpdateNodeParamsTexts { method })
      = "method" := method
      ~> jsonEmptyObject
----------------------------------------------------------------------

type UpdateNodeProps =
  ( id       :: GT.ID
  , dispatch :: Action -> Aff Unit
  , name     :: GT.Name
  , nodeType :: NodeType
  , params   :: UpdateNodeParams
  )

