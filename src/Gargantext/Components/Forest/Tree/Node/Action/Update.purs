module Gargantext.Components.Forest.Tree.Node.Action.Update where

import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))
import Effect.Aff (Aff)
import Gargantext.Components.Forest.Tree.Node.Action (Action)
import Gargantext.Components.Forest.Tree.Node.Tools (formChoiceSafe, formButton)
import Gargantext.Types (NodeType(..))
import Gargantext.Types  as GT
import Gargantext.Prelude (Unit, class Show, class Read, show, bind, ($), pure)
import Reactix as R
import Reactix.DOM.HTML as H

{-
updateNode :: Session -> ID -> UpdateNodeParams -> Aff (Array ID)
updateNode session nodeId params = post session $ GR.NodeAPI GT.Node (Just nodeId) ""
-}

data UpdateNodeParams = UpdateNodeParamsList { method :: Method }
                      | UpdateNodeParamsGraph { method :: String }
                      | UpdateNodeParamsTexts { method :: Int }

instance encodeJsonUpdateNodeParams :: EncodeJson UpdateNodeParams
  where
    encodeJson (UpdateNodeParamsList { method })
      = "method" := show method
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

----------------------------------------------------------------------

data Method = Basic | Advanced | WithModel

instance readMethod :: Read Method where
  read "Basic"    = Just Basic
  read "Advanced" = Just Advanced
  read "WithModel" = Just WithModel
  read _           = Nothing

instance showMethod :: Show Method where
  show Basic     = "Basic"
  show Advanced  = "Advanced"
  show WithModel = "WithModel"

----------------------------------------------------------------------

update :: NodeType -> R.Hooks R.Element
update NodeList = do
  method  @( _ /\ setMethod  ) <- R.useState' Basic
  nodeType@( _ /\ setNodeType) <- R.useState' NodeList
  pure $ H.div {} [ formChoiceSafe [Basic, Advanced, WithModel] Basic setMethod
                  , formButton NodeList setNodeType
                  ]
update Graph    = pure $ H.div {} []
update Texts    = pure $ H.div {} []
update _        = pure $ H.div {} []

-- fragmentPT $ "Update " <> show nodeType
