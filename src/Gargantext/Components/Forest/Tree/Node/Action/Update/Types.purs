module Gargantext.Components.Forest.Tree.Node.Action.Update.Types where

import Data.Argonaut as Argonaut
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Tuple.Nested ((/\))
import Gargantext.Utils.Argonaut (genericSumDecodeJson, genericSumEncodeJson)
import Data.Maybe (Maybe(..))
import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))
import Effect.Aff (Aff)
import Gargantext.Types (NodeType(..))
import Gargantext.Types  as GT
import Gargantext.Prelude (Unit, class Eq, class Show, class Read, class Read, show, bind, ($), pure)
import Reactix as R
import Reactix.DOM.HTML as H


data UpdateNodeParams = UpdateNodeParamsList { method :: Method }
                      | UpdateNodeParamsGraph { method :: String }
                      | UpdateNodeParamsTexts { method :: Int }


derive instance eqUpdateNodeParams :: Eq UpdateNodeParams

derive instance genericUpdateNodeParams :: Generic UpdateNodeParams _

instance showUpdateNodeParams :: Show UpdateNodeParams where
  show = genericShow

instance decodeJsonUpdateNodeParams :: Argonaut.DecodeJson UpdateNodeParams where
  decodeJson = genericSumDecodeJson

instance encodeJsonUpdateNodeParams :: Argonaut.EncodeJson UpdateNodeParams where
  encodeJson = genericSumEncodeJson

{-
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
-}
----------------------------------------------------------------------

----------------------------------------------------------------------
data Method = Basic | Advanced | WithModel

derive instance genericMethod :: Generic Method _

derive instance eqMethod :: Eq Method

instance showMethod :: Show Method where
  show = genericShow

instance readMethod :: Read Method where
  read "Basic"    = Just Basic
  read "Advanced" = Just Advanced
  read "WithModel" = Just WithModel
  read _           = Nothing

instance decodeJsonMethod :: Argonaut.DecodeJson Method where
  decodeJson = genericSumDecodeJson

instance encodeJsonMethod :: Argonaut.EncodeJson Method where
  encodeJson = genericSumEncodeJson


