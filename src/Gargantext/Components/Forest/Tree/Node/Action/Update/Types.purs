module Gargantext.Components.Forest.Tree.Node.Action.Update.Types where

import Data.Argonaut as Argonaut
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Gargantext.Utils.Argonaut (genericSumDecodeJson, genericSumEncodeJson)
import Data.Maybe (Maybe(..))
import Gargantext.Prelude (class Eq, class Read, class Show)


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


