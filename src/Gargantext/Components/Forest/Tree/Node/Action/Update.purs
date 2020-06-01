module Gargantext.Components.Forest.Tree.Node.Action.Update where

import Data.Array (length, head)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Maybe (Maybe(..), fromMaybe)
-- import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff)
import Effect.Uncurried (mkEffectFn1)
import Gargantext.Components.Forest.Tree.Node.Action (Action(..), ID, Name)
import Gargantext.Components.Forest.Tree.Node (SettingsBox(..), settingsBox)
import Gargantext.Types (NodeType(..), readNodeType)
import Gargantext.Utils.Reactix as R2
import Gargantext.Sessions (Session, post)
import Gargantext.Routes as GR
import Gargantext.Types  as GT
import Prelude (Unit, bind, const, discard, map, pure, show, ($), (<>), (>), (<<<))
import Reactix as R
import Reactix.DOM.HTML as H

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
  ( id       :: ID
  , dispatch :: Action -> Aff Unit
  , name     :: Name
  , nodeType :: NodeType
  , params   :: UpdateNodeParams
  )

