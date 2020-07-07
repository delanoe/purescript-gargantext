module Gargantext.Components.Forest.Tree.Node.Action.Share where

import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Prelude (($))
import Reactix as R
import Gargantext.Components.Forest.Tree.Node.Action (Action)
import Gargantext.Components.Forest.Tree.Node.Action as Action
import Gargantext.Types as GT
import Gargantext.Types (ID)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, post)
import Gargantext.Components.Forest.Tree.Node.Tools as Tools


import Data.Argonaut as Argonaut
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Gargantext.Utils.Argonaut (genericSumDecodeJson, genericSumEncodeJson, genericEnumDecodeJson, genericEnumEncodeJson)
import Data.Maybe (Maybe(..))
import Gargantext.Prelude (class Eq, class Read, class Show)

------------------------------------------------------------------------
share :: Session -> ID -> ShareNode -> Aff ID
share session nodeId =
  post session $ GR.NodeAPI GT.Node (Just nodeId) "share"

shareAction :: String -> Action
shareAction username = Action.ShareTeam username

------------------------------------------------------------------------
newtype ShareValue = ShareValue
  { text :: String }

instance encodeJsonShareValue :: EncodeJson ShareValue where
  encodeJson (ShareValue {text})
     = "username" := text
    ~> jsonEmptyObject

------------------------------------------------------------------------
textInputBox :: Record Tools.TextInputBoxProps -> R.Element
textInputBox  = Tools.textInputBox

------------------------------------------------------------------------

data ShareNode = ShareTeam { username :: String }
               | SharePublic { rights :: String }

derive instance eqShareNode :: Eq ShareNode

derive instance genericShareNode :: Generic ShareNode _

instance showShareNode :: Show ShareNode where
  show = genericShow

instance decodeJsonShareNode :: Argonaut.DecodeJson ShareNode where
  decodeJson = genericSumDecodeJson

instance encodeJsonShareNode :: Argonaut.EncodeJson ShareNode where
  encodeJson = genericSumEncodeJson


