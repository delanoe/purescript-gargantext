module Gargantext.Components.Forest.Tree.Node.Action.Share where

import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Prelude (($))
import Reactix as R
import Gargantext.Components.Forest.Tree.Node.Action
import Gargantext.Types as GT
import Gargantext.Types (ID)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, post)
import Gargantext.Components.Forest.Tree.Node.Tools.TextInputBox as Tools

------------------------------------------------------------------------
share :: Session -> ID -> ShareValue -> Aff (Array ID)
share session nodeId =
  post session $ GR.NodeAPI GT.Node (Just nodeId) "share"

shareAction :: String -> Action
shareAction username = ShareNode username

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

