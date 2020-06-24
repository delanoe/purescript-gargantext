module Gargantext.Components.Forest.Tree.Node.Action.Link
  where

import Data.Argonaut as Argonaut
import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Gargantext.Prelude
import Effect.Aff (Aff)
import Gargantext.Types  as GT
import Gargantext.Sessions (Session, put_)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Types (NodeType(..))
import Gargantext.Utils.Argonaut (genericSumDecodeJson, genericSumEncodeJson, genericEnumDecodeJson, genericEnumEncodeJson)
import Data.Generic.Rep.Show (genericShow)
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Reactix as R
import Gargantext.Components.Forest.Tree.Node.Tools (submitButton, panel)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (SubTreeParamsProps, subTreeView)
import Reactix.DOM.HTML as H

-- TODO moveNodeReq
linkNodeReq :: Session -> GT.ID -> GT.ID -> Aff (Array GT.ID)
linkNodeReq session fromId toId =
  put_ session $ NodeAPI GT.Node (Just fromId) ("link/" <> show toId)

linkNode :: Record SubTreeParamsProps -> R.Hooks R.Element
linkNode p = do
  pure $ subTreeView p

