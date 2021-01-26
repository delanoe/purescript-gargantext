module Gargantext.Components.Forest.Tree.Node.Action.Move where

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Tools (submitButton, panel)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (subTreeView, SubTreeParamsIn)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, put_)
import Gargantext.Types as GT

thisModule :: String
thisModule = "Gargantext.Components.Forest.Tree.Node.Action.Move"

moveNodeReq :: Session -> GT.ID -> GT.ID -> Aff (Array GT.ID)
moveNodeReq session fromId toId =
  put_ session $ NodeAPI GT.Node (Just fromId) ("move/" <> show toId)

moveNode :: Record SubTreeParamsIn -> R.Element
moveNode p = R.createElement moveNodeCpt p []

moveNodeCpt :: R.Component SubTreeParamsIn
moveNodeCpt = R.hooksComponentWithModule thisModule "moveNode" cpt
  where
    cpt { dispatch, handed, id, nodeType, session, subTreeParams } _ = do
      action@(valAction /\ setAction) :: R.State Action <- R.useState' (MoveNode {params: Nothing})

      let button = case valAction of
              MoveNode {params} -> case params of
                Just val -> submitButton (MoveNode {params: Just val}) dispatch
                Nothing -> H.div {} []
              _                   -> H.div {} []

      pure $ panel [ subTreeView { action
                                 , dispatch
                                 , handed
                                 , id
                                 , nodeType
                                 , session
                                 , subTreeParams
                                 }
              ] button
