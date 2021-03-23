module Gargantext.Components.Forest.Tree.Node.Action.Move where

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Prelude

import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Tools (submitButton, panel)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (subTreeView, SubTreeParamsIn)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, put_)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Move"

moveNodeReq :: Session -> GT.ID -> GT.ID -> Aff (Array GT.ID)
moveNodeReq session fromId toId =
  put_ session $ NodeAPI GT.Node (Just fromId) ("move/" <> show toId)

moveNode :: R2.Component SubTreeParamsIn
moveNode = R.createElement moveNodeCpt

moveNodeCpt :: R.Component SubTreeParamsIn
moveNodeCpt = here.component "moveNode" cpt
  where
    cpt { dispatch, handed, id, nodeType, session, subTreeParams } _ = do
      action :: T.Box Action <- T.useBox (MoveNode {params: Nothing})
      action' <- T.useLive T.unequal action

      let button = case action' of
              MoveNode { params } -> case params of
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
                                 } []
              ] button
