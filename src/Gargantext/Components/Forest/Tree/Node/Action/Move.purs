module Gargantext.Components.Forest.Tree.Node.Action.Move
  ( moveNodeReq
  , moveNode
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(..))
import Gargantext.Components.Forest.Tree.Node.Tools (submitButton, panel)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (SubTreeParamsIn, subTreeView)
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, put_)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Move"

moveNodeReq :: Session -> GT.ID -> GT.ID -> AffRESTError (Array GT.ID)
moveNodeReq session fromId toId =
  put_ session $ NodeAPI GT.Node (Just fromId) ("move/" <> show toId)

moveNode :: R2.Component SubTreeParamsIn
moveNode = R.createElement moveNodeCpt
moveNodeCpt :: R.Component SubTreeParamsIn
moveNodeCpt = here.component "moveNode" cpt
  where
    cpt { boxes, dispatch, id, nodeType, session, subTreeParams } _ = do
      action :: T.Box Action <- T.useBox (MoveNode {params: Nothing})

      pure $

        moveNode' { action
            , boxes
            , dispatch
            , id
            , nodeType
            , session
            , subTreeParams
            } []

type Props =
  ( action :: T.Box Action
  | SubTreeParamsIn
  )

-- @XXX re-render issue -> clone component
moveNode' :: R2.Component Props
moveNode' = R.createElement moveNodeCpt'
moveNodeCpt' :: R.Component Props
moveNodeCpt' = here.component "__clone__" cpt where
  cpt { boxes, dispatch, id, nodeType, session, subTreeParams, action } _ = do

    action' <- T.useLive T.unequal action

    let button = case action' of
            MoveNode { params } -> case params of
              Just val -> submitButton (MoveNode {params: Just val}) dispatch
              Nothing -> H.div {} []
            _                   -> H.div {} []

    pure $

      panel
      [ subTreeView { action
                      , boxes
                      , dispatch
                      , id
                      , nodeType
                      , session
                      , subTreeParams
                      } []
      ] button
