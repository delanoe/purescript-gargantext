module Gargantext.Components.Forest.Tree.Node.Action.Move
  where

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Gargantext.Components.Forest.Tree.Node.Action (Props, Action(..))
import Gargantext.Components.Forest.Tree.Node.Tools (submitButton, panel)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (SubTreeParamsIn, subTreeView, SubTreeOut(..))
import Gargantext.Prelude
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, put_)
import Gargantext.Types as GT
import Reactix as R
import Reactix.DOM.HTML as H

moveNodeReq :: Session -> GT.ID -> GT.ID -> Aff (Array GT.ID)
moveNodeReq session fromId toId =
  put_ session $ NodeAPI GT.Node (Just fromId) ("move/" <> show toId)

moveNode :: Record SubTreeParamsIn -> R.Hooks R.Element
moveNode p@{dispatch, subTreeParams, id, nodeType, session} = do
  subTreeOut@(subTreeOutParams /\ setSubTreeOut) :: R.State (Maybe SubTreeOut)
    <- R.useState' Nothing
  let button = case subTreeOutParams of
        Nothing   -> H.div {} []
        Just sbto -> submitButton (MoveNode inId outId) dispatch
          where
            (SubTreeOut { in:inId, out:outId}) = sbto
  pure $ panel [ subTreeView { subTreeOut
                             , dispatch
                             , subTreeParams
                             , id
                             , nodeType
                             , session
                             }
               ] button

