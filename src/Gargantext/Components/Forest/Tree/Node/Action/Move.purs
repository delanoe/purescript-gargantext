module Gargantext.Components.Forest.Tree.Node.Action.Move
  where

import Data.Maybe (Maybe(..))
import Gargantext.Prelude
import Effect.Aff (Aff)
import Data.Tuple.Nested ((/\))
import Gargantext.Sessions (Session, put_)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Reactix as R
import Gargantext.Components.Forest.Tree.Node.Tools (submitButton, panel)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (SubTreeParamsProps, subTreeView, SubTreeOut(..))
import Gargantext.Types as GT
import Reactix.DOM.HTML as H

moveNodeReq :: Session -> GT.ID -> GT.ID -> Aff (Array GT.ID)
moveNodeReq session fromId toId =
  put_ session $ NodeAPI GT.Node (Just fromId) ("move/" <> show toId)

moveNode :: Record SubTreeParamsProps -> R.Hooks R.Element
moveNode p@{subTreeOut, dispatch} = pure $ panel [subTreeView p] button
  where
    ( subTreeOutParams /\ _ ) = subTreeOut
    button = case subTreeOutParams of
      Nothing   -> H.div {} []
      Just sbto -> submitButton (MoveNode inId outId) dispatch
        where
          (SubTreeOut { in:inId, out:outId}) = sbto

