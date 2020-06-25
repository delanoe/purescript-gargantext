module Gargantext.Components.Forest.Tree.Node.Action.Link
  where

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Tools (submitButton, panel)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (SubTreeParamsIn, subTreeView, SubTreeOut(..))
import Gargantext.Prelude
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, put_)
import Gargantext.Types  as GT
import Reactix as R
import Reactix.DOM.HTML as H

linkNodeReq :: Session -> GT.ID -> GT.ID -> Aff (Array GT.ID)
linkNodeReq session fromId toId =
  put_ session $ NodeAPI GT.Node (Just fromId) ("link/" <> show toId)

linkNode :: Record SubTreeParamsIn -> R.Hooks R.Element
linkNode p@{dispatch, subTreeParams, id, nodeType, session} = do
  subTreeOut@(subTreeOutParams /\ setSubTreeOut) :: R.State (Maybe SubTreeOut)
    <- R.useState' Nothing
  let button = case subTreeOutParams of
        Nothing   -> H.div {} []
        Just sbto -> submitButton (LinkNode inId outId) dispatch
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

