module Gargantext.Components.Forest.Tree.Node.Action.Merge
  where

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Tools (submitButton, panel, checkbox, checkboxes)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (subTreeView, SubTreeParamsIn)
import Gargantext.Prelude
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, put_)
import Gargantext.Types  as GT
import Reactix as R
import Reactix.DOM.HTML as H
import Data.Set as Set

mergeNodeReq :: Session -> GT.ID -> GT.ID -> Aff (Array GT.ID)
mergeNodeReq session fromId toId =
  put_ session $ NodeAPI GT.Node (Just fromId) ("merge/" <> show toId)

mergeNode :: Record SubTreeParamsIn -> R.Hooks R.Element
mergeNode p@{dispatch, subTreeParams, id, nodeType, session} = do
  action@(valAction /\ setAction) :: R.State Action <- R.useState' (MergeNode {params:Nothing})

  merge   <- R.useState' false
  options <- R.useState' (Set.singleton GT.MapTerm)

  let button = case valAction of
        MergeNode {params} -> case params of
          Just val -> submitButton (MergeNode {params: Just val}) dispatch
          Nothing -> H.div {} []
        _                   -> H.div {} []

  pure $ panel [ subTreeView { action
                             , dispatch
                             , id
                             , nodeType
                             , session
                             , subTreeParams
                             }
               , H.div { className:"panel panel-primary"}
                       [ H.text "Merge which list?"
                       , checkboxes [GT.MapTerm, GT.CandidateTerm, GT.StopTerm] options
                       ]
               , H.div { className:"panel panel-primary"}
                       [ H.text "Title"
                       , H.div {className: "checkbox"}
                               [checkbox merge, H.text "Merge data?"]
                       ]
               ] button

