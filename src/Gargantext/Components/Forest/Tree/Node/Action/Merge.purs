module Gargantext.Components.Forest.Tree.Node.Action.Merge where

import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Tools (submitButton, panel, checkbox, checkboxesListGroup)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (subTreeView, SubTreeParamsIn)
import Gargantext.Prelude
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, put_)
import Gargantext.Types  as GT
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Merge"

mergeNodeReq :: Session -> GT.ID -> GT.ID -> Aff (Array GT.ID)
mergeNodeReq session fromId toId =
  put_ session $ NodeAPI GT.Node (Just fromId) ("merge/" <> show toId)

mergeNode :: R2.Component SubTreeParamsIn
mergeNode = R.createElement mergeNodeCpt

mergeNodeCpt :: R.Component SubTreeParamsIn
mergeNodeCpt = here.component "mergeNode" cpt
  where
    cpt p@{dispatch, subTreeParams, id, nodeType, session, handed} _ = do
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
                      , handed
                      }
                  , H.ul { className:"merge mx-auto list-group"}
                     ([ H.li { className: "list-group-item" }
                       [ H.h5 { className: "mb-1" } [ H.text "Merge which list?" ]
                       ]
                     ] <> (checkboxesListGroup [ GT.MapTerm, GT.CandidateTerm, GT.StopTerm ] options))
                  , H.ul { className:"merge mx-auto list-group"}
                     [ H.li { className: "list-group-item" }
                       [ H.h5 { className: "mb-1" } [ H.text "Title" ]
                       ]
                     , H.li { className: "list-group-item" }
                       [ H.div { className: " form-check" }
                         [ checkbox merge
                         , H.label { className: "form-check-label" } [ H.text "Merge data?" ]
                         ]
                       ]
                     ]
                  ]
                  button
