module Gargantext.Components.Forest.Tree.Node.Action.Merge where

import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Data.Set as Set
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(..))
import Gargantext.Components.Forest.Tree.Node.Tools (submitButton, panel, checkbox, checkboxesListGroup)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (subTreeView, SubTreeParamsIn)
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, put_)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Merge"

mergeNodeReq :: Session -> GT.ID -> GT.ID -> AffRESTError (Array GT.ID)
mergeNodeReq session fromId toId =
  put_ session $ NodeAPI GT.Node (Just fromId) ("merge/" <> show toId)

mergeNode :: R2.Component SubTreeParamsIn
mergeNode = R.createElement mergeNodeCpt
mergeNodeCpt :: R.Component SubTreeParamsIn
mergeNodeCpt = here.component "mergeNode" cpt
  where
    cpt { boxes, dispatch, id, nodeType, session, subTreeParams } _ = do
      action <- T.useBox (MergeNode { params: Nothing })
      action' <- T.useLive T.unequal action

      merge   <- T.useBox false
      options <- T.useBox (Set.singleton GT.MapTerm)

      let button = case action' of
            MergeNode {params} -> case params of
              Just val -> submitButton (MergeNode {params: Just val}) dispatch
              Nothing -> H.div {} []
            _                   -> H.div {} []

      pure $ panel
        [ subTreeView { action
                      , boxes
                      , dispatch
                      , id
                      , nodeType
                      , session
                      , subTreeParams
                      } []
                  , H.ul { className:"merge mx-auto list-group"}
                     ([ H.li { className: "list-group-item" }
                       [ H.h5 { className: "mb-1" } [ H.text "Merge which list?" ]
                       , checkboxesListGroup { groups: [ GT.MapTerm, GT.CandidateTerm, GT.StopTerm ]
                                             , options } []
                       ]
                     ])
                  , H.ul { className:"merge mx-auto list-group"}
                     [ H.li { className: "list-group-item" }
                       [ H.h5 { className: "mb-1" } [ H.text "Title" ]
                       ]
                     , H.li { className: "list-group-item" }
                       [ H.div { className: " form-check" }
                         [ checkbox { value: merge }
                         , H.label { className: "form-check-label" } [ H.text "Merge data?" ]
                         ]
                       ]
                     ]
                  ]
                  button
