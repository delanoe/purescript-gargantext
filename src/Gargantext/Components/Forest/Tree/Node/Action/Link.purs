module Gargantext.Components.Forest.Tree.Node.Action.Link where

import Gargantext.Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Update.Types (LinkNodeReq(..), UpdateNodeParams(..))
import Gargantext.Components.Forest.Tree.Node.Tools (submitButton, panel)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (subTreeView, SubTreeParamsIn)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree.Types (SubTreeOut(..))
import Gargantext.Config.REST (AffRESTError, RESTError)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, post)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Link"

linkNodeReq :: Session -> Maybe GT.NodeType -> GT.ID -> GT.ID -> AffRESTError GT.AsyncTaskWithType
linkNodeReq session nt fromId toId = do
  eTask :: Either RESTError GT.AsyncTask <- post session (NodeAPI GT.Node (Just fromId) "update")
                        (LinkNodeReq { nodeType: linkNodeType nt, id: toId })
  pure $ (\task -> GT.AsyncTaskWithType { task, typ: GT.UpdateNode }) <$> eTask

linkNodeType :: Maybe GT.NodeType -> GT.NodeType
linkNodeType (Just GT.Corpus)   = GT.Annuaire
linkNodeType (Just GT.Annuaire) = GT.Corpus
linkNodeType  _   = GT.Error


linkNode :: R2.Component SubTreeParamsIn
linkNode = R.createElement linkNodeCpt
linkNodeCpt :: R.Component SubTreeParamsIn
linkNodeCpt = here.component "linkNode" cpt
  where
    cpt { boxes, dispatch, id, nodeType, subTreeParams } _ = do
      action <- T.useBox (LinkNode { nodeType: Nothing, params: Nothing})

      pure $

          linkNode' { action
                      , boxes
                      , dispatch
                      , id
                      , nodeType
                      , subTreeParams
                      } []

type Props =
  ( action :: T.Box Action
  | SubTreeParamsIn
  )

-- @XXX re-render issue -> clone component
linkNode' :: R2.Component Props
linkNode' = R.createElement linkNodeCpt'
linkNodeCpt' :: R.Component Props
linkNodeCpt' = here.component "__clone__" cpt
  where
    cpt { boxes, dispatch, id, nodeType, subTreeParams, action } _ = do

      action' <- T.useLive T.unequal action

      let

        button = case action' of
          LinkNode { params } -> case params of
            Just (SubTreeOut { in: inId }) -> submitButton
              (toParams nodeType inId)
              dispatch
            Nothing -> mempty
          _         -> mempty

      pure $ panel [
          subTreeView { action
                      , boxes
                      , dispatch
                      , id
                      , nodeType
                      , subTreeParams
                      } []
              ] button

toParams :: GT.NodeType -> GT.ID -> Action
toParams nodeType id
  = UpdateNode
    $ UpdateNodeParamsLink
      $ { methodLink: LinkNodeReq { nodeType, id } }
