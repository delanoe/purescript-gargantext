module Gargantext.Components.Forest.Tree.Node.Action.Link where

import Data.Argonaut as Argonaut
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Tools (submitButton, panel)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (subTreeView, SubTreeParamsIn)
import Gargantext.Prelude
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, post)
import Gargantext.Types  as GT
import Gargantext.Utils.Argonaut (genericSumDecodeJson, genericSumEncodeJson)
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Link"

data LinkNodeReq = LinkNodeReq { nodeType :: GT.NodeType, id :: GT.ID }

derive instance eqLinkNodeReq :: Eq LinkNodeReq
derive instance genericLinkNodeReq :: Generic LinkNodeReq _
instance showLinkNodeReq :: Show LinkNodeReq where
  show = genericShow
instance decodeJsonLinkNodeReq :: Argonaut.DecodeJson LinkNodeReq where
  decodeJson = genericSumDecodeJson
instance encodeJsonLinkNodeReq :: Argonaut.EncodeJson LinkNodeReq where
  encodeJson = genericSumEncodeJson


linkNodeReq :: Session -> Maybe GT.NodeType -> GT.ID -> GT.ID -> Aff GT.AsyncTaskWithType
linkNodeReq session nt fromId toId = do
  task <- post session (NodeAPI GT.Node (Just fromId) "update")
                       (LinkNodeReq { nodeType: linkNodeType nt, id: toId })
  pure $ GT.AsyncTaskWithType {task, typ: GT.UpdateNode }

linkNodeType :: Maybe GT.NodeType -> GT.NodeType
linkNodeType (Just GT.Corpus)   = GT.Annuaire
linkNodeType (Just GT.Annuaire) = GT.Corpus
linkNodeType  _   = GT.Error


linkNode :: R2.Component SubTreeParamsIn
linkNode = R.createElement linkNodeCpt

linkNodeCpt :: R.Component SubTreeParamsIn
linkNodeCpt = here.component "linkNode" cpt
  where
    cpt p@{dispatch, subTreeParams, id, nodeType, session, handed} _ = do
      action <- T.useBox (LinkNode { nodeType: Nothing, params: Nothing})
      action' <- T.useLive T.unequal action

      let button = case action' of
              LinkNode { params } -> case params of
                Just val -> submitButton (LinkNode {nodeType: Just nodeType, params: Just val}) dispatch
                Nothing -> H.div {} []
              _                   -> H.div {} []

      pure $ panel [
          subTreeView { action
                      , dispatch
                      , handed
                      , id
                      , nodeType
                      , session
                      , subTreeParams
                      } []
              ] button
