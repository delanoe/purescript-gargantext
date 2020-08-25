module Gargantext.Components.Forest.Tree.Node.Action.Link
  where

import Data.Argonaut as Argonaut
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Tools (submitButton, panel)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (subTreeView, SubTreeParamsIn)
import Gargantext.Prelude
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, post)
import Gargantext.Routes as GR
import Gargantext.Types  as GT
import Gargantext.Utils.Argonaut (genericSumDecodeJson, genericSumEncodeJson)
import Reactix as R
import Reactix.DOM.HTML as H


data LinkNodeReq = LinkNodeReq { nodeType :: GT.NodeType
                               , id       :: GT.ID
                               }


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
                       (LinkNodeReq { nodeType: linkNodeType nt
                                    , id: toId
                                    }
                       )
  pure $ GT.AsyncTaskWithType {task, typ: GT.UpdateNode }
    where
      p = GR.NodeAPI GT.Node (Just fromId) "update"

linkNodeType :: Maybe GT.NodeType -> GT.NodeType
linkNodeType (Just GT.Corpus)   = GT.Annuaire
linkNodeType (Just GT.Annuaire) = GT.Corpus
linkNodeType  _   = GT.Error


linkNode :: Record SubTreeParamsIn -> R.Element
linkNode p = R.createElement linkNodeCpt p []

linkNodeCpt :: R.Component SubTreeParamsIn
linkNodeCpt = R.hooksComponent "G.C.F.T.N.A.L.linkNode" cpt
  where
    cpt p@{dispatch, subTreeParams, id, nodeType, session, handed} _ = do

      action@(valAction /\ setAction) :: R.State Action <- R.useState' (LinkNode {nodeType:Nothing,params:Nothing})

      let button = case valAction of
              LinkNode {params} -> case params of
                Just val -> submitButton (LinkNode {nodeType: Just nodeType, params: Just val}) dispatch
                Nothing -> H.div {} []
              _                   -> H.div {} []

      pure $ panel [
          subTreeView { action
                      , dispatch
                      , id
                      , nodeType
                      , session
                      , subTreeParams
                      , handed
                      }
              ] button
