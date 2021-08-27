module Gargantext.Components.Forest.Tree.Node.Action.Link where

import Gargantext.Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Tools (submitButton, panel)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (subTreeView, SubTreeParamsIn)
import Gargantext.Config.REST (RESTError)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, post)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Link"

newtype LinkNodeReq = LinkNodeReq { nodeType :: GT.NodeType, id :: GT.ID }
derive instance Eq LinkNodeReq
derive instance Generic LinkNodeReq _
instance Show LinkNodeReq where show = genericShow
derive newtype instance JSON.ReadForeign LinkNodeReq
derive newtype instance JSON.WriteForeign LinkNodeReq


linkNodeReq :: Session -> Maybe GT.NodeType -> GT.ID -> GT.ID -> Aff (Either RESTError GT.AsyncTaskWithType)
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
    cpt { boxes, dispatch, id, nodeType, session, subTreeParams } _ = do
      action <- T.useBox (LinkNode { nodeType: Nothing, params: Nothing})
      action' <- T.useLive T.unequal action

      let button = case action' of
              LinkNode { params } -> case params of
                Just val -> submitButton (LinkNode {nodeType: Just nodeType, params: Just val}) dispatch
                Nothing -> H.div {} []
              _                   -> H.div {} []

      pure $ panel [
          subTreeView { action
                      , boxes
                      , dispatch
                      , id
                      , nodeType
                      , session
                      , subTreeParams
                      } []
              ] button
