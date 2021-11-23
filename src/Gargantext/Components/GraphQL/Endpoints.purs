module Gargantext.Components.GraphQL.Endpoints where

import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Gargantext.Components.GraphQL (queryGql)
import Gargantext.Components.GraphQL.Node
import Gargantext.Components.GraphQL.User
import Gargantext.Config.REST (RESTError(..))
import Gargantext.Prelude
import Gargantext.Sessions (Session)
import Gargantext.Types (NodeType(..))
import Gargantext.Utils.Reactix as R2
import GraphQL.Client.Variables (withVars)

here :: R2.Here
here = R2.here "Gargantext.Components.GraphQL.Endpoints"

getNodeParent :: Session -> Int -> NodeType -> Aff (Array Node)
getNodeParent session nodeId parentType = do
  { node_parent } <- queryGql session "get node parent" $
                     nodeParentQuery `withVars` { id: nodeId
                                                , parent_type: show parentType }  -- TODO: remove "show"
  liftEffect $ here.log2 "[getNodeParent] node_parent" node_parent
  pure $ node_parent

getUserInfo :: Session -> Int -> Aff (Either RESTError UserInfo)
getUserInfo session id = do
  { user_infos } <- queryGql session "get user infos" $ userInfoQuery `withVars` { id }
  liftEffect $ here.log2 "[getUserInfo] user infos" user_infos
  pure $ case A.head user_infos of
    Nothing -> Left (CustomError $ "user with id " <> show id <> " not found")
    -- NOTE Contact is at G.C.N.A.U.C.Types
    Just ui -> Right ui
