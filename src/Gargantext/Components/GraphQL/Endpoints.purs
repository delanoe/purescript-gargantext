module Gargantext.Components.GraphQL.Endpoints where

import Gargantext.Components.GraphQL.Node
import Gargantext.Components.GraphQL.User
import Gargantext.Components.GraphQL.Tree
import Gargantext.Prelude

import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Unit (unit)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Gargantext.Components.GraphQL (getClient, queryGql)
import Gargantext.Components.GraphQL.IMT as GQLIMT
import Gargantext.Components.GraphQL.Task as GQLT
import Gargantext.Config.REST (AffRESTError, RESTError(..))
import Gargantext.Sessions (Session)
import Gargantext.Types (AsyncTaskWithType(..), AsyncTask(..), AsyncTaskType(..), NodeType)
import Gargantext.Utils.Reactix as R2
import GraphQL.Client.Args (onlyArgs, (=>>))
import GraphQL.Client.Query (mutation)
import GraphQL.Client.Variables (withVars)
import Simple.JSON as JSON

here :: R2.Here
here = R2.here "Gargantext.Components.GraphQL.Endpoints"

getIMTSchools :: Session -> AffRESTError (Array GQLIMT.School)
getIMTSchools session = do
  { imt_schools } <- queryGql session "get imt schools" $
                         GQLIMT.schoolsQuery
  liftEffect $ here.log2 "[getIMTSchools] imt_schools" imt_schools
  pure $ Right imt_schools

getNodeParent :: Session -> Int -> NodeType -> Aff (Array Node)
getNodeParent session nodeId parentType = do
  { node_parent } <- queryGql session "get node parent" $
                     nodeParentQuery `withVars` { id: nodeId
                                                , parent_type: show parentType }  -- TODO: remove "show"
  liftEffect $ here.log2 "[getNodeParent] node_parent" node_parent
  pure node_parent

getUserInfo :: Session -> Int -> AffRESTError UserInfo
getUserInfo session id = do
  { user_infos } <- queryGql session "get user infos" $ userInfoQuery `withVars` { id }
  liftEffect $ here.log2 "[getUserInfo] user infos" user_infos
  pure $ case A.head user_infos of
    Nothing -> Left (CustomError $ "user with id " <> show id <> " not found")
    -- NOTE Contact is at G.C.N.A.U.C.Types
    Just ui -> Right ui

getTreeFirstLevel :: Session -> Int -> AffRESTError TreeFirstLevel
getTreeFirstLevel session id = do
  { tree } <- queryGql session "get tree first level" $ treeFirstLevelQuery `withVars` { id }
  liftEffect $ here.log2 "[getTreeFirstLevel] tree first level" tree
  pure $ Right tree -- TODO: error handling

