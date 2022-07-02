module Gargantext.Components.GraphQL.Endpoints where

import Gargantext.Prelude

import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Gargantext.Components.GraphQL (getClient, queryGql)
import Gargantext.Components.GraphQL.IMT as GQLIMT
import Gargantext.Components.GraphQL.Node (Node, nodeParentQuery, nodesQuery)
import Gargantext.Components.GraphQL.Team (TeamMember, teamQuery)
import Gargantext.Components.GraphQL.Tree (TreeFirstLevel, treeFirstLevelQuery)
import Gargantext.Components.GraphQL.User (UserInfo, userInfoQuery)
import Gargantext.Config.REST (RESTError(..), AffRESTError)
import Gargantext.Sessions (Session(..))
import Gargantext.Types (NodeType)
import Gargantext.Utils.Reactix as R2
import Gargnatext.Components.GraphQL.Contact (AnnuaireContact, annuaireContactQuery)
import GraphQL.Client.Args (onlyArgs)
import GraphQL.Client.Query (mutation)
import GraphQL.Client.Variables (withVars)

here :: R2.Here
here = R2.here "Gargantext.Components.GraphQL.Endpoints"

getIMTSchools :: Session -> AffRESTError (Array GQLIMT.School)
getIMTSchools session = do
  { imt_schools } <- queryGql session "get imt schools" $
                         GQLIMT.schoolsQuery
  liftEffect $ here.log2 "[getIMTSchools] imt_schools" imt_schools
  pure $ Right imt_schools

getNode :: Session -> Int -> AffRESTError Node
getNode session nodeId = do
  { nodes } <- queryGql session "get nodes" $
              nodesQuery `withVars` { id: nodeId }
  liftEffect $ here.log2 "[getNode] node" nodes
  pure $ case A.head nodes of
    Nothing -> Left (CustomError $ "node with id" <> show nodeId <>" not found")
    Just node -> Right node

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

getAnnuaireContact :: Session -> Int -> AffRESTError AnnuaireContact
getAnnuaireContact session id = do
  { annuaire_contacts } <- queryGql session "get annuaire contact" $
    annuaireContactQuery `withVars` { id }
  liftEffect $ here.log2 "[getAnnuaireContact] data" annuaire_contacts
  pure $ case A.head annuaire_contacts of
    Nothing -> Left (CustomError $ "contact id=" <> show id <> " not found")
    Just r  -> Right r

getTreeFirstLevel :: Session -> Int -> AffRESTError TreeFirstLevel
getTreeFirstLevel session id = do
  { tree } <- queryGql session "get tree first level" $ treeFirstLevelQuery `withVars` { id }
  liftEffect $ here.log2 "[getTreeFirstLevel] tree first level" tree
  pure $ Right tree -- TODO: error handling

getTeam :: Session -> Int -> AffRESTError TeamMember
getTeam session id = do
  { team } <- queryGql session "get team" $ teamQuery `withVars` { id }
  liftEffect $ here.log2 "[getTree] data" team
  pure $ case A.head team of
    Nothing -> Left (CustomError $ "team node id=" <> show id <> " not found")
    Just t -> Right t

type SharedFolderId = Int
type TeamNodeId = Int

deleteTeamMembership :: Session -> SharedFolderId -> TeamNodeId -> AffRESTError Int
deleteTeamMembership session sharedFolderId teamNodeId = do
  let token = getToken session
  client <- liftEffect $ getClient session
  { delete_team_membership } <- mutation
    client
    "delete_team_membership"
    { delete_team_membership: onlyArgs { token: token
                                       , shared_folder_id: sharedFolderId
                                       , team_node_id: teamNodeId } }
  pure $ case A.head delete_team_membership of
    Nothing -> Left (CustomError $ "Failed  to delete team membership. team node id=" <> show teamNodeId <> " shared folder id=" <> show sharedFolderId)
    Just i -> Right i
  where
    getToken (Session { token }) = token
