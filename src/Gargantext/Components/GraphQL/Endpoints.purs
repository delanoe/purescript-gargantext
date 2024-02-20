module Gargantext.Components.GraphQL.Endpoints where

import Data.Array as A
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Gargantext.Components.GraphQL (queryGql, mutationGql)
import Gargantext.Components.GraphQL.Contact (AnnuaireContact, annuaireContactQuery)
import Gargantext.Components.GraphQL.Context as GQLCTX
import Gargantext.Components.GraphQL.IMT as GQLIMT
import Gargantext.Components.GraphQL.NLP as GQLNLP
import Gargantext.Components.GraphQL.Node (Corpus, Node, nodeParentQuery, nodesQuery, nodesCorpusQuery)
import Gargantext.Components.GraphQL.Team (Team, teamQuery)
import Gargantext.Components.GraphQL.Tree (TreeFirstLevel, treeFirstLevelQuery, BreadcrumbInfo, breadcrumbQuery)
import Gargantext.Components.GraphQL.User (UserInfo, userInfoQuery, User, userQuery)
import Gargantext.Components.Lang (Lang)
import Gargantext.Config.REST (RESTError(..), AffRESTError)
import Gargantext.Core.NgramsTable.Types (NgramsTerm(..))
import Gargantext.Prelude
import Gargantext.Sessions (Session(..))
import Gargantext.Types (CorpusId, NodeType)
import Gargantext.Utils.Reactix as R2
import GraphQL.Client.Args (onlyArgs)
import GraphQL.Client.Variables (withVars)


here :: R2.Here
here = R2.here "Gargantext.Components.GraphQL.Endpoints"

getIMTSchools :: Session -> AffRESTError (Array GQLIMT.School)
getIMTSchools session = do
  eRes <- queryGql session "get imt schools" $ GQLIMT.schoolsQuery
  pure $ rmap _.imt_schools eRes
  -- liftEffect $ here.log2 "[getIMTSchools] imt_schools" imt_schools
  -- pure $ Right imt_schools

getNode :: Session -> Int -> AffRESTError Node
getNode session nodeId = do
  eRes <- queryGql session "get nodes" $ nodesQuery `withVars` { id: nodeId }
  case eRes of
    Left err -> pure $ Left err
    Right { nodes } -> do
      liftEffect $ here.log2 "[getNode] node" nodes
      pure $ case A.head nodes of
        Nothing -> Left (CustomError $ "node with id" <> show nodeId <>" not found")
        Just node -> Right node

getNodeCorpus :: Session -> Int -> AffRESTError Corpus
getNodeCorpus session corpusId = do
  eRes <- queryGql session "get nodes corpus" $
              nodesCorpusQuery `withVars` { id: corpusId }                      
  case eRes of
    Left err -> pure $ Left err
    Right { nodes_corpus } -> do
      liftEffect $ here.log2 "[getNodesCorpus] nodes_corpus" nodes_corpus
      pure $ case A.head nodes_corpus of
        Nothing -> Left (CustomError $ "corpus with id" <> show corpusId <>" not found")
        Just corpus -> Right corpus

getNodeParent :: Session -> Int -> NodeType -> AffRESTError (Array Node)
getNodeParent session nodeId parentType = do
  eRes <- queryGql session "get node parent" $
                     nodeParentQuery `withVars` { id: nodeId
                                                , parent_type: show parentType }  -- TODO: remove "show"
  -- liftEffect $ here.log2 "[getNodeParent] node_parent" node_parent
  --pure node_parent
  pure $ rmap _.node_parent eRes

getUser :: Session -> Int -> AffRESTError User
getUser session id = do
  eRes <- queryGql session "get user" $ userQuery `withVars` { id }
  case eRes of
    Left err -> pure $ Left err
    Right { users } -> do
      liftEffect $ here.log2 "[getUser] users" users
      pure $ case A.head users of
        Nothing -> Left (CustomError $ "user with id " <> show id <> " not found")
        Just u -> Right u
    
updateUserPubmedAPIKey :: Session -> Int -> String -> AffRESTError Unit
updateUserPubmedAPIKey session user_id api_key = do
  eRes <- mutationGql session
                      "update_user_pubmed_api_key"
                      { update_user_pubmed_api_key: onlyArgs { user_id
                                                             , api_key } }
  -- { update_user_pubmed_api_key }
  pure $ rmap (const unit) eRes
  
updateUserEPOAPIUser :: Session -> Int -> String -> AffRESTError Unit
updateUserEPOAPIUser session user_id api_user = do
  eRes <- mutationGql session
                      "update_user_epo_api_user"
                      { update_user_epo_api_user: onlyArgs { user_id
                                                           , api_user } }
  pure $ rmap (const unit) eRes
  
updateUserEPOAPIToken :: Session -> Int -> String -> AffRESTError Unit
updateUserEPOAPIToken session user_id api_token = do
  eRes <- mutationGql session
                      "update_user_epo_api_token"
                      { update_user_epo_api_token: onlyArgs { user_id
                                                            , api_token } }
  pure $ rmap (const unit) eRes

getUserInfo :: Session -> Int -> AffRESTError UserInfo
getUserInfo session id = do
  eRes <- queryGql session "get user infos" $ userInfoQuery `withVars` { id }
  case eRes of
    Left err -> pure $ Left err
    Right { user_infos } -> do
      liftEffect $ here.log2 "[getUserInfo] user infos" user_infos
      pure $ case A.head user_infos of
        Nothing -> Left (CustomError $ "user with id " <> show id <> " not found")
        -- NOTE Contact is at G.C.N.A.U.C.Types
        Just ui -> Right ui
    
getAnnuaireContact :: Session -> Int -> AffRESTError AnnuaireContact
getAnnuaireContact session id = do
  eRes <- queryGql session "get annuaire contact" $
    annuaireContactQuery `withVars` { id }
  case eRes of
    Left err -> pure $ Left err
    Right { annuaire_contacts } -> do
      liftEffect $ here.log2 "[getAnnuaireContact] data" annuaire_contacts
      pure $ case A.head annuaire_contacts of
        Nothing -> Left (CustomError $ "contact id=" <> show id <> " not found")
        Just r  -> Right r

getTreeFirstLevel :: Session -> Int -> AffRESTError TreeFirstLevel
getTreeFirstLevel session id = do
  eRes <- queryGql session "get tree first level" $ treeFirstLevelQuery `withVars` { id }
  case eRes of
    Left err -> pure $ Left err
    Right { tree } -> do
      liftEffect $ here.log2 "[getTreeFirstLevel] tree first level" tree
      pure $ Right tree -- TODO: error handling
    
getTeam :: Session -> Int -> AffRESTError Team
getTeam session id = do
  eRes <- queryGql session "get team" $ teamQuery `withVars` { id }
  case eRes of
    Left err -> pure $ Left err
    Right { team } -> do
      liftEffect $ here.log2 "[getTree] data" team
      pure $ Right team

type SharedFolderId = Int
type TeamNodeId = Int

deleteTeamMembership :: Session -> SharedFolderId -> TeamNodeId -> AffRESTError Int
deleteTeamMembership session sharedFolderId teamNodeId = do
  let token = getToken session
  eRes <- mutationGql session
                      "delete_team_membership"
                      { delete_team_membership: onlyArgs { token: token
                                                         , shared_folder_id: sharedFolderId
                                                         , team_node_id: teamNodeId } }
  pure $ case eRes of
    Left err -> Left err
    Right { delete_team_membership } ->
      case A.head delete_team_membership of
        Nothing -> Left (CustomError $ "Failed  to delete team membership. team node id=" <> show teamNodeId <> " shared folder id=" <> show sharedFolderId)
        Just _ -> Right sharedFolderId
  where
    getToken (Session { token }) = token

getNodeContext :: Session -> Int -> Int -> AffRESTError GQLCTX.NodeContext
getNodeContext session context_id node_id = do
  let query = GQLCTX.nodeContextQuery `withVars` { context_id, node_id }
  eRes <- queryGql session "get node context" query
  case eRes of
    Left err -> pure $ Left err
    Right { contexts } -> do
      --liftEffect $ here.log2 "[getNodeContext] node context" contexts
      case A.head contexts of
        Nothing -> pure $ Left $ CustomError "no node context found"
        Just context -> pure $ Right context -- TODO: error handling
    
type ContextsForNgramsGQL = { contexts_for_ngrams :: Array GQLCTX.Context }
getContextsForNgrams :: Session -> CorpusId -> Array String -> AffRESTError (Array GQLCTX.Context)
getContextsForNgrams session corpus_id ngrams_terms = do
  let query = GQLCTX.contextsForNgramsQuery `withVars` { corpus_id
                                                       , ngrams_terms: GQLCTX.NgramsTerms ngrams_terms }
  eRes <- queryGql session "get contexts for ngrams" query
  pure $ rmap _.contexts_for_ngrams eRes

updateNodeContextCategory :: Session -> Int -> Int -> Int -> AffRESTError Int
updateNodeContextCategory session context_id node_id category = do
  eRes <- mutationGql session
                      "update_node_context_category"
                      { update_node_context_category: onlyArgs { context_id
                                                               , node_id
                                                               , category } }
  pure $ case eRes of
    Left err -> Left err
    Right { update_node_context_category } -> case A.head update_node_context_category of
      Nothing -> Left (CustomError $ "Failed to update node category")
      Just _ -> Right context_id

getLanguages :: Session -> AffRESTError (Map.Map Lang GQLNLP.LanguageProperties)
getLanguages session = do
  let query = GQLNLP.nlpQuery
  eRes <- queryGql session "get languages" query
  case eRes of
    Left err -> pure $ Left err
    Right { languages } -> do    
      liftEffect $ here.log2 "[getLanguages] languages" languages
    
      pure $ Right $ Map.fromFoldable $ (\{ lt_lang, lt_server } -> Tuple lt_lang lt_server) <$> languages

getContextNgrams :: Session -> Int -> Int -> AffRESTError (Array NgramsTerm)
getContextNgrams session context_id list_id = do
  let query = GQLCTX.contextNgramsQuery `withVars` { context_id, list_id }
  eRes <- queryGql session "get context ngrams" query
  pure $ rmap (\{ context_ngrams } -> NormNgramsTerm <$> context_ngrams) eRes

getBreadcrumb :: Session -> Int -> AffRESTError BreadcrumbInfo
getBreadcrumb session id = do
  eRes <- queryGql session "get breadcrumb branch" $ breadcrumbQuery `withVars` { id }
  -- liftEffect $ here.log2 "[getBreadcrumb] breadcrumb" tree_branch
  pure $ rmap _.tree_branch eRes
