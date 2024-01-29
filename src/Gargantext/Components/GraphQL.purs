module Gargantext.Components.GraphQL where

import Gargantext.Prelude

import Affjax.RequestHeader as ARH
import Control.Monad.Error.Class as Errors
import Data.Argonaut.Core as AC
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, throwError, error)
import Effect.Class (liftEffect)
import FFI.Simple ((.?))
import Foreign as Foreign
import Gargantext.Components.GraphQL.Contact (AnnuaireContact)
import Gargantext.Components.GraphQL.Context as GQLCTX
import Gargantext.Components.GraphQL.IMT as GQLIMT
import Gargantext.Components.GraphQL.NLP as GQLNLP
import Gargantext.Components.GraphQL.Node as GQLNode
import Gargantext.Components.GraphQL.Tree (TreeFirstLevel, BreadcrumbInfo)
import Gargantext.Components.GraphQL.User (User, UserInfo, UserInfoM, UserPubmedAPIKeyM, UserEPOAPIUserM, UserEPOAPITokenM)
import Gargantext.Components.GraphQL.Team (Team, TeamDeleteM)
-- import Gargantext.Config.REST (AffRESTError, FrontendError, RESTError(..))
import Gargantext.Config.REST as REST
import Gargantext.Ends (Backend(..))
import Gargantext.Sessions (Session(..))
import Gargantext.Utils.Reactix as R2
import GraphQL.Client.Args (type (==>))
import GraphQL.Client.BaseClients.Urql (UrqlClient, createClient)
import GraphQL.Client.Query (queryFullRes, queryWithDecoder, decodeGqlRes, getFullRes, queryFullRes)
import GraphQL.Client.SafeQueryName (safeQueryName)
import GraphQL.Client.ToGqlString (toGqlQueryString)
import GraphQL.Client.Types (class GqlQuery, GqlRes, Client(..), class QueryClient, clientQuery, defQueryOpts)
import GraphQL.Client.Variables (class VarsTypeChecked, getVarsJson, getVarsTypeNames)
import Simple.JSON as JSON
import Unsafe.Coerce (unsafeCoerce)


here :: R2.Here
here = R2.here "Gargantext.Components.GraphQL"


-- TODO Rewrite this to REST.RESTError
type E a = Either JsonDecodeError a

--client :: Client AffjaxClient Schema Void Void
--client = Client $ AffjaxClient "http://localhost:8008/gql" []

-- | Run a graphQL query with a custom decoder and custom options
gqlQuery ::
  forall client schema query returns a b queryOpts mutationOpts.
  QueryClient client queryOpts mutationOpts =>
  GqlQuery schema query returns =>
  -- VarsTypeChecked query =>
  JSON.ReadForeign returns =>
  --(queryOpts -> queryOpts) ->
  (Client client schema a b) ->
  String ->
  query ->
  REST.AffRESTError returns
gqlQuery c@(Client client) queryNameUnsafe q = do
  let opts = defQueryOpts client

  json <- clientQuery opts client queryName (getVarsTypeNames q <> toGqlQueryString q) (getVarsJson q)

  pure $ parseGQLJson json
  where
    queryName = safeQueryName queryNameUnsafe

parseGQLJson json =
  -- NOTE 'json' contains interesting data, in particular the gql operation, error, etc
  case json .? "error" of
    -- what 'decodeGqlRes' does is take 'json.data' and decode it using our custom decode function
    Nothing -> lmap (REST.ReadJSONError <<< unsafeCoerce) (decodeGqlRes decodeFn json)
    Just err ->
      -- Surely, an error must be returned, but which one?
      Left $ case err .? "graphQLErrors" of
        Nothing -> REST.CustomError "[gqlQuery] no 'graphQLErrors'"
        Just gqlErrors -> case A.head gqlErrors of
          Nothing -> REST.CustomError "[gqlQuery] empty 'graphQLErrors'"
          Just gqlErr -> case gqlErr .? "extensions" of
            Nothing -> REST.CustomError "[gqlQuery] graphQLError no 'extensions'"
            Just ext -> case (Foreign.unsafeToForeign >>> JSON.read >>> lmap toJsonError $ ext) of
              Left err'  -> REST.CustomError $ "[gqlQuery] error reading 'extensions': " <> show err'
              Right err' -> REST.FE err'
  where
    decodeFn = Foreign.unsafeToForeign >>> JSON.read >>> lmap toJsonError

toJsonError :: Foreign.MultipleErrors -> JsonDecodeError
toJsonError = unsafeCoerce  -- map ForeignErrors to JsonDecodeError as you wish

getClient :: Session -> Effect (Client UrqlClient Schema Mutation Void)
getClient (Session { token, backend: Backend b }) = createClient { headers, url: b.baseUrl <> "/gql" }
  where
    headers = [ ARH.RequestHeader "Authorization" $ "Bearer " <> token
              , ARH.RequestHeader "X-Garg-Error-Scheme" $ "new" ]

queryGql ::
  forall query returns.
  GqlQuery Schema query returns =>
  JSON.ReadForeign returns =>
     Session
  -> String
  -> query
  -> REST.AffRESTError returns
queryGql session name q = do
  --query client name q
  client <- liftEffect $ getClient session
  gqlQuery (client :: Client UrqlClient Schema Mutation Void) name q

  --query_ "http://localhost:8008/gql" (Proxy :: Proxy Schema)

-- Schema
type Schema
  = { annuaire_contacts :: { contact_id :: Int } ==> Array AnnuaireContact
    , context_ngrams :: { context_id :: Int, list_id :: Int } ==> Array String
    , contexts :: { context_id :: Int, node_id :: Int } ==> Array GQLCTX.NodeContext
    , contexts_for_ngrams :: { corpus_id :: Int, ngrams_terms :: Array String } ==> Array GQLCTX.Context
    , imt_schools :: {} ==> Array GQLIMT.School
    , languages :: {} ==> Array GQLNLP.Language
    , node_parent :: { node_id :: Int, parent_type :: String } ==> Array GQLNode.Node  -- TODO: parent_type :: NodeType
    , nodes :: { node_id :: Int } ==> Array GQLNode.Node
    , nodes_corpus :: { corpus_id :: Int } ==> Array GQLNode.Corpus
    , user_infos :: { user_id :: Int } ==> Array UserInfo
    , users :: { user_id :: Int } ==> Array User
    , team :: { team_node_id :: Int } ==> Team
    , tree :: { root_id :: Int } ==> TreeFirstLevel
    , tree_branch :: { node_id :: Int } ==> BreadcrumbInfo
    }

type Mutation
  = { update_user_info :: UserInfoM ==> Int
    , update_user_pubmed_api_key :: UserPubmedAPIKeyM ==> Int
    , update_user_epo_api_user :: UserEPOAPIUserM ==> Int
    , update_user_epo_api_token :: UserEPOAPITokenM ==> Int
    , delete_team_membership :: TeamDeleteM ==> Array Int
    , update_node_context_category :: GQLCTX.NodeContextCategoryM ==> Array Int }
