module Gargantext.Components.GraphQL where

import Gargantext.Prelude

import Affjax.RequestHeader as ARH
import Control.Monad.Error.Class as Errors
import Data.Argonaut.Core as AC
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List.Types (NonEmptyList)
import Effect (Effect)
import Effect.Aff (Aff, throwError, error)
import Effect.Class (liftEffect)
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
import GraphQL.Client.Query (queryFullRes, queryWithDecoder, decodeGqlRes, queryFullRes)
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
  Aff returns
  -- REST.AffRESTError returns
-- TODO Send this by hand, parse errors if any
--   GQL API returns status != 200 for errors.
-- gqlQuery = queryWithDecoder  (Foreign.unsafeToForeign >>> JSON.read >>> lmap toJsonError)
gqlQuery c@(Client client) queryNameUnsafe q = do
  -- res@{ data_ } <-
  --   queryFullRes (Foreign.unsafeToForeign >>> JSON.read >>> lmap toJsonError) (const $ defQueryOpts client) c queryNameUnsafe q :: Aff (GqlRes returns)
  -- liftEffect $ here.log2 "[gqlQuery] res" res
  -- case data_ of
  --   Left err -> do
  --     liftEffect $ here.log2 "[gqlQuery] err" err
  --     throwError $ error "Error!"
  --   Right res -> pure res

  let opts = defQueryOpts client
      decodeFn = Foreign.unsafeToForeign >>> JSON.read >>> lmap toJsonError
      handleError err = do
        liftEffect $ here.log2 "[gqlQuery] error" err
        pure $ AC.fromString ""
  liftEffect $ here.log2 "[gqlQuery] calling query" q
  json <- Errors.catchError (clientQuery opts client queryName (getVarsTypeNames q <> toGqlQueryString q) (getVarsJson q)) handleError
  -- json <- clientQuery opts client queryName (getVarsTypeNames q <> toGqlQueryString q) (getVarsJson q)
  -- NOTE 'json' contains interesting data, in particular the gql operation, error, etc
  liftEffect $ here.log2 "[gqlQuery] json" json
  -- pure $ REST.readJSON json
  -- pure $ decodeJSONOrError $ Foreign.unsafeToForeign json
  -- pure $ lmap foreignToRESTError $ JSON.read $ Foreign.unsafeToForeign json
  
  -- case (JSON.read $ Foreign.unsafeToForeign json) of
  --   Right a -> pure a
  --   Left err -> throwError $ error $ show err

  -- what 'decodeGqlRes' does is take 'json.data' and decode it using our custom decode function
  case (decodeGqlRes decodeFn json) of
    Left err -> throwError $ error $ show err
    Right a -> pure a
  where
    queryName = safeQueryName queryNameUnsafe
    

-- decodeJSONOrError :: forall a. JSON.ReadForeign a => Foreign.Foreign -> Either RESTError a
-- decodeJSONOrError f =
--   lmap parseJSONError (JSON.read f :: JSON.E a)
--   where
--     parseJSONError :: Foreign.Foreign -> RESTError
--     parseJSONError f =
--       case (JSON.read f :: JSON.E FrontendError) of
--         Right err -> FE err
--         Left

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
  -> Aff returns
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
