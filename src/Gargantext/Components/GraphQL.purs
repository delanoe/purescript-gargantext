module Gargantext.Components.GraphQL where

import Gargantext.Prelude

import Affjax.RequestHeader as ARH
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Bifunctor (lmap)
import Data.List.Types (NonEmptyList)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign, ForeignError)
import Gargantext.Components.GraphQL.IMT as GQLIMT
import Gargantext.Components.GraphQL.Node (Node)
import Gargantext.Components.GraphQL.Tree (TreeFirstLevel)
import Gargantext.Components.GraphQL.User (User, UserInfo, UserInfoM)
import Gargantext.Components.GraphQL.Team (TeamMember, TeamDeleteM)
import Gargantext.Ends (Backend(..))
import Gargantext.Sessions (Session(..))
import Gargantext.Utils.Reactix as R2
import Gargnatext.Components.GraphQL.Contact (AnnuaireContact)
import GraphQL.Client.Args (type (==>))
import GraphQL.Client.BaseClients.Urql (UrqlClient, createClient)
import GraphQL.Client.Query (queryWithDecoder)
import GraphQL.Client.Types (class GqlQuery, Client, class QueryClient)
import Simple.JSON as JSON
import Unsafe.Coerce (unsafeCoerce)


here :: R2.Here
here = R2.here "Gargantext.Components.GraphQL"

--client :: Client AffjaxClient Schema Void Void
--client = Client $ AffjaxClient "http://localhost:8008/gql" []

-- | Run a graphQL query with a custom decoder and custom options
gqlQuery ::
  forall client schema query returns a b queryOpts mutationOpts.
  QueryClient client queryOpts mutationOpts =>
  GqlQuery schema query returns =>
  JSON.ReadForeign returns =>
  --(queryOpts -> queryOpts) ->
  (Client client schema a b) ->
  String ->
  query ->
  Aff returns
gqlQuery = queryWithDecoder  (unsafeToForeign >>> JSON.read >>> lmap toJsonError)

toJsonError :: NonEmptyList ForeignError -> JsonDecodeError
toJsonError = unsafeCoerce  -- map ForeignErrors to JsonDecodeError as you wish

getClient :: Session -> Effect (Client UrqlClient Schema Mutation Void)
getClient (Session { token, backend: Backend b }) = createClient { headers, url: b.baseUrl <> "/gql" }
  where
    headers = [ ARH.RequestHeader "Authorization" $ "Bearer " <> token ]

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
  = { imt_schools :: {} ==> Array GQLIMT.School
    , nodes :: { node_id :: Int } ==> Array Node
    , node_parent :: { node_id :: Int, parent_type :: String } ==> Array Node  -- TODO: parent_type :: NodeType
    , user_infos :: { user_id :: Int } ==> Array UserInfo
    , users :: { user_id :: Int } ==> Array User
    , tree :: { root_id :: Int } ==> TreeFirstLevel
    , annuaire_contacts :: { contact_id :: Int } ==> Array AnnuaireContact
    , team :: { team_node_id :: Int } ==> Array TeamMember
    }

type Mutation
  = { update_user_info :: UserInfoM ==> Int
    , delete_team_membership :: TeamDeleteM ==> Array Int }
