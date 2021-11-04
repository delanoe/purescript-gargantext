module Gargantext.Components.GraphQL where

import Data.Argonaut.Decode (JsonDecodeError)
import Data.Bifunctor (lmap)
import Data.List.Types (NonEmptyList)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign, ForeignError)
import Gargantext.Components.GraphQL.User (User, UserInfo)
import Gargantext.Prelude
import Gargantext.Utils.Reactix as R2
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

getClient :: Effect (Client UrqlClient Schema Void Void)
getClient = createClient { headers: [], url: "http://localhost:8008/gql" }

queryGql ::
  forall query returns.
  GqlQuery Schema query returns =>
  JSON.ReadForeign returns =>
  String -> query -> Aff returns
queryGql name q = do
  --query client name q
  client <- liftEffect getClient
  gqlQuery (client :: Client UrqlClient Schema Void Void) name q
  
  --query_ "http://localhost:8008/gql" (Proxy :: Proxy Schema) 

-- Schema
type Schema
  = { user_infos :: { user_id :: Int } ==> Array UserInfo
    , users :: { user_id :: Int } ==> Array User
    }
