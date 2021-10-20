module Gargantext.Components.GraphQL where

--import Data.Argonaut.Decode (class DecodeJson)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Gargantext.Components.GraphQL.AffjaxSimpleJSONClient (AffjaxClient(..))
import Gargantext.Components.GraphQL.User
import Gargantext.Prelude
import Gargantext.Utils.Reactix as R2
import GraphQL.Client.Args (type (==>))
--import GraphQL.Client.BaseClients.Urql
import GraphQL.Client.Query (query)
import GraphQL.Client.Types (class GqlQuery, Client(..))
import Simple.JSON as JSON


here :: R2.Here
here = R2.here "Gargantext.Components.GraphQL"

client :: Client AffjaxClient Schema Void Void
client = Client $ AffjaxClient "http://localhost:8008/gql" []

queryGql ::
  forall query returns.
  GqlQuery Schema query returns =>
  JSON.ReadForeign returns =>
  String -> query -> Aff returns
queryGql name q = do
  
  --client <- liftEffect $ createClient { headers: [], url: "http://localhost:8008/gql" }
  query client name q
  --query_ "http://localhost:8008/gql" (Proxy :: Proxy Schema) 

-- Schema
type Schema
  = { users :: { user_id :: Int } ==> Array User
    }
