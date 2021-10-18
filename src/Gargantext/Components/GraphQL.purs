module Gargantext.Components.GraphQL where

import Data.Argonaut.Decode (class DecodeJson)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.GraphQL.User
import Gargantext.Prelude
import Gargantext.Utils.Reactix as R2
import GraphQL.Client.Args (type (==>), (=>>))
import GraphQL.Client.BaseClients.Urql
import GraphQL.Client.Query (query, query_)
import GraphQL.Client.Types (class GqlQuery, Client)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T
import Type.Proxy (Proxy(..))

type Props = ()


here :: R2.Here
here = R2.here "Gargantext.Components.GraphQL"

graphQLTest :: R2.Component Props
graphQLTest = R.createElement graphQLTestCpt
graphQLTestCpt :: R.Component Props
graphQLTestCpt = here.component "graphQLTest" cpt where
  cpt {} _ = do
    userBox <- T.useBox Nothing
    user' <- T.useLive T.unequal userBox
    R.useEffect' do
      launchAff_ $ do
        { users } <-
          queryGql "get user"
            { users: { user_id: 1 } =>> { userLight_id
                                        , userLight_username
                                        , userLight_password
                                        , userLight_email } }
        liftEffect $ do
          here.log2 "[graphQLTest] users" users
          case A.head users of
            Nothing -> here.log2 "[graphQLTest] user not found with id" 1
            Just u  -> T.write_ (Just u) userBox
    
    pure $ R2.row
      --[ H.div { className: "col-12 d-flex justify-content-center" }
      [ R2.col 12
        [ R2.row
          [ R2.col 12
            [ H.h1 {} [ H.text "graph ql test" ]
            ]
          ]
        , R2.row
          [ R2.col 12
            [ H.p {} [ H.text $ showMUser user' ]
            ]
          ]
        ]
      ]

queryGql ::
  forall query returns.
  GqlQuery Schema query returns =>
  DecodeJson returns =>
  String -> query -> Aff returns
queryGql name q = do
  client <- liftEffect $ createClient { headers: [], url: "http://localhost:8008/gql" }
  query (client :: Client UrqlClient Schema _ _) name q
  --query_ "http://localhost:8008/gql" (Proxy :: Proxy Schema) 

-- Schema
type Schema
  = { users :: { user_id :: Int } ==> Array User
    }
