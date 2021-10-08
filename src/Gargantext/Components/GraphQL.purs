module Gargantext.Components.GraphQL where

import Gargantext.Prelude

import Affjax.RequestBody (RequestBody(..))
import Data.Argonaut.Decode (class DecodeJson)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Utils.Reactix as R2
import GraphQL.Client.Args (type (==>), (=>>))
import GraphQL.Client.Query (query_)
import GraphQL.Client.Types (class GqlQuery)
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
        { user } <-
          queryGql "get user"
            { user: { user_id: 1 } =>> { userLight_id
                                       , userLight_username
                                       , userLight_password
                                       , userLight_email } }
        liftEffect $ here.log2 "[graphQLTest] user" user
        liftEffect $ T.write_ (Just user) userBox
    
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
queryGql = query_ "http://localhost:8008/gql" (Proxy :: Proxy Schema) 

-- Schema
type Schema
  = { user :: { user_id :: Int } ==> User
    }

type User
  = { userLight_id :: Int
    , userLight_username :: String
    , userLight_password :: String
    , userLight_email :: String
    }
showUser { userLight_id
         , userLight_username
         , userLight_password
         , userLight_email } = "[" <> show userLight_id <> "] " <> userLight_username <> " :: " <> userLight_email
showMUser u = maybe "" showUser u

-- Symbols 
userLight_id :: Proxy "userLight_id"
userLight_id = Proxy
userLight_username :: Proxy "userLight_username"
userLight_username = Proxy
userLight_password :: Proxy "userLight_password"
userLight_password = Proxy
userLight_email :: Proxy "userLight_email"
userLight_email = Proxy
