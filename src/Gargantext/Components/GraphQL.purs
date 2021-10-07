module Gargantext.Components.GraphQL where

import Gargantext.Prelude

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
            { user: { name: "x" } =>> { name, user_id } }
        liftEffect $ here.log2 "[graphQLTest] user" user
        liftEffect $ T.write_ (Just user) userBox
    
    pure $ H.div { className: "col-12 d-flex justify-content-center" }
      [ H.h1 {} [ H.text "graph ql test" ]
      , H.p {} [ H.text $ showMUser user' ]
      ]

queryGql ::
  forall query returns.
  GqlQuery Schema query returns =>
  DecodeJson returns =>
  String -> query -> Aff returns
queryGql = query_ "http://localhost:8008/gql" (Proxy :: Proxy Schema) 

-- Schema
type Schema
  = { user :: { name :: String } ==> User
    }

type User
  = { name    :: String
    , user_id :: Int
    }
showUser { name, user_id } = name <> " :: " <> show user_id
showMUser u = maybe "" showUser u

-- Symbols 
name :: Proxy "name"
name = Proxy

user_id :: Proxy "user_id"
user_id = Proxy
