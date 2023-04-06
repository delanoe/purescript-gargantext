module Gargantext.Components.GraphQL.Team where

import Gargantext.Prelude

import GraphQL.Client.Args (Args, NotNull, (=>>))
import GraphQL.Client.Variable (Var(..))
import Gargantext.Utils.GraphQL as GGQL
import Type.Proxy (Proxy(..))

type Team
  = { team_owner_username :: String
    , team_members         :: Array TeamMember
    }

type TeamMember
  = { username         :: String
    , shared_folder_id :: Int
    }

type TeamDeleteM
  = { token :: NotNull String
    , shared_folder_id :: Int
    , team_node_id     :: Int
    }

type TeamQuery =
  { team :: Args
  { team_node_id :: Var "id" Int}
  { team_owner_username :: Unit
  , team_members ::
       { username :: Unit
       , shared_folder_id :: Unit } }
  }

teamQuery :: TeamQuery
teamQuery = { team: { team_node_id: Var :: _ "id" Int } =>>
              GGQL.getFieldsStandard (Proxy :: _ Team)
            }
