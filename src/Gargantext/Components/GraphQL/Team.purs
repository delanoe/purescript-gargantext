module Gargantext.Components.GraphQL.Team where

import Gargantext.Prelude

import GraphQL.Client.Args (NotNull, (=>>))
import GraphQL.Client.Variable (Var(..))

type Team
  = { team_leader_username :: String
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

teamQuery = { team: { team_node_id: Var :: _ "id" Int } =>>
              { team_leader_username: unit
              , team_members: { username: unit
                              , shared_folder_id: unit 
                              }
              }
            }