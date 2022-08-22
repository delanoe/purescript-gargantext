module Gargantext.Components.GraphQL.Team where

import Gargantext.Prelude

import GraphQL.Client.Args (NotNull, (=>>))
import GraphQL.Client.Variable (Var(..))

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
              { username: unit
              , shared_folder_id: unit }
            }