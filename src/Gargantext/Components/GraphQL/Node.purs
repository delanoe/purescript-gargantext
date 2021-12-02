module Gargantext.Components.GraphQL.Node where

import Gargantext.Prelude

import GraphQL.Client.Args ((=>>))
import GraphQL.Client.Variable (Var(..))


type Node
  = { id        :: Int
    , name      :: String
    , parent_id :: Int
    , type_id   :: Int }

nodesQuery = { nodes: { node_id: Var :: _ "id" Int } =>>
               { id: unit
               , name: unit
               , parent_id: unit
               , type_id: unit }
             }

nodeParentQuery = { node_parent: { node_id: Var :: _ "id" Int
                                 , parent_type: Var :: _ "parent_type" String } =>>  -- TODO parent_type :: NodeType
                    { id: unit
                    , name: unit
                    , parent_id: unit
                    , type_id: unit }
                  }
