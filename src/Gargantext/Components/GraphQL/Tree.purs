module Gargantext.Components.GraphQL.Tree where

import Gargantext.Prelude

import Data.Maybe (Maybe)
import Gargantext.Types (NodeType)
import GraphQL.Client.Args ((=>>))
import GraphQL.Client.Variable (Var(..))

type TreeNode =
  { name      :: String
  , id        :: Int
  , node_type :: NodeType
  }
type TreeFirstLevel =
  { root     :: TreeNode
  , children :: Array TreeNode
  , parent   :: Maybe TreeNode
  }

treeFirstLevelQuery = { tree: { root_id: Var :: _ "id" Int} =>>
  { root: { name: unit
          , node_type: unit
          , id: unit
          }
  , children: { name: unit
              , node_type: unit
              , id: unit
              }
  , parent: { name: unit
            , node_type: unit
            , id: unit
            }
  }
 }
