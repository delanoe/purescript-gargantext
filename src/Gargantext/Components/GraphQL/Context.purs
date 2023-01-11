module Gargantext.Components.GraphQL.Context
  ( NodeContext
  , NodeContext'
  , nodeContextQuery
  , NodeContextCategoryM
  ) where

import Gargantext.Prelude

import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..), fromMaybe)
import GraphQL.Client.Args (Args, NotNull, (=>>))
import GraphQL.Client.Variable (Var(..))

import Data.Array as A

type NodeContext'
  = ( nc_id         :: Maybe Int
    , nc_node_id    :: Int
    , nc_context_id :: Int
    , nc_score      :: Maybe Number
    , nc_category   :: Maybe Int
    )
type NodeContext = Record NodeContext'

type NodeContextQuery
  = { contexts   :: Args
      { context_id :: Var "context_id" Int
      , node_id    :: Var "node_id" Int }
      { nc_id         :: Unit
      , nc_node_id    :: Unit
      , nc_context_id :: Unit
      , nc_score      :: Unit
      , nc_category   :: Unit
      }
    }
nodeContextQuery :: NodeContextQuery
nodeContextQuery
  = { contexts:
      { context_id: Var :: _ "context_id" Int
      , node_id: Var :: _ "node_id" Int } =>>
        { nc_id: unit
        , nc_node_id: unit
        , nc_context_id: unit
        , nc_score: unit
        , nc_category: unit
        }
    }

------------------------------------------------------------------------

type NodeContextCategoryM
  = { context_id :: NotNull Int
    , node_id    :: NotNull Int
    , category   :: Int
    }
