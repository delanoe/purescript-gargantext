module Gargantext.Components.GraphQL.Node where

import Gargantext.Prelude

import Data.Maybe (Maybe)
import GraphQL.Client.Args (Args, (=>>))
import GraphQL.Client.Variable (Var(..))
import Gargantext.Utils.GraphQL as GGQL
import Type.Proxy (Proxy(..))


type Corpus
  = { id           :: Int
    , name         :: String
    , parent_id    :: Int
    , pubmedAPIKey :: Maybe String
    , type_id      :: Int }

type Node
  = { id        :: Int
    , name      :: String
    , parent_id :: Int
    , type_id   :: Int }

type NodesCorpusQuery =
  { nodes_corpus :: Args
             { corpus_id :: Var "id" Int }
             { id :: Unit
             , name :: Unit
             , parent_id :: Unit
             , pubmedAPIKey :: Unit
             , type_id :: Unit } }

type NodesQuery =
  { nodes :: Args
       { node_id :: Var "id" Int }
       { id :: Unit
       , name :: Unit
       , parent_id :: Unit
       , type_id :: Unit } }

nodesQuery :: NodesQuery
nodesQuery = { nodes: { node_id: Var :: _ "id" Int } =>>
               GGQL.getFieldsStandard (Proxy :: _ Node)
             }

nodesCorpusQuery :: NodesCorpusQuery
nodesCorpusQuery = { nodes_corpus: { corpus_id: Var :: _ "id" Int } =>>
               GGQL.getFieldsStandard (Proxy :: _ Corpus)
             }

nodeParentQuery = { node_parent: { node_id: Var :: _ "id" Int
                                 , parent_type: Var :: _ "parent_type" String } =>>  -- TODO parent_type :: NodeType
                    GGQL.getFieldsStandard (Proxy :: _ Node)
                  }
