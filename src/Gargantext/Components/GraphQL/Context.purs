module Gargantext.Components.GraphQL.Context
  ( Context_
  , Context
  , Hyperdata_
  , Hyperdata
  , NodeContext
  , NodeContext_
  , nodeContextQuery
  , NodeContextCategoryM
  , contextsForNgramsQuery
  ) where

import Gargantext.Prelude

import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..), fromMaybe)
import GraphQL.Client.Args (Args, NotNull, (=>>))
import GraphQL.Client.Variable (Var(..))
import Gargantext.Utils.GraphQL as GGQL
import Type.Proxy (Proxy(..))

import Data.Array as A

type Context_
  = ( c_id        :: Int
    , c_name      :: String
    , c_typename  :: Int
    , c_hash_id   :: String
    , c_user_id   :: Int
    , c_parent_id :: Int
    , c_hyperdata :: Maybe Hyperdata )

type Context = Record Context_

type Hyperdata_
  = ( hrd_abstract           :: String
    , hrd_authors            :: String
    , hrd_bdd                :: String
    , hrd_doi                :: String
    , hrd_institutes         :: String
    , hrd_language_iso2      :: String
    , hrd_page               :: Int
    , hrd_publication_date   :: String
    , hrd_publication_day    :: Int
    , hrd_publication_hour   :: Int
    , hrd_publication_minute :: Int
    , hrd_publication_month  :: Int
    , hrd_publication_second :: Int
    , hrd_publication_year   :: Int
    , hrd_source             :: String
    , hrd_title              :: String
    , hrd_url                :: String
    , hrd_uniqId             :: String
    , hrd_uniqIdBdd          :: String )

type Hyperdata = Record Hyperdata_

type NodeContext_
  = ( nc_id         :: Maybe Int
    , nc_node_id    :: Int
    , nc_context_id :: Int
    , nc_score      :: Maybe Number
    , nc_category   :: Maybe Int
    )
type NodeContext = Record NodeContext_

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
      GGQL.getFieldsStandard (Proxy :: _ NodeContext)
    }

type ContextsForNgramsQuery
  = { contexts_for_ngrams :: Args
      { corpus_id :: Var "corpus_id" Int
      , ngrams_ids :: Var "ngrams_ids" (Array Int) }
      { c_id                        :: Unit
      , c_name                      :: Unit
      , c_typename                  :: Unit
      , c_hash_id                   :: Unit
      , c_user_id                   :: Unit
      , c_parent_id                 :: Unit
      , c_hyperdata                 ::
           { hrd_abstract           :: Unit
           , hrd_authors            :: Unit
           , hrd_bdd                :: Unit
           , hrd_doi                :: Unit
           , hrd_institutes         :: Unit
           , hrd_language_iso2      :: Unit
           , hrd_page               :: Unit
           , hrd_publication_date   :: Unit
           , hrd_publication_day    :: Unit
           , hrd_publication_hour   :: Unit
           , hrd_publication_minute :: Unit
           , hrd_publication_month  :: Unit
           , hrd_publication_second :: Unit
           , hrd_publication_year   :: Unit
           , hrd_source             :: Unit
           , hrd_title              :: Unit
           , hrd_url                :: Unit
           , hrd_uniqId             :: Unit
           , hrd_uniqIdBdd          :: Unit }
      }
    }
contextsForNgramsQuery :: ContextsForNgramsQuery
contextsForNgramsQuery
  = { contexts_for_ngrams:
      { corpus_id: Var :: _ "corpus_id" Int
      , ngrams_ids: Var :: _ "ngrams_ids" (Array Int) } =>>
      GGQL.getFieldsStandard (Proxy :: _ Context)
    }

------------------------------------------------------------------------

type NodeContextCategoryM
  = { context_id :: NotNull Int
    , node_id    :: NotNull Int
    , category   :: Int
    }
