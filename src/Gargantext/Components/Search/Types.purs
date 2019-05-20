module Gargantext.Components.Search.Types where

import Control.Monad.Cont.Trans (lift)
import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Routing.Hash (setHash)
import Thermite (PerformAction, modifyState)

import Gargantext.Prelude
import Gargantext.Types (class ToQuery)
import Gargantext.Config.REST (post)
import Gargantext.Components.Modals.Modal (modalHide)
import Gargantext.Pages.Layout.Specs.AddCorpus.States (Response, State)
import Gargantext.Utils (id)
import URI.Extra.QueryPairs as QP

data Database = All | PubMed | HAL | IsTex

instance showDatabase :: Show Database where
  show All = "All"
  show PubMed = "PubMed"
  show HAL = "HAL"
  show IsTex = "IsTex"

readDatabase :: String -> Maybe Database
readDatabase "All" = Just All
readDatabase "PubMed" = Just PubMed
readDatabase "HAL" = Just HAL
readDatabase "IsTex" = Just IsTex
readDatabase _ = Nothing

derive instance eqDatabase :: Eq Database

allDatabases :: Array Database
allDatabases = [All, HAL, IsTex, PubMed]

data SearchOrder
  = DateAsc
  | DateDesc
  | TitleAsc
  | TitleDesc
  | ScoreAsc
  | ScoreDesc

instance showSearchOrder :: Show SearchOrder where
  show DateAsc = "DateAsc"
  show DateDesc = "DateDesc"
  show TitleAsc = "TitleAsc"
  show TitleDesc = "TitleDesc"
  show ScoreAsc = "ScoreAsc"
  show ScoreDesc = "ScoreDesc"

newtype SearchQuery = SearchQuery
  { query :: String
  , databases :: Array Database
  , corpus_id :: Maybe Int
  , offset :: Maybe Int
  , limit :: Maybe Int
  , order :: Maybe SearchOrder }

derive instance newtypeSearchQuery :: Newtype SearchQuery _

defaultSearchQuery :: SearchQuery
defaultSearchQuery = SearchQuery
  { query: ""
  , databases: allDatabases
  , corpus_id: Nothing
  , offset: Nothing
  , limit: Nothing
  , order: Nothing }

instance searchQueryToQuery :: ToQuery SearchQuery where
  toQuery (SearchQuery {offset, limit, order}) =
    QP.print id id $ QP.QueryPairs $
         pair "offset" offset <> pair "limit" limit <> pair "order" order
    where pair :: forall a. Show a => String -> Maybe a -> Array (Tuple QP.Key (Maybe QP.Value))
          pair k = maybe [] $ \v ->
            [ QP.keyFromString k /\ Just (QP.valueFromString $ show v) ]

instance encodeJsonSearchQuery :: EncodeJson SearchQuery where
  encodeJson (SearchQuery {query, corpus_id})
    =   "query"       :=  query
    ~>  "corpus_id"   :=  fromMaybe 0 corpus_id
    ~> jsonEmptyObject
