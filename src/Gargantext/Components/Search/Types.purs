module Gargantext.Components.Search.Types where

import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>), encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)

import Gargantext.Prelude
import Gargantext.Types (class ToQuery)
import Gargantext.Config (End(..), NodeType(..), toUrl)
import Gargantext.Config.REST (put)
import Gargantext.Utils (id)
import URI.Extra.QueryPairs as QP

data Database = All | PubMed | HAL | IsTex

instance showDatabase :: Show Database where
  show All    = "All"
  show PubMed = "PubMed"
  show HAL    = "HAL"
  show IsTex  = "IsTex"

readDatabase :: String -> Maybe Database
readDatabase "All" = Just All
readDatabase "PubMed" = Just PubMed
readDatabase "HAL" = Just HAL
readDatabase "IsTex" = Just IsTex
readDatabase _ = Nothing

derive instance eqDatabase :: Eq Database

instance encodeJsonDatabase :: EncodeJson Database where
  encodeJson a = encodeJson (show a)


allDatabases :: Array Database
allDatabases = [All, PubMed]

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
  , files_id  :: Array String
  , offset :: Maybe Int
  , limit :: Maybe Int
  , order :: Maybe SearchOrder }

derive instance newtypeSearchQuery :: Newtype SearchQuery _

defaultSearchQuery :: SearchQuery
defaultSearchQuery = SearchQuery
  { query: ""
  , databases: allDatabases
  , corpus_id: Nothing
  , files_id : []
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
  encodeJson (SearchQuery {query, databases, corpus_id, files_id})
    =   "query"      := query
    ~> "databases"   := databases
    ~>  "corpus_id"  := fromMaybe 0 corpus_id
    ~>  "files_id"   := files_id
    ~> jsonEmptyObject


data Category = Trash | Normal | Favorite
derive instance genericFavorite :: Generic Category _
instance showCategory :: Show Category where
  show = genericShow
instance eqCategory :: Eq Category where
  eq = genericEq
instance encodeJsonCategory :: EncodeJson Category where
  encodeJson Trash = encodeJson 0
  encodeJson Normal = encodeJson 1
  encodeJson Favorite = encodeJson 2

favCategory :: Category -> Category
favCategory Normal = Favorite
favCategory Trash = Favorite
favCategory Favorite = Normal

trashCategory :: Category -> Category
trashCategory Normal = Trash
trashCategory Trash = Normal
trashCategory Favorite = Trash


decodeCategory :: Int -> Category
decodeCategory 0 = Trash
decodeCategory 1 = Normal
decodeCategory 2 = Favorite
decodeCategory _ = Normal



newtype CategoryQuery = CategoryQuery {
    nodeIds :: Array Int
  , category :: Category
  }

instance encodeJsonCategoryQuery :: EncodeJson CategoryQuery where
  encodeJson (CategoryQuery post) =
    "ntc_nodesId" := post.nodeIds
    ~> "ntc_category" := encodeJson post.category
    ~> jsonEmptyObject

categoryUrl :: Int -> String
categoryUrl nodeId = toUrl Back Node (Just nodeId) <> "/category"

putCategories :: Int -> CategoryQuery -> Aff (Array Int)
putCategories nodeId = put $ categoryUrl nodeId
