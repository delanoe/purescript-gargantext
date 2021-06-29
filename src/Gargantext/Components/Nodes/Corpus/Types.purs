module Gargantext.Components.Nodes.Corpus.Types where

import Gargantext.Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, (.:), (:=), (~>), jsonEmptyObject)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.List as List
import Data.Maybe (Maybe(..))
import Gargantext.Components.Node (NodePoly)
import Gargantext.Components.Nodes.Types (FTField, Field(..), FieldType(..), isJSON)
import Reactix as R
import Toestand as T

newtype Hyperdata =
  Hyperdata { fields :: List.List FTField }
derive instance genericHyperdata :: Generic Hyperdata _
instance eqHyperdata :: Eq Hyperdata where
  eq = genericEq
instance decodeHyperdata :: DecodeJson Hyperdata where
  decodeJson json = do
    obj <- decodeJson json
    fields <- obj .: "fields"
    pure $ Hyperdata {fields}
instance encodeHyperdata :: EncodeJson Hyperdata where
  encodeJson (Hyperdata {fields}) = do
       "fields"  := fields
    ~> jsonEmptyObject

newtype CorpusInfo =
  CorpusInfo { title        :: String
             , authors      :: String
             , desc         :: String
             , query        :: String
             , totalRecords :: Int
             }

instance decodeCorpusInfo :: DecodeJson CorpusInfo where
  decodeJson json = do
    obj <- decodeJson json
    title <- obj .: "title"
    desc  <- obj .: "desc"
    query <- obj .: "query"
    authors <- obj .: "authors"
    let totalRecords = 47361 -- TODO
    pure $ CorpusInfo {title, authors, desc, query, totalRecords}

type CorpusData = { corpusId :: Int
                  , corpusNode :: NodePoly Hyperdata -- CorpusInfo
                  , defaultListId :: Int }

getCorpusInfo :: List.List FTField -> CorpusInfo
getCorpusInfo as = case List.head (List.filter isJSON as) of
  Just (Field {typ: JSON {authors, desc, query, title}}) -> CorpusInfo { title
                                                                       , desc
                                                                       , query
                                                                       , authors
                                                                       , totalRecords: 0
                                                                       }
  _                                -> CorpusInfo { title:"Empty"
                                                 , desc:""
                                                 , query:""
                                                 , authors:""
                                                 , totalRecords: 0
                                                 }
