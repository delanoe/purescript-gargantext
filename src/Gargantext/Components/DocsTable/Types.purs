module Gargantext.Components.DocsTable.Types where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Map (Map)
import Data.Map as Map

import Gargantext.Prelude

import Gargantext.Components.Category.Types (Category(..), decodeCategory)

data Action
  = MarkCategory Int Category

newtype DocumentsView
  = DocumentsView
    { _id    :: Int
    , category :: Category
    , date   :: Int
    , ngramCount :: Int
    , source :: String
    , title  :: String
    , url    :: String
    }

{-
derive instance genericDocumentsView :: Generic DocumentsView _
instance showDocumentsView :: Show DocumentsView where
  show = genericShow
instance decodeJsonSearchType :: Argonaut.DecodeJson SearchType where
  decodeJson = genericSumDecodeJson
instance encodeJsonSearchType :: Argonaut.EncodeJson SearchType where
  encodeJson = genericSumEncodeJson
  -}

instance decodeDocumentsView :: DecodeJson DocumentsView where
  decodeJson json = do
    obj <- decodeJson json
    _id <- obj .: "id"
    category <- obj .: "category"
    date <- obj .: "date"
    ngramCount <- obj .: "ngramCount"
    source <- obj .: "source"
    title <- obj .: "title"
    url <- obj .: "url"
    pure $ DocumentsView { _id, category, date, ngramCount, source, title, url }
instance encodeDocumentsView :: EncodeJson DocumentsView where
  encodeJson (DocumentsView dv) =
       "id" := dv._id
    ~> "category" := dv.category
    ~> "date" := dv.date
    ~> "ngramCount" := dv.ngramCount
    ~> "source" := dv.source
    ~> "title" := dv.title
    ~> "url" := dv.url
    ~> jsonEmptyObject


newtype Response = Response
  { cid        :: Int
  , hyperdata  :: Hyperdata
  , category   :: Category
  , ngramCount :: Int
  , title      :: String
  }


newtype Hyperdata = Hyperdata
  { title  :: String
  , source :: String
  , pub_year   :: Int
  }


instance decodeHyperdata :: DecodeJson Hyperdata where
  decodeJson json = do
    obj    <- decodeJson json
    title  <- obj .: "title"
    source <- obj .: "source"
    pub_year <- obj .: "publication_year"
    pure $ Hyperdata { title,source, pub_year}

instance decodeResponse :: DecodeJson Response where
  decodeJson json = do
    obj        <- decodeJson json
    category   <- obj .: "category"
    cid        <- obj .: "id"
    hyperdata  <- obj .: "hyperdata"
    ngramCount <- obj .: "id"
    title      <- obj .: "title"
    pure $ Response { cid, title, category: decodeCategory category, ngramCount, hyperdata }


type LocalCategories = Map Int Category
type Query = String
