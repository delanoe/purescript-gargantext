module Gargantext.Components.Nodes.Corpus.Types where

import Data.Argonaut (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Either (Either(..))
import Data.Maybe (Maybe)

import Gargantext.Prelude

import Gargantext.Components.Node (NodePoly)

type Author = String
type Description = String
type Query = String
type Tag = String
type Title = String
type MarkdownText = String

newtype CorpusHyperdata =
  CorpusHyperdata
  {
    fields :: Array (CorpusField CorpusFieldType)
  }
instance decodeCorpusHyperdata :: DecodeJson CorpusHyperdata where
  decodeJson json = do
    obj <- decodeJson json
    fields <- obj .: "fields"
    pure $ CorpusHyperdata {fields}

newtype CorpusField a = CorpusField {
    name :: String
  , typ  :: a
  }

data CorpusFieldType = JSON {
    authors        :: Author
  , desc           :: Description
  , query          :: Query
  , tag            :: Tag
  , title          :: Title
  }
  | Markdown {
    tag              :: Tag
  , text             :: MarkdownText
  }
instance decodeCorpusField :: DecodeJson (CorpusField CorpusFieldType) where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .: "name"
    type_ <- obj .: "type"
    data_ <- obj .: "data"
    typ <- case type_ of
      "JSON" -> do
        authors <- data_ .: "authors"
        desc <- data_ .: "desc"
        query <- data_ .: "query"
        tag <- data_ .: "tag"
        title <- data_ .: "title"
        pure $ JSON {authors, desc, query, tag, title}
      "Markdown" -> do
        tag <- data_ .: "tag"
        text <- data_ .: "text"
        pure $ Markdown {tag, text}
      _ -> Left $ "Unsupported 'type' " <> type_
    pure $ CorpusField {name, typ}

newtype CorpusInfo =
  CorpusInfo
  { title        :: String
  , desc         :: String
  , query        :: String
  , authors      :: String
  , chart        :: (Maybe (Array Number))
  , totalRecords :: Int }

instance decodeCorpusInfo :: DecodeJson CorpusInfo where
  decodeJson json = do
    obj <- decodeJson json
    title <- obj .: "title"
    desc  <- obj .: "desc"
    query <- obj .: "query"
    authors <- obj .: "authors"
    chart   <- obj .:? "chart"
    let totalRecords = 47361 -- TODO
    pure $ CorpusInfo {title, desc, query, authors, chart, totalRecords}

type CorpusData = { corpusId :: Int
                  , corpusNode :: NodePoly CorpusInfo
                  , defaultListId :: Int}
