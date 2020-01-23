module Gargantext.Components.Nodes.Corpus.Types where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, (.:), (.:?), (:=), (~>), jsonEmptyObject)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Maybe (Maybe)

import Gargantext.Prelude

import Gargantext.Components.Node (NodePoly)

type Author = String
type Description = String
type Query = String
type Tag = String
type Title = String
type MarkdownText = String

newtype Hyperdata =
  Hyperdata
  {
    fields :: Array FTField
  }
instance decodeHyperdata :: DecodeJson Hyperdata where
  decodeJson json = do
    obj <- decodeJson json
    fields <- obj .: "fields"
    pure $ Hyperdata {fields}
instance encodeHyperdata :: EncodeJson Hyperdata where
  encodeJson (Hyperdata {fields}) = do
       "fields"  := fields
    ~> jsonEmptyObject

newtype Field a = Field {
    name :: String
  , typ  :: a
  }
type FTField = Field FieldType
derive instance genericFTField :: Generic (Field FieldType) _
instance eqFTField :: Eq (Field FieldType) where
  eq = genericEq

data FieldType = JSON {
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
derive instance genericFieldType :: Generic FieldType _
instance eqFieldType :: Eq FieldType where
  eq = genericEq
instance decodeFTField :: DecodeJson (Field FieldType) where
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
    pure $ Field {name, typ}
instance encodeFTField :: EncodeJson (Field FieldType) where
  encodeJson (Field {name, typ}) =
        "data"  := typ
    ~>  "name"  := name
    ~> "type"   := typ' typ
    ~> jsonEmptyObject
    where
      typ' (JSON _) = "JSON"
      typ' (Markdown _) = "Markdown"
instance encodeFieldType :: EncodeJson FieldType where
  encodeJson (JSON {authors, desc, query, tag, title}) =
       "authors" := authors
    ~> "desc"    := desc
    ~> "query"   := query
    ~> "tag"     := "JsonField"
    ~> "title"   := title
    ~> jsonEmptyObject
  encodeJson (Markdown {text}) =
       "tag"  := "MarkdownField"
    ~> "text" := text
    ~> jsonEmptyObject

defaultField :: FTField
defaultField = Field {
  name: "New file"
  , typ: Markdown {
    tag: "MarkdownField"
    , text: "# New file"
    }
  }

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
