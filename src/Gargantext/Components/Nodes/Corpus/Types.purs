module Gargantext.Components.Nodes.Corpus.Types where

import Data.Maybe (Maybe(..))
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, (.:), (.:?), (:=), (~>), jsonEmptyObject)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.List ((:))
import Data.List as List
import Data.Maybe (Maybe)

import Gargantext.Prelude

import Gargantext.Components.Node (NodePoly)
import Gargantext.Components.Nodes.Corpus.Chart.Predefined as P

type Author = String
type Description = String
type Query = String
type Tag = String
type Title = String
type HaskellCode = String
type MarkdownText = String
type Hash = String

newtype Hyperdata =
  Hyperdata
  {
    fields :: List.List FTField
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
instance showFTField :: Show (Field FieldType) where
  show = genericShow

data FieldType =
    Haskell {
    haskell          :: HaskellCode
  , tag              :: Tag
  }
  | JSON {
    authors        :: Author
  , charts         :: Array P.PredefinedChart
  , desc           :: Description
  , query          :: Query
  , tag            :: Tag
  , title          :: Title
  }
  | Markdown {
    tag              :: Tag
  , text             :: MarkdownText
  }


isJSON :: FTField -> Boolean
isJSON (Field {typ}) = isJSON' typ
  where
    isJSON' (JSON _) = true
    isJSON' _        = false

getCorpusInfo :: List.List FTField -> CorpusInfo
getCorpusInfo as = case List.head (List.filter isJSON as) of
  Just (Field {typ: JSON {authors, charts, desc, query, title}}) -> CorpusInfo { title
                                                                     , desc
                                                                     , query
                                                                     , authors
                                                                     , charts
                                                                     , totalRecords: 0
                                                                     }
  _                                -> CorpusInfo { title:"Empty"
                                                 , desc:""
                                                 , query:""
                                                 , authors:""
                                                 , charts: []
                                                 , totalRecords: 0
                                                 }


updateHyperdataCharts :: Hyperdata -> Array P.PredefinedChart -> Hyperdata
updateHyperdataCharts (Hyperdata h@{fields}) charts = Hyperdata $ h { fields = updateFieldsCharts fields charts }

updateFieldsCharts :: List.List FTField -> Array P.PredefinedChart -> List.List FTField
updateFieldsCharts List.Nil _ = List.Nil
updateFieldsCharts fs [] = fs
updateFieldsCharts ((Field f@{typ: JSON j@{charts}}):as) pcharts = (Field $ f { typ = JSON $ j { charts = pcharts } }):as
updateFieldsCharts (a@(Field {typ: _}):as) pcharts = a:(updateFieldsCharts as pcharts)

derive instance genericFieldType :: Generic FieldType _
instance eqFieldType :: Eq FieldType where
  eq = genericEq
instance showFieldType :: Show FieldType where
  show = genericShow
instance decodeFTField :: DecodeJson (Field FieldType) where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .: "name"
    type_ <- obj .: "type"
    data_ <- obj .: "data"
    typ <- case type_ of
      "Haskell" -> do
        haskell <- data_ .: "haskell"
        tag <- data_ .: "tag"
        pure $ Haskell {haskell, tag}
      "JSON" -> do
        authors <- data_ .: "authors"
        charts <- data_ .: "charts"
        desc <- data_ .: "desc"
        query <- data_ .: "query"
        tag <- data_ .: "tag"
        title <- data_ .: "title"
        pure $ JSON {authors, charts, desc, query, tag, title}
      "Markdown" -> do
        tag <- data_ .: "tag"
        text <- data_ .: "text"
        pure $ Markdown {tag, text}
      _ -> Left $ "Unsupported 'type' " <> type_
    pure $ Field {name, typ}
instance encodeFTField :: EncodeJson (Field FieldType) where
  encodeJson (Field {name, typ}) =
       "data"  := typ
    ~> "name"  := name
    ~> "type"   := typ' typ
    ~> jsonEmptyObject
    where
      typ' (Haskell _)  = "Haskell"
      typ' (JSON _)     = "JSON"
      typ' (Markdown _) = "Markdown"
instance encodeFieldType :: EncodeJson FieldType where
  encodeJson (Haskell {haskell}) =
       "haskell" := haskell
    ~> "tag"  := "HaskellField"
    ~> jsonEmptyObject
  encodeJson (JSON {authors, charts, desc, query, tag, title}) =
       "authors" := authors
    ~> "charts"  := charts
    ~> "desc"    := desc
    ~> "query"   := query
    ~> "tag"     := "JsonField"
    ~> "title"   := title
    ~> jsonEmptyObject
  encodeJson (Markdown {text}) =
       "tag"  := "MarkdownField"
    ~> "text" := text
    ~> jsonEmptyObject

defaultHaskell :: FieldType
defaultHaskell = Haskell defaultHaskell'
defaultHaskell' = {
    haskell: ""
  , tag: "HaskellField"
  }

defaultJSON :: FieldType
defaultJSON = JSON defaultJSON'
defaultJSON' = {
    authors: ""
  , charts: []
  , desc: ""
  , query: ""
  , tag: "JSONField"
  , title: ""
}

defaultMarkdown :: FieldType
defaultMarkdown = Markdown defaultMarkdown'
defaultMarkdown' = {
    tag: "MarkdownField"
  , text: "# New file"
  }

defaultField :: FTField
defaultField = Field {
    name: "New file"
  , typ: defaultMarkdown
  }

newtype CorpusInfo =
  CorpusInfo
  { title        :: String
  , authors      :: String
  , charts       :: Array P.PredefinedChart
  , desc         :: String
  , query        :: String
  , totalRecords :: Int }

instance decodeCorpusInfo :: DecodeJson CorpusInfo where
  decodeJson json = do
    obj <- decodeJson json
    title <- obj .: "title"
    desc  <- obj .: "desc"
    query <- obj .: "query"
    authors <- obj .: "authors"
    charts  <- obj .: "charts"
    let totalRecords = 47361 -- TODO
    pure $ CorpusInfo {title, authors, charts, desc, query, totalRecords}

type CorpusData = { corpusId :: Int
                  , corpusNode :: NodePoly Hyperdata -- CorpusInfo
                  , defaultListId :: Int }
