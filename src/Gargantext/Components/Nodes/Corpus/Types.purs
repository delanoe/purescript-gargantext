module Gargantext.Components.Nodes.Corpus.Types where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, (.:), (:=), (~>), jsonEmptyObject)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.List as List
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Gargantext.Components.Node (NodePoly)
import Gargantext.Prelude

type Author = String
type Description = String
type Query = String
type Tag = String
type Title = String
type HaskellCode = String
type MarkdownText = String
type Hash = String

newtype Hyperdata =
  Hyperdata { fields :: List.List FTField }

instance decodeHyperdata :: DecodeJson Hyperdata where
  decodeJson json = do
    obj <- decodeJson json
    fields <- obj .: "fields"
    pure $ Hyperdata {fields}

instance encodeHyperdata :: EncodeJson Hyperdata where
  encodeJson (Hyperdata {fields}) = do
       "fields"  := fields
    ~> jsonEmptyObject

newtype Field a =
  Field { name :: String
        , typ  :: a
        }

type FTField = Field FieldType

derive instance genericFTField :: Generic (Field FieldType) _

instance eqFTField :: Eq (Field FieldType) where
  eq = genericEq

instance showFTField :: Show (Field FieldType) where
  show = genericShow

data FieldType =
    Haskell { haskell :: HaskellCode
            , tag     :: Tag
            }
  | Python { python :: HaskellCode
           , tag     :: Tag
           }

  | JSON { authors :: Author
         , desc    :: Description
         , query   :: Query
         , tag     :: Tag
         , title   :: Title
         }
  | Markdown { tag  :: Tag
             , text :: MarkdownText
             }


isJSON :: FTField -> Boolean
isJSON (Field {typ}) = isJSON' typ
  where
    isJSON' (JSON _) = true
    isJSON' _        = false

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

      "Python" -> do
        python <- data_ .: "python"
        tag    <- data_ .: "tag"
        pure $ Python {python, tag}

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
      _ -> Left $ TypeMismatch $ "Unsupported 'type' " <> type_
    pure $ Field {name, typ}

instance encodeFTField :: EncodeJson (Field FieldType) where
  encodeJson (Field {name, typ}) =
       "data"  := typ
    ~> "name"  := name
    ~> "type"   := typ' typ
    ~> jsonEmptyObject
    where
      typ' (Haskell  _) = "Haskell"
      typ' (Python   _) = "Python"
      typ' (JSON     _) = "JSON"
      typ' (Markdown _) = "Markdown"

instance encodeFieldType :: EncodeJson FieldType where
  encodeJson (Haskell {haskell}) =
       "haskell" := haskell
    ~> "tag"  := "HaskellField"
    ~> jsonEmptyObject

  encodeJson (Python {python}) =
       "python" := python
    ~> "tag"  := "PythonField"
    ~> jsonEmptyObject

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

defaultPython :: FieldType
defaultPython  = Python defaultPython'

defaultPython' :: { python :: String, tag :: String }
defaultPython' = { python: "import Foo"
                  , tag    : "PythonField"
                  }

defaultHaskell :: FieldType
defaultHaskell  = Haskell defaultHaskell'

defaultHaskell' :: { haskell :: String, tag :: String }
defaultHaskell' = { haskell: ""
                  , tag    : "HaskellField"
                  }

defaultJSON :: FieldType
defaultJSON = JSON defaultJSON'


defaultJSON' :: { authors :: String
                , desc :: String
                , query :: String
                , tag :: String
                , title :: String
                }
defaultJSON' = { authors: ""
               , desc: ""
               , query: ""
               , tag: "JSONField"
               , title: ""
               }

defaultMarkdown :: FieldType
defaultMarkdown = Markdown defaultMarkdown'
defaultMarkdown' :: { tag  :: String
                    , text :: String
                    }
defaultMarkdown' = { tag: "MarkdownField"
                   , text: "# New file"
                   }

defaultField :: FTField
defaultField = Field { name: "New file"
                    , typ: defaultMarkdown
                    }

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
