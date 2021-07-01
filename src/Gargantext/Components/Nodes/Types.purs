module Gargantext.Components.Nodes.Types where

import Gargantext.Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Foreign as F
import Simple.JSON as JSON

import Gargantext.Utils.JSON as GUJ

type Author = String
type Description = String
type HaskellCode = String
type Hash = String
type MarkdownText = String
type PythonCode = String
type Query = String
type Tag = String
type Title = String


-- We need FTFields with indices because it's the only way to identify the
-- FTField element inside a component (there are no UUIDs and such)
type Index = Int
type FTFieldWithIndex = { idx :: Index, ftField :: FTField }

newtype FTFieldsWithIndex = FTFieldsWithIndex (List.List FTFieldWithIndex)
derive instance Generic FTFieldsWithIndex _
derive instance Newtype FTFieldsWithIndex _
instance Eq FTFieldsWithIndex where eq = genericEq
instance JSON.ReadForeign FTFieldsWithIndex where readImpl f = FTFieldsWithIndex <$> GUJ.readList f
instance JSON.WriteForeign FTFieldsWithIndex where writeImpl (FTFieldsWithIndex lst) = GUJ.writeList lst

newtype Field a =
  Field { name :: String
        , typ  :: a
        }

type FTField = Field FieldType

type HaskellFT =
  ( haskell :: HaskellCode )
type JSONFT =
  ( authors :: Author
  , desc :: Description
  , query :: Query
  , title :: Title )
type MarkdownFT =
  ( text :: MarkdownText )
type PythonFT =
  ( python :: PythonCode )

type FieldFieldTypeJSONRead =
  { name :: String
  , type :: String
  , data :: { tag :: Tag
            -- HaskellFT
            , haskell :: Maybe HaskellCode
             -- JSONFT
            , authors :: Maybe Author
            , desc :: Maybe Description
            , query :: Maybe Query
            , title :: Maybe Title
            -- MarkdownFT
            , text :: Maybe MarkdownText
            -- PythonFT
            , python :: Maybe PythonCode
            }
  }

derive instance Generic (Field FieldType) _
derive instance Newtype (Field FieldType) _
instance JSON.ReadForeign (Field FieldType) where
  readImpl f = do
    r :: FieldFieldTypeJSONRead <- JSON.readImpl f
    typ <- case r.type of
                "Haskell" -> pure $ Haskell { haskell: fromMaybe "" r.data.haskell, tag: r.data.tag }
                "JSON" -> pure $ JSON { authors: fromMaybe "" r.data.authors
                                      , desc: fromMaybe "" r.data.desc
                                      , query: fromMaybe "" r.data.query
                                      , tag: r.data.tag
                                      , title: fromMaybe "" r.data.title }
                "Markdown" -> pure $ Markdown { tag: r.data.tag, text: fromMaybe "" r.data.text }
                "Python" -> pure $ Python { python: fromMaybe "" r.data.python, tag: r.data.tag }
                _ -> F.fail $ F.ErrorAtProperty "type" $ F.ForeignError "Unknown type"
    pure $ Field { name: r.name, typ }
instance JSON.WriteForeign (Field FieldType) where
  writeImpl (Field { name, typ }) = JSON.writeImpl $ { data: typ
                                                     , name
                                                     , type: typ' typ }
    where
      typ' (Haskell _)  = "Haskell"
      typ' (JSON _)     = "JSON"
      typ' (Markdown _) = "Markdown"
      typ' (Python _)   = "Python"
instance Eq (Field FieldType) where eq = genericEq
instance Show (Field FieldType) where show = genericShow

data FieldType =
    Haskell { tag :: Tag | HaskellFT }
  | JSON { tag :: Tag | JSONFT }
  | Markdown { tag :: Tag | MarkdownFT }
  | Python { tag :: Tag | PythonFT }
derive instance Generic FieldType _
instance JSON.WriteForeign FieldType where
  writeImpl (Haskell { haskell }) = JSON.writeImpl { haskell, tag: "HaskellField" }
  writeImpl (JSON { authors, desc, query, tag, title }) = JSON.writeImpl { authors, desc, query, tag: "JsonField", title }
  writeImpl (Markdown { text }) = JSON.writeImpl { tag: "MarkdownField", text }
  writeImpl (Python { python }) = JSON.writeImpl { python, tag: "PythonField" }
instance Eq FieldType where eq = genericEq
instance Show FieldType where show = genericShow

newtype FTFieldList = FTFieldList (List.List FTField)
derive instance Generic FTFieldList _
derive instance Newtype FTFieldList _
instance Eq FTFieldList where eq = genericEq
instance JSON.ReadForeign FTFieldList where readImpl f = FTFieldList <$> GUJ.readList f
instance JSON.WriteForeign FTFieldList where writeImpl (FTFieldList lst) = GUJ.writeList lst


isJSON :: FTField -> Boolean
isJSON (Field {typ}) = isJSON' typ
  where
    isJSON' (JSON _) = true
    isJSON' _        = false
-- instance DecodeJson (Field FieldType) where
--   decodeJson json = do
--     obj <- decodeJson json
--     name <- obj .: "name"
--     type_ <- obj .: "type"
--     data_ <- obj .: "data"
--     typ <- case type_ of
--       "Haskell" -> do
--         haskell <- data_ .: "haskell"
--         tag <- data_ .: "tag"
--         pure $ Haskell {haskell, tag}

--       "Python" -> do
--         python <- data_ .: "python"
--         tag    <- data_ .: "tag"
--         pure $ Python {python, tag}

--       "JSON" -> do
--         authors <- data_ .: "authors"
--         desc <- data_ .: "desc"
--         query <- data_ .: "query"
--         tag <- data_ .: "tag"
--         title <- data_ .: "title"
--         pure $ JSON {authors, desc, query, tag, title}
--       "Markdown" -> do
--         tag <- data_ .: "tag"
--         text <- data_ .: "text"
--         pure $ Markdown {tag, text}
--       _ -> Left $ TypeMismatch $ "Unsupported 'type' " <> type_
--     pure $ Field {name, typ}
-- instance EncodeJson (Field FieldType) where
--   encodeJson (Field {name, typ}) =
--        "data"  := typ
--     ~> "name"  := name
--     ~> "type"   := typ' typ
--     ~> jsonEmptyObject
--     where
--       typ' (Haskell  _) = "Haskell"
--       typ' (Python   _) = "Python"
--       typ' (JSON     _) = "JSON"
--       typ' (Markdown _) = "Markdown"

-- instance EncodeJson FieldType where
--   encodeJson (Haskell {haskell}) =
--        "haskell" := haskell
--     ~> "tag"  := "HaskellField"
--     ~> jsonEmptyObject

--   encodeJson (Python {python}) =
--        "python" := python
--     ~> "tag"  := "PythonField"
--     ~> jsonEmptyObject

--   encodeJson (JSON {authors, desc, query, tag, title}) =
--        "authors" := authors
--     ~> "desc"    := desc
--     ~> "query"   := query
--     ~> "tag"     := "JsonField"
--     ~> "title"   := title
--     ~> jsonEmptyObject
--   encodeJson (Markdown {text}) =
--        "tag"  := "MarkdownField"
--     ~> "text" := text
--     ~> jsonEmptyObject

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
