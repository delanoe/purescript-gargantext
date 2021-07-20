module Gargantext.Components.Nodes.Corpus.Types where

import Data.Argonaut     (class DecodeJson, class EncodeJson, decodeJson, (.:), (:=), (~>), jsonEmptyObject)
import Data.Eq.Generic   (genericEq)
import Data.Generic.Rep  (class Generic)
import Data.List   as List
import Data.Maybe        (Maybe(..))
import Data.Newtype      (class Newtype)
import Gargantext.Components.Node (NodePoly)
import Gargantext.Components.Nodes.Types (FTField, Field(..), FieldType(..), FTField, FTFieldList(..), isJSON)
import Gargantext.Prelude
import Reactix     as R
import Record      as Record
import Simple.JSON as JSON
import Toestand    as T

newtype Hyperdata =
  Hyperdata { fields :: FTFieldList }
derive instance Generic Hyperdata _
derive instance Newtype Hyperdata _
instance Eq Hyperdata where eq = genericEq
derive newtype instance JSON.ReadForeign Hyperdata
derive newtype instance JSON.WriteForeign Hyperdata

type NoTotalRecords =
  ( title :: String
  , authors :: String
  , desc :: String
  , query :: String
  )

newtype CorpusInfo =
  CorpusInfo { totalRecords :: Int
             | NoTotalRecords
             }
derive instance Generic CorpusInfo _
derive instance Newtype CorpusInfo _
instance JSON.ReadForeign CorpusInfo where
  readImpl f = do
    inst :: Record NoTotalRecords  <- JSON.readImpl f
    pure $ CorpusInfo $ Record.merge inst { totalRecords: 47361 }  -- TODO
derive newtype instance JSON.WriteForeign CorpusInfo

type CorpusData = { corpusId :: Int
                  , corpusNode :: NodePoly Hyperdata -- CorpusInfo
                  , defaultListId :: Int }

getCorpusInfo :: FTFieldList -> CorpusInfo
getCorpusInfo (FTFieldList as) = case List.head (List.filter isJSON as) of
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
