module Gargantext.Pages.Texts.Tabs.Types where

import Data.Argonaut (class DecodeJson, decodeJson, (.?), (.??))
import Data.Maybe (Maybe(..))
--------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Loader as Loader

newtype CorpusInfo = CorpusInfo { title   :: String
                                , desc    :: String
                                , query   :: String
                                , authors :: String
                                , chart   :: (Maybe (Array Number))
                                , totalRecords :: Int
                                }

corpusInfoDefault :: NodePoly CorpusInfo
corpusInfoDefault = NodePoly { id : 0
                             , typename : 0
                             , userId : 0
                             , parentId : 0
                             , name : "Default name"
                             , date  : " Default date"
                             , hyperdata : CorpusInfo
                                { title : "Default title"
                                , desc  : " Default desc"
                                , query : " Default Query"
                                , authors : " Author(s): default"
                                , chart   : Nothing
                                , totalRecords : 0
                                }
                             }

instance decodeCorpusInfo :: DecodeJson CorpusInfo where
  decodeJson json = do
    obj <- decodeJson json
    title <- obj .? "title"
    desc  <- obj .? "desc"
    query <- obj .? "query"
    authors <- obj .? "authors"
    chart   <- obj .?? "chart"
    let totalRecords = 47361 -- TODO
    pure $ CorpusInfo {title, desc, query, authors, chart, totalRecords}

type CorpusData = { corpusId :: Int
                  , corpusNode :: NodePoly CorpusInfo
                  , defaultListId :: Int}

type PropsRow = ( corpusId :: Int, corpusData :: CorpusData )
type Props = Record PropsRow

-- TODO include Gargantext.Pages.Corpus.Tabs.States
-- TODO include Gargantext.Pages.Corpus.Tabs.Actions