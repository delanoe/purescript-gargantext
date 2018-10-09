module Gargantext.Pages.Corpus where

import Data.Maybe (Maybe(..), maybe)
import Prelude hiding (div)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Gargantext.Components.Charts.Options.ECharts (chart)
import Gargantext.Pages.Corpus.Doc.Facets.Dashboard (globalPublis)
import Gargantext.Pages.Corpus.Doc.Facets as Tab
import React.DOM (div, h3, hr, i, p, text)
import React.DOM.Props (className, style)
import Thermite (Render, Spec, defaultPerformAction, simpleSpec)

-------------------------------------------------------------------

type State = { info    :: Maybe CorpusInfo
             }

initialState :: State
initialState = { info : Nothing }

data Action = Load Int

newtype Node a = Node { id       :: Int
                      , typename :: Int
                      , userId   :: Int
                      , parentId  :: Int
                      , name      :: String
                      , date      :: String
                      , hyperdata :: a
                      }

newtype CorpusInfo = CorpusInfo { title   :: String
                  , desc    :: String
                  , query   :: String
                  , date    :: String
                  , authors :: String
                  , chart   :: (Maybe (Array Number))
                  }

corpusInfoDefault :: CorpusInfo
corpusInfoDefault = CorpusInfo
                    { title : "Global Publications"
                    , desc  : " Hal Database"
                    , query : " Query: all publications"
                    , date  : " June. 26 2018, 10:59 am"
                    , authors : " Author(s): first.last name"
                    , chart   : Nothing
                    }

instance decodeCorpusInfo :: DecodeJson CorpusInfo where
  decodeJson json = do
    obj <- decodeJson json
    title <- obj .? "title"
    desc  <- obj .? "desc"
    query <- obj .? "query"
    date  <- obj .? "date"
    authors <- obj .? "authors"
    chart   <- obj .? "chart"
    pure $ CorpusInfo {title, desc, query, date, authors, chart}


instance decodeNode :: (DecodeJson a) => DecodeJson (Node a) where
  decodeJson json = do
    obj <- decodeJson json
    id        <- obj .? "id"
    typename  <- obj .? "typename"
    userId    <- obj .? "userId"
    parentId  <- obj .? "parentId"
    name      <- obj .? "name"
    date      <- obj .? "date"
    
    hyperdata  <- obj .? "hyperdata"
    hyperdata' <- decodeJson hyperdata
    
    pure $ Node  { id : id
                 , typename : typename
                 , userId   : userId
                 , parentId : parentId
                 , name     : name
                 , date     : date
                 , hyperdata: hyperdata'
                 }


layout :: Spec State {} Action
layout = corpusSpec -- <> Tab.pureTab1

corpusSpec :: Spec State {} Action
corpusSpec = simpleSpec defaultPerformAction render
  where
    render :: Render State {} Action
    render dispatch _ state _ =
        [ div [className "row"]
          [ div [className "col-md-3"] [ h3 [] [text corpus.title] ]
          , div [className "col-md-9"] [ hr [style {height : "2px",backgroundColor : "black"}] ]
          ]
        , div [className "row"] [ div [className "jumbotron1", style {padding : "12px 0px 20px 12px"}]
              [ div [ className "col-md-8 content"]
                    [ p [] [ i [className "fa fa-globe"] []
                           , text corpus.desc
                           ]
                    , p [] [ i [className "fab fa-searchengin"] []
                           , text corpus.query
                           ]
                    ]
              , div [ className "col-md-4 content"]
                    [ p [] [ i [className "fa fa-calendar"] []
                           , text corpus.date
                           ]
                    , p [] [ i [className "fa fa-user"] []
                           , text corpus.authors
                           ]
                    ]
              ]
          ]
          -- , chart globalPublis TODO add chart data in state
        ]
          where
            CorpusInfo corpus = maybe corpusInfoDefault identity state.info

