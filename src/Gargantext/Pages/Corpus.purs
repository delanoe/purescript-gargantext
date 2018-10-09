module Gargantext.Pages.Corpus where

import Prelude hiding (div)

import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..), maybe)
import Data.Lens (Lens', Prism', lens, prism, (?~))
import Data.Either (Either(..))
import Data.Argonaut (class DecodeJson, decodeJson, (.?))

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)

import React.DOM (div, h3, hr, i, p, text)
import React.DOM.Props (className, style)
import Thermite ( Render, Spec, PerformAction
                , defaultPerformAction, simpleSpec, modifyState)
--------------------------------------------------------
import Gargantext.Config      (toUrl, NodeType(..), End(..))
import Gargantext.Config.REST (get)
import Gargantext.Components.Charts.Options.ECharts (chart)
import Gargantext.Pages.Corpus.Doc.Facets.Dashboard (globalPublis)
import Gargantext.Pages.Corpus.Doc.Facets (pureTab1)
---------------------------------------------------------
-- Facets
import Gargantext.Pages.Corpus.Doc.Facets.Documents as D
import Gargantext.Pages.Corpus.Doc.Facets.Sources   as S
import Gargantext.Pages.Corpus.Doc.Facets.Authors   as A
import Gargantext.Pages.Corpus.Doc.Facets.Terms     as T
import Gargantext.Components.Tab as Tab
-------------------------------------------------------------------
type State = { info       :: Maybe (NodePoly CorpusInfo)
--             , docview    :: D.state
--             , authorview :: A.State
--             , sourceview :: S.State
--             , termsview  :: T.State
--             , activeTab  :: Int
             }

initialState :: State
initialState = { info : Nothing }

data Action = Load Int

newtype NodePoly a = NodePoly { id :: Int
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
                                , authors :: String
                                , chart   :: (Maybe (Array Number))
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
                                }
                             }

instance decodeCorpusInfo :: DecodeJson CorpusInfo where
  decodeJson json = do
    obj <- decodeJson json
    title <- obj .? "title"
    desc  <- obj .? "desc"
    query <- obj .? "query"
    authors <- obj .? "authors"
    chart   <- obj .? "chart"
    pure $ CorpusInfo {title, desc, query, authors, chart}


instance decodeNode :: (DecodeJson a) => DecodeJson (NodePoly a) where
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
    
    pure $ NodePoly  { id : id
                 , typename : typename
                 , userId   : userId
                 , parentId : parentId
                 , name     : name
                 , date     : date
                 , hyperdata: hyperdata'
                 }


------------------------------------------------------------------------
layout :: Spec State {} Action
layout = corpusSpec -- <> pureTab1

corpusSpec :: Spec State {} Action
corpusSpec = simpleSpec performAction render
  where
    render :: Render State {} Action
    render dispatch _ state _ =
        [ div [className "row"]
          [ div [className "col-md-3"] [ h3 [] [text "Corpus " <> text title] ]
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
                           , text date'
                           ]
                    , p [] [ i [className "fa fa-user"] []
                           , text corpus.authors
                           ]
                    ]
              ]
          ]
          --, chart globalPublis -- TODO add chart data in state
        ]
          where
            NodePoly { name: title
                     , date: date'
                     , hyperdata : CorpusInfo corpus
                   }
              = maybe corpusInfoDefault identity state.info


------------------------------------------------------------------------

performAction :: PerformAction State {} Action
performAction (Load nId) _ _ = do
  eitherInfo <- lift $ getNode nId
  _ <- case eitherInfo of
            (Right node') -> void $ modifyState $ _info ?~ node'
            (Left       err)  -> do
              liftEffect $ log err
  liftEffect <<< log $ "Node Corpus fetched."
performAction _ _ _ = pure unit

getNode :: Int -> Aff (Either String (NodePoly CorpusInfo))
getNode id = get $ toUrl Back Node id

_info :: Lens' State (Maybe (NodePoly CorpusInfo))
_info = lens (\s -> s.info) (\s ss -> s{info = ss})

------------------------------------------------------------------------
-- Tabs
------------------------------------------------------------------------





