module Gargantext.Pages.Texts.Tabs where

import Data.Argonaut (class DecodeJson, decodeJson, (.?), (.??))
import Data.Maybe (Maybe(..))
--------------------------------------------------------
import Gargantext.Prelude
import Prelude hiding (div)

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.Charts.Options.ECharts (chart) as ECharts
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.Loader as Loader
import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Tab as Tab
import Gargantext.Config (CTabNgramType(..), TabSubType(..), TabType(..), Ends)
import Gargantext.Pages.Corpus.Chart.Histo (histo)
import Gargantext.Utils.Reactix as R2

data Mode = MoreLikeFav | MoreLikeTrash

derive instance genericMode :: Generic Mode _

instance showMode :: Show Mode where
  show = genericShow

derive instance eqMode :: Eq Mode

modeTabType :: Mode -> CTabNgramType
modeTabType MoreLikeFav    = CTabAuthors  -- TODO
modeTabType MoreLikeTrash  = CTabSources  -- TODO

type Props = ( ends :: Ends, corpusId :: Int, corpusData :: CorpusData )

tabs :: Record Props -> R.Element
tabs props = R.createElement tabsCpt props []

tabsCpt :: R.Component Props
tabsCpt = R.hooksComponent "CorpusTabs" cpt
  where
    cpt {ends, corpusId, corpusData} _ = do
      (selected /\ setSelected) <- R.useState' 0
      pure $ Tab.tabs { tabs, selected }
      where
        tabs = [ "Documents"     /\ docs,        "Trash"           /\ trash
               , "More like fav" /\ moreLikeFav, "More like trash" /\ moreLikeTrash ]
        docView' tabType = docView { ends, corpusId, corpusData, tabType }
        docs = R.fragment [ docsHisto, docView' TabDocs ]
        docsHisto = histo { path, ends }
          where path = { corpusId, tabType: TabCorpus TabDocs }
        moreLikeFav = docView' TabMoreLikeFav
        moreLikeTrash = docView' TabMoreLikeTrash
        trash = docView' TabTrash

type DocViewProps a = ( ends :: Ends, corpusId :: Int, corpusData :: CorpusData, tabType :: TabSubType a )

docView :: forall a. Record (DocViewProps a) -> R.Element
docView props = R.createElement docViewCpt props []

--docViewSpec :: forall a. TabSubType a -> Props -> R.Element
docViewCpt :: forall a. R.Component (DocViewProps a)
docViewCpt = R.hooksComponent "DocViewWithCorpus" cpt
  where
    cpt {ends, corpusId, corpusData: {defaultListId}, tabType} _children = do
      pure $ DT.docView $ params tabType
      where
        params :: forall a. TabSubType a -> Record DT.Props
        params TabDocs =
          { nodeId: corpusId
            -- ^ TODO merge nodeId and corpusId in DT
          , chart  : H.div {} []
          , tabType: TabCorpus TabDocs
          , totalRecords: 4737
          , listId: defaultListId
          , corpusId: Just corpusId
          , showSearch: true
          , ends }
        params TabMoreLikeFav =
          { nodeId: corpusId
            -- ^ TODO merge nodeId and corpusId in DT
          , chart  : H.div {} []
          , tabType: TabCorpus TabMoreLikeFav
          , totalRecords: 4737
          , listId: defaultListId
          , corpusId: Just corpusId
          , showSearch: false
          , ends }
        params TabMoreLikeTrash =
          { nodeId: corpusId
            -- ^ TODO merge nodeId and corpusId in DT
          , chart  : H.div {} []
          , tabType: TabCorpus TabMoreLikeTrash
          , totalRecords: 4737
          , listId: defaultListId
          , corpusId: Just corpusId
          , showSearch: false
          , ends }
        params TabTrash =
          { nodeId: corpusId
            -- ^ TODO merge nodeId and corpusId in DT
          , chart  : H.div {} []
          , tabType: TabCorpus TabTrash
          , totalRecords: 4737
          , listId: defaultListId
          , corpusId: Nothing
          , showSearch: true
          , ends }
        -- DUMMY
        params _ =
          { nodeId: corpusId
            -- ^ TODO merge nodeId and corpusId in DT
          , chart  : H.div {} []
          , tabType: TabCorpus TabTrash
          , totalRecords: 4737
          , listId: defaultListId
          , corpusId: Nothing
          , showSearch: true
          , ends }

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



