module Gargantext.Pages.Corpus.Tabs.Ngrams.NgramsTable
  (Mode(..), ngramsTableSpec)
  where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import React (ReactElement)
import React as React
import Thermite (Render, Spec, createClass, defaultPerformAction, simpleSpec)

import Gargantext.Components.Node (NodePoly)
import Gargantext.Components.NgramsTable as NT
import Gargantext.Prelude
import Gargantext.Config (CTabNgramType(..), End(..), Offset, Limit, OrderBy(..), Path(..), TabSubType(..), TabType(..), toUrl)
import Gargantext.Config.REST (get)
import Gargantext.Components.Loader as Loader
import Gargantext.Pages.Corpus.Tabs.Types (CorpusData)

data Mode = Authors | Sources | Institutes | Terms

derive instance genericMode :: Generic Mode _

instance showMode :: Show Mode where
  show = genericShow

derive instance eqMode :: Eq Mode

type Props = NT.Props CorpusData Mode

modeTabType :: Mode -> CTabNgramType
modeTabType Authors = CTabAuthors
modeTabType Sources = CTabSources
modeTabType Institutes = CTabInstitutes
modeTabType Terms = CTabTerms

-- TODO: Move to Components.NgramsTable?
loadPage :: NT.PageParams -> Aff NT.VersionedNgramsTable
loadPage { nodeId, listIds, termListFilter, termTypeFilter
         , searchQuery, tabType, params: {offset, limit, orderBy}} =
  get $ toUrl Back
          (GetNgrams { tabType, offset, limit, listIds
                     , orderBy: convOrderBy <$> orderBy
                     , termListFilter, termTypeFilter
                     , searchQuery
                     })
          (Just nodeId)
  where
    convOrderBy _ = DateAsc -- TODO

-- TODO: Move to Components.NgramsTable?
ngramsLoaderClass :: Loader.LoaderClass NT.PageParams NT.VersionedNgramsTable
ngramsLoaderClass = Loader.createLoaderClass "CorpusNgramsLoader" loadPage

-- TODO: Move to Components.NgramsTable?
ngramsLoader :: Loader.Props' NT.PageParams NT.VersionedNgramsTable -> ReactElement
ngramsLoader props = React.createElement ngramsLoaderClass props []

-- TODO: Move to Components.NgramsTable?
ngramsTableClass :: Loader.InnerClass NT.PageParams NT.VersionedNgramsTable
ngramsTableClass = createClass "CorpusNgramsTable" NT.ngramsTableSpec NT.initialState

ngramsTableSpec :: Spec {} Props Void
ngramsTableSpec = simpleSpec defaultPerformAction render
  where
    render :: Render {} Props Void
    render _ {path: nodeId, loaded: {defaultListId}, mode} _ _ =
      [ ngramsLoader { path: NT.initialPageParams nodeId [defaultListId] tabType
                     , component: ngramsTableClass
                     } ]
      where
        tabType = TabCorpus $ TabNgramType $ modeTabType mode
