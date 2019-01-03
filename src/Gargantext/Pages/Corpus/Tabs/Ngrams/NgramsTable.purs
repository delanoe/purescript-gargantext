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
import Gargantext.Config (CTabNgramType(..), End(..), Offset, Limit, Path(..), TabSubType(..), TabType(..), toUrl)
import Gargantext.Config.REST (get)
import Gargantext.Components.Loader as Loader
import Gargantext.Pages.Corpus.Tabs.Types (CorpusInfo)

data Mode = Authors | Sources | Institutes | Terms

derive instance genericMode :: Generic Mode _

instance showMode :: Show Mode where
  show = genericShow

derive instance eqMode :: Eq Mode

type Props = NT.Props (NodePoly CorpusInfo) Mode

type PageParams = NT.PageParams Mode

getTable :: CTabNgramType -> Maybe Int -> Offset -> Limit -> Aff NT.VersionedNgramsTable
getTable tab nodeId offset limit =
  get $ toUrl Back (Ngrams (TabCorpus (TabNgramType tab))
                           offset limit Nothing) nodeId

modeTabType :: Mode -> CTabNgramType
modeTabType Authors = CTabAuthors
modeTabType Sources = CTabSources
modeTabType Institutes = CTabInstitutes
modeTabType Terms = CTabTerms

loadPage :: PageParams -> Aff NT.VersionedNgramsTable
loadPage {nodeId, mode, params: {offset, limit}} =
  getTable (modeTabType mode) (Just nodeId) offset limit
  -- TODO this ignores orderBy

ngramsLoaderClass :: Loader.LoaderClass PageParams NT.VersionedNgramsTable
ngramsLoaderClass = Loader.createLoaderClass "CorpusNgramsLoader" loadPage

ngramsLoader :: Loader.Props' PageParams NT.VersionedNgramsTable -> ReactElement
ngramsLoader props = React.createElement ngramsLoaderClass props []

ngramsTableClass :: Loader.InnerClass PageParams NT.VersionedNgramsTable
ngramsTableClass = createClass "CorpusNgramsTable" NT.ngramsTableSpec NT.initialState

ngramsTableSpec :: Spec {} Props Void
ngramsTableSpec = simpleSpec defaultPerformAction render
  where
    render :: Render {} Props Void
    render _ {path: nodeId, mode} _ _ =
      -- TODO: ignored loaded param
      [ ngramsLoader { path: NT.initialPageParams nodeId mode
                     , component: ngramsTableClass
                     } ]
