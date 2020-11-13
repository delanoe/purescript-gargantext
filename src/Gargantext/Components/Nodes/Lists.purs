module Gargantext.Components.Nodes.Lists where

import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Reactix as R
import Record as Record
------------------------------------------------------------------------
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.NgramsTable.Loader (clearCache)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
import Gargantext.Components.Nodes.Corpus.Types (getCorpusInfo, CorpusInfo(..), Hyperdata(..))
import Gargantext.Components.Nodes.Lists.Tabs as Tabs
import Gargantext.Components.Nodes.Lists.Types as NT
import Gargantext.Components.Table as Table
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude
import Gargantext.Sessions (Session, sessionId, getCacheState, setCacheState)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2

thisModule :: String
thisModule = "Gargantext.Components.Nodes.Lists"
------------------------------------------------------------------------

type Props = (
    appReload     :: GT.ReloadS
  , asyncTasksRef :: R.Ref (Maybe GAT.Reductor)
  , nodeId        :: Int
  , session       :: Session
  , sessionUpdate :: Session -> Effect Unit
  , treeReloadRef :: R.Ref (Maybe GT.ReloadS)
  )

type WithTreeProps = (
    handed :: GT.Handed
  | Props
  )

listsLayoutWithTree :: R2.Component WithTreeProps
listsLayoutWithTree props = R.createElement listsLayoutWithTreeCpt props

listsLayoutWithTreeCpt :: R.Component WithTreeProps
listsLayoutWithTreeCpt = R.hooksComponentWithModule thisModule "listsLayoutWithTree" cpt
  where
    cpt { appReload, asyncTasksRef, handed, nodeId, session, sessionUpdate, treeReloadRef } _ = do
      pure $ listsLayout { appReload, asyncTasksRef, nodeId, session, sessionUpdate, treeReloadRef }

listsLayout :: Record Props -> R.Element
listsLayout props = R.createElement listsLayoutCpt props []

listsLayoutCpt :: R.Component Props
listsLayoutCpt = R.hooksComponentWithModule thisModule "listsLayout" cpt
  where
    cpt path@{ nodeId, session } _ = do
      let sid = sessionId session

      pure $ listsLayoutWithKey $ Record.merge path { key: show sid <> "-" <> show nodeId }

type KeyProps = (
  key :: String
  | Props
  )

listsLayoutWithKey :: Record KeyProps -> R.Element
listsLayoutWithKey props = R.createElement listsLayoutWithKeyCpt props []

listsLayoutWithKeyCpt :: R.Component KeyProps
listsLayoutWithKeyCpt = R.hooksComponentWithModule thisModule "listsLayoutWithKey" cpt
  where
    cpt { appReload, asyncTasksRef, nodeId, session, sessionUpdate, treeReloadRef } _ = do
      let path = { nodeId, session }

      cacheState <- R.useState' $ getCacheState NT.CacheOn session nodeId

      useLoader path loadCorpusWithChild $
        \corpusData@{ corpusId, corpusNode: NodePoly poly, defaultListId } ->
          let { date, hyperdata : Hyperdata h, name } = poly
              CorpusInfo { authors, desc, query } = getCorpusInfo h.fields
          in
          R.fragment [
            Table.tableHeaderLayout {
                afterCacheStateChange
              , cacheState
              , date
              , desc
              , key: "listsLayoutWithKey-header-" <> (show $ fst cacheState)
              , query
              , title: "Corpus " <> name
              , user: authors }
          , Tabs.tabs {
               appReload
             , asyncTasksRef
             , cacheState
             , corpusData
             , corpusId
             , key: "listsLayoutWithKey-tabs-" <> (show $ fst cacheState)
             , session
             , treeReloadRef
             }
          ]
      where
        afterCacheStateChange cacheState = do
          launchAff_ $ clearCache unit
          sessionUpdate $ setCacheState session nodeId cacheState
------------------------------------------------------------------------
