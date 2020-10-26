module Gargantext.Components.Nodes.Lists where

import Effect (Effect)
import Effect.Aff (launchAff_)
import Reactix as R
------------------------------------------------------------------------
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
import Gargantext.Utils.Reactix as R2

thisModule :: String
thisModule = "Gargantext.Components.Nodes.Lists"
------------------------------------------------------------------------
------------------------------------------------------------------------

type Props = (
    nodeId :: Int
  , session :: Session
  , sessionUpdate :: Session -> Effect Unit
  )

listsLayout :: Record Props -> R.Element
listsLayout props = R.createElement listsLayoutCpt props []

listsLayoutCpt :: R.Component Props
listsLayoutCpt = R.hooksComponentWithModule thisModule "listsLayout" cpt
  where
    cpt path@{ nodeId, session, sessionUpdate } _ = do
      let sid = sessionId session

      pure $ listsLayoutWithKey { key: show sid <> "-" <> show nodeId, nodeId, session, sessionUpdate }

type KeyProps = (
  key :: String
  | Props
  )

listsLayoutWithKey :: Record KeyProps -> R.Element
listsLayoutWithKey props = R.createElement listsLayoutWithKeyCpt props []

listsLayoutWithKeyCpt :: R.Component KeyProps
listsLayoutWithKeyCpt = R.hooksComponentWithModule thisModule "listsLayoutWithKey" cpt
  where
    cpt { nodeId, session, sessionUpdate } _ = do
      let path = { nodeId, session }

      cacheState <- R.useState' $ getCacheState NT.CacheOn session nodeId

      useLoader path loadCorpusWithChild $
        \corpusData@{ corpusId, corpusNode: NodePoly poly, defaultListId } ->
              let { date, hyperdata : Hyperdata h, name } = poly
                  CorpusInfo {desc,query,authors} = getCorpusInfo h.fields
           in
          R.fragment [
            Table.tableHeaderLayout {
                afterCacheStateChange
              , cacheState
              , date
              , desc
              , query
              , title: "Corpus " <> name
              , user: authors }
          , Tabs.tabs {
               cacheState
             , corpusData
             , corpusId
             , session }
          ]
      where
        afterCacheStateChange cacheState = do
          launchAff_ $ clearCache unit
          sessionUpdate $ setCacheState session nodeId cacheState
------------------------------------------------------------------------
