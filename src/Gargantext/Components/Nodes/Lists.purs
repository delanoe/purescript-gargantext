module Gargantext.Components.Nodes.Lists where

import Reactix as R
------------------------------------------------------------------------
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
import Gargantext.Components.Nodes.Corpus.Types (getCorpusInfo, CorpusInfo(..), Hyperdata(..))
import Gargantext.Components.Nodes.Lists.Tabs as Tabs
import Gargantext.Components.Table as Table
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.Nodes.Lists"
------------------------------------------------------------------------
------------------------------------------------------------------------

type Props = (
    nodeId :: Int
  , session :: Session
  )

listsLayout :: Record Props -> R.Element
listsLayout props = R.createElement listsLayoutCpt props []

listsLayoutCpt :: R.Component Props
listsLayoutCpt = R2.hooksComponent thisModule "listsLayout" cpt
  where
    cpt path@{ nodeId, session } _ = do
      let sid = sessionId session

      pure $ listsLayoutWithKey { key: show sid <> "-" <> show nodeId, nodeId, session }

type KeyProps = (
  key :: String
  | Props
  )

listsLayoutWithKey :: Record KeyProps -> R.Element
listsLayoutWithKey props = R.createElement listsLayoutWithKeyCpt props []

listsLayoutWithKeyCpt :: R.Component KeyProps
listsLayoutWithKeyCpt = R2.hooksComponent thisModule "listsLayoutWithKey" cpt
  where
    cpt { nodeId, session } _ = do
      let path = { nodeId, session }

      useLoader path loadCorpusWithChild $
        \corpusData@{ corpusId, corpusNode: NodePoly poly, defaultListId } ->
              let { date, hyperdata : Hyperdata h, name } = poly
                  CorpusInfo {desc,query,authors} = getCorpusInfo h.fields
           in
          R.fragment
          [ Table.tableHeaderLayout
            { title: "Corpus " <> name, desc, query, user:authors, date }
         , Tabs.tabs {session, corpusId, corpusData}]
------------------------------------------------------------------------
