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
import Gargantext.Sessions (Session)
------------------------------------------------------------------------
------------------------------------------------------------------------

type Props = ( session :: Session, nodeId :: Int )

listsLayout :: Record Props -> R.Element
listsLayout props = R.createElement listsLayoutCpt props []

listsLayoutCpt :: R.Component Props
listsLayoutCpt = R.hooksComponent "G.P.Lists.listsLayout" cpt
  where
    cpt path@{session} _ =
      useLoader path loadCorpusWithChild $
        \corpusData@{corpusId, defaultListId, corpusNode: NodePoly poly} ->
              let { name, date, hyperdata : Hyperdata h} = poly
                  CorpusInfo {desc,query,authors} = getCorpusInfo h.fields
           in
          R.fragment
          [ Table.tableHeaderLayout
            { title: "Corpus " <> name, desc, query, user:authors, date }
         , Tabs.tabs {session, corpusId, corpusData}]
------------------------------------------------------------------------
