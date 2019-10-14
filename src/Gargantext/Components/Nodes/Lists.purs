module Gargantext.Components.Nodes.Lists where

import Reactix as R
--------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Components.Nodes.Corpus (CorpusInfo(..), loadCorpus)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Table as Table
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Components.Nodes.Lists.Tabs as Tabs
import Gargantext.Sessions (Session)

------------------------------------------------------------------------

type Props = ( session :: Session, nodeId :: Int )

listsLayout :: Record Props -> R.Element
listsLayout props = R.createElement listsLayoutCpt props []

listsLayoutCpt :: R.Component Props
listsLayoutCpt = R.hooksComponent "G.P.Lists.listsLayout" cpt
  where
    cpt path@{session} _ =
      useLoader path loadCorpus $
        \corpusData@{corpusId, defaultListId, corpusNode: NodePoly poly} ->
          let { name, date, hyperdata: CorpusInfo corpus } = poly
              { desc, query, authors: user } = corpus in
          R.fragment
          [ Table.tableHeaderLayout
            { title: "Corpus " <> name, desc, query, user, date }
         , Tabs.tabs {session, corpusId, corpusData}]
------------------------------------------------------------------------
