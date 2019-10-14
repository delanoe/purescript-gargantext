module Gargantext.Components.Nodes.Texts where

import Data.Maybe (Maybe(..))
import Reactix as R
--------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Components.Loader (loader)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Table as Table
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Components.Nodes.Corpus (CorpusInfo(..), loadCorpus)
import Gargantext.Components.Nodes.Texts.Tabs as Tabs
import Gargantext.Sessions (Session)
import Gargantext.Types (NodeType(..))

type Props = ( session :: Session, nodeId :: Int )

textsLayout :: Record Props -> R.Element
textsLayout props = R.createElement textsLayoutCpt props []

------------------------------------------------------------------------
textsLayoutCpt :: R.Component Props
textsLayoutCpt = R.hooksComponent "G.P.Texts.textsLayout" cpt where
  cpt path@{session} _ = do
    pure $ loader path loadCorpus paint
    where
      paint corpusData@{corpusId, corpusNode, defaultListId} =
        R.fragment [ Table.tableHeaderLayout headerProps, tabs ]
        where
          NodePoly { name, date, hyperdata: CorpusInfo corpus } = corpusNode
          {desc, query, authors: user} = corpus
          tabs = Tabs.tabs {session, corpusId, corpusData}
          title = "Corpus " <> name
          headerProps = { title, desc, query, date, user }
