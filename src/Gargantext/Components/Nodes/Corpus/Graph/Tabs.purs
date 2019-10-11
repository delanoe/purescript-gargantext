module Gargantext.Components.Nodes.Corpus.Graph.Tabs where

import Prelude hiding (div)
import Data.Array (fromFoldable)
import Data.Tuple (Tuple(..), fst)
import Reactix as R
import Gargantext.Components.GraphExplorer.Types (GraphSideCorpus(..))
import Gargantext.Components.FacetsTable (TextQuery, docView)
import Gargantext.Components.Table as T
import Gargantext.Components.Tab as Tab
import Gargantext.Sessions (Session)

type Props = ( session :: Session, query :: TextQuery, sides :: Array GraphSideCorpus )

tabs :: Record Props -> R.Element
tabs props = R.createElement tabsCpt props []

-- TODO no need for Children here
tabsCpt :: R.Component Props
tabsCpt = R.hooksComponent "G.P.Corpus.Graph.Tabs.tabs" cpt
  where
    cpt {session, query, sides} _ = do
      active <- R.useState' 0
      pure $ Tab.tabs {tabs: tabs', selected: fst active}
      where
        tabs' = fromFoldable $ tab session query <$> sides

tab :: Session -> TextQuery -> GraphSideCorpus -> Tuple String R.Element
tab session query (GraphSideCorpus {corpusId: nodeId, corpusLabel, listId}) =
  Tuple corpusLabel (docView dvProps)
  where
    dvProps = {session, nodeId, listId, query, chart, totalRecords: 4736, container}
    -- TODO totalRecords: probably need to insert a corpusLoader.
    chart = mempty
    container = T.graphContainer {title: corpusLabel}

