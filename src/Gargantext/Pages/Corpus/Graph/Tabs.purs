module Gargantext.Pages.Corpus.Graph.Tabs where

import Prelude hiding (div)
import Data.Array (fromFoldable)
import Data.Tuple (Tuple(..), fst)
import Gargantext.Config (Ends)
import Gargantext.Components.GraphExplorer.Types (GraphSideCorpus(..))
import Gargantext.Components.FacetsTable (TextQuery, docView)
import Gargantext.Components.Table as T
import Gargantext.Components.Tab as Tab
import Reactix as R
import Reactix.DOM.HTML as H

type Props = ( ends :: Ends, query :: TextQuery, sides :: Array GraphSideCorpus )

tabs :: Record Props -> R.Element
tabs props = R.createElement tabsCpt props []

-- TODO no need for Children here
tabsCpt :: R.Component Props
tabsCpt = R.hooksComponent "G.P.Corpus.Graph.Tabs.tabs" cpt
  where
    cpt {ends, query, sides} _ = do
      active <- R.useState' 0
      pure $ Tab.tabs {tabs: tabs', selected: fst active}
      where
        tabs' = fromFoldable $ tab ends query <$> sides

tab :: Ends -> TextQuery -> GraphSideCorpus -> Tuple String R.Element
tab ends query (GraphSideCorpus {corpusId: nodeId, corpusLabel, listId}) =
  Tuple corpusLabel (docView dvProps)
  where
    dvProps = {ends, nodeId, listId, query, chart, totalRecords: 4736, container}
    -- TODO totalRecords: probably need to insert a corpusLoader.
    chart = mempty
    container = T.graphContainer {title: corpusLabel}

