module Gargantext.Components.Nodes.Corpus.Graph.Tabs where

import Prelude hiding (div)
import Data.Array (fromFoldable)
import Data.Tuple (Tuple(..))
import Reactix as R
import Toestand as T

import Gargantext.Components.GraphExplorer.Types (GraphSideCorpus(..))
import Gargantext.Components.FacetsTable (docView)
import Gargantext.Components.Search (SearchQuery)
import Gargantext.Components.Table as Table
import Gargantext.Components.Tab as Tab
import Gargantext.Ends (Frontends)
import Gargantext.Sessions (Session)
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Graph.Tabs"

type Props =
  ( frontends :: Frontends
  , query     :: SearchQuery
  , session   :: Session
  , sides     :: Array GraphSideCorpus
  )

tabs :: Record Props -> R.Element
tabs props = R.createElement tabsCpt props []

-- TODO no need for Children here
tabsCpt :: R.Component Props
tabsCpt = here.component "tabs" cpt
  where
    cpt {frontends, query, session, sides} _ = do
      activeTab <- T.useBox 0

      pure $ Tab.tabs { activeTab, tabs: tabs' }
      where
        tabs' = fromFoldable $ tab frontends session query <$> sides

tab :: Frontends -> Session -> SearchQuery -> GraphSideCorpus -> Tuple String R.Element
tab frontends session query (GraphSideCorpus {corpusId: nodeId, corpusLabel, listId}) =
  Tuple corpusLabel (docView dvProps)
  where
    dvProps   = {frontends, session, nodeId, listId, query, chart, totalRecords: 0, container}
    chart     = mempty
    container = Table.graphContainer {title: corpusLabel}
