module Gargantext.Components.Nodes.Corpus.Graph.Tabs where

import Prelude hiding (div)
import Data.Array (fromFoldable)
import Data.Tuple (Tuple(..), fst)
import Reactix as R
import Gargantext.Components.GraphExplorer.Types (GraphSideCorpus(..))
import Gargantext.Components.FacetsTable (docView)
import Gargantext.Components.Search (SearchQuery)
import Gargantext.Components.Table as T
import Gargantext.Components.Tab as Tab
import Gargantext.Ends (Frontends)
import Gargantext.Sessions (Session)
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.Nodes.Corpus.Graph.Tabs"

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
tabsCpt = R2.hooksComponent thisModule "tabs" cpt
  where
    cpt {frontends, query, session, sides} _ = do
      active <- R.useState' 0
      pure $ Tab.tabs {tabs: tabs', selected: fst active}
      where
        tabs' = fromFoldable $ tab frontends session query <$> sides

tab :: Frontends -> Session -> SearchQuery -> GraphSideCorpus -> Tuple String R.Element
tab frontends session query (GraphSideCorpus {corpusId: nodeId, corpusLabel, listId}) =
  Tuple corpusLabel (docView dvProps)
  where
    dvProps   = {frontends, session, nodeId, listId, query, chart, totalRecords: 0, container}
    chart     = mempty
    container = T.graphContainer {title: corpusLabel}

