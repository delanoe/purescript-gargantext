module Gargantext.Pages.Corpus.Graph.Tabs where

import Prelude hiding (div)
import Data.Lens (view)
import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))
import Gargantext.Components.GraphExplorer.Types (GraphSideCorpus(..))
import Gargantext.Components.FacetsTable (TextQuery, docViewSpec)
import Gargantext.Components.Table as T
import Gargantext.Components.Tab as Tab
import React (ReactElement, ReactClass, Children, createElement)
import Thermite ( Spec, PerformAction, Render, _performAction, _render
                , hideState, noState, cmapProps, simpleSpec, createClass
                )

type Props = { query :: TextQuery, sides :: Array GraphSideCorpus }

tabsElt :: Props -> ReactElement
tabsElt props = createElement tabsClass props []

-- TODO no need for Children here
tabsClass :: ReactClass { query :: TextQuery, sides :: Array GraphSideCorpus, children :: Children }
tabsClass = createClass "GraphTabs" pureTabs (const {})

pureTabs :: Spec {} Props Void
pureTabs = hideState (const {activeTab: 0}) statefulTabs

tab :: forall props state. TextQuery -> GraphSideCorpus -> Tuple String (Spec state props Tab.Action)
tab query (GraphSideCorpus {corpusId: nodeId, corpusLabel, listId}) =
  Tuple corpusLabel $
    cmapProps (const {nodeId, listId, query, chart, totalRecords: 4736, container}) $
      noState docViewSpec
  where
    -- TODO totalRecords: probably need to insert a corpusLoader.
    chart = mempty
    container = T.graphContainer {title: corpusLabel}

statefulTabs :: Spec Tab.State Props Tab.Action
statefulTabs =
  withProps (\{query, sides} ->
    Tab.tabs identity identity $ fromFoldable $ tab query <$> sides)

-- TODO move to Thermite
-- | This function captures the props of the `Spec` as a function argument.
withProps
  :: forall state props action
   . (props -> Spec state props action)
  -> Spec state props action
withProps f = simpleSpec performAction render
  where
    performAction :: PerformAction state props action
    performAction a p st = view _performAction (f p) a p st

    render :: Render state props action
    render k p st = view _render (f p) k p st
