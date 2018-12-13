module Gargantext.Pages.Corpus.Graph.Tabs where

import Prelude hiding (div)
import Data.Lens (view)
import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))
import Gargantext.Config (TabType(..), TabSubType(..))
import Gargantext.Components.GraphExplorer.Types (GraphSideCorpus(..))
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.Tab as Tab
import React (ReactElement, ReactClass, Children, createElement)
import Thermite ( Spec, PerformAction, Render, _performAction, _render
                , hideState, noState, cmapProps, simpleSpec, createClass
                )

type Props = { sides :: Array GraphSideCorpus }

tabsElt :: Props -> ReactElement
tabsElt props = createElement tabsClass props []

-- TODO no need for Children here
tabsClass :: ReactClass { sides :: Array GraphSideCorpus, children :: Children }
tabsClass = createClass "GraphTabs" pureTabs (const {})

pureTabs :: Spec {} Props Void
pureTabs = hideState (const {activeTab: 0}) statefulTabs

tab :: forall props state. GraphSideCorpus -> Tuple String (Spec state props Tab.Action)
tab (GraphSideCorpus {corpusId: nodeId, corpusLabel}) =
  Tuple corpusLabel $
    cmapProps (const {nodeId, chart, tabType: TabCorpus TabDocs, totalRecords: 4736}) $
      noState DT.docViewSpec
  where
    -- TODO totalRecords: probably need to insert a corpusLoader.
    chart = mempty

statefulTabs :: Spec Tab.State Props Tab.Action
statefulTabs =
  withProps (\{sides} -> Tab.tabs identity identity $ fromFoldable $ tab <$> sides)

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
