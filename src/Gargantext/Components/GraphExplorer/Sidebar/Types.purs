module Gargantext.Components.GraphExplorer.Sidebar.Types where

import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Reactix as R
import Toestand as T

import Gargantext.Prelude

import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Hooks.Sigmax.Types as SigmaxT

type SidePanel =
  (
    mGraph             :: Maybe SigmaxT.SGraph
  , mMetaData          :: Maybe GET.MetaData
  , multiSelectEnabled :: Boolean
  , removedNodeIds     :: SigmaxT.NodeIds
  , selectedNodeIds    :: SigmaxT.NodeIds
  , showControls       :: Boolean
  , sideTab            :: GET.SideTab
  )

initialSidePanel :: Maybe (Record SidePanel)
initialSidePanel = Nothing


focusedSidePanel :: T.Box (Maybe (Record SidePanel))
                 -> R.Hooks { mGraph              :: T.Box (Maybe SigmaxT.SGraph)
                            , mMetaData          :: T.Box (Maybe GET.MetaData)
                            , multiSelectEnabled :: T.Box Boolean
                            , removedNodeIds     :: T.Box SigmaxT.NodeIds
                            , selectedNodeIds    :: T.Box SigmaxT.NodeIds
                            , showControls       :: T.Box Boolean
                            , sideTab            :: T.Box GET.SideTab }
focusedSidePanel sidePanel = do
  mGraph <- T.useFocused
            (maybe Nothing _.mGraph)
            (\val -> maybe Nothing (\sp -> Just $ sp { mGraph = val })) sidePanel
  mMetaData <- T.useFocused
            (maybe Nothing _.mMetaData)
            (\val -> maybe Nothing (\sp -> Just $ sp { mMetaData = val })) sidePanel
  multiSelectEnabled <- T.useFocused
                        (maybe false _.multiSelectEnabled)
                        (\val -> maybe Nothing (\sp -> Just $ sp { multiSelectEnabled = val })) sidePanel
  removedNodeIds <- T.useFocused
                     (maybe Set.empty _.removedNodeIds)
                     (\val -> maybe Nothing (\sp -> Just $ sp { removedNodeIds = val })) sidePanel
  selectedNodeIds <- T.useFocused
                     (maybe Set.empty _.selectedNodeIds)
                     (\val -> maybe Nothing (\sp -> Just $ sp { selectedNodeIds = val })) sidePanel
  showControls <- T.useFocused
                  (maybe false _.showControls)
                  (\val -> maybe Nothing (\sp -> Just $ sp { showControls = val })) sidePanel
  sideTab <- T.useFocused
                  (maybe GET.SideTabLegend _.sideTab)
                  (\val -> maybe Nothing (\sp -> Just $ sp { sideTab = val })) sidePanel

  pure $ {
    mGraph
  , mMetaData
  , multiSelectEnabled
  , removedNodeIds
  , selectedNodeIds
  , showControls
  , sideTab
  }
