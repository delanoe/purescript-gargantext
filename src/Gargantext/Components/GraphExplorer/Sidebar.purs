module Gargantext.Components.GraphExplorer.Sidebar
  (Props, sidebar)
  where

import Control.Parallel (parTraverse)
import Data.Array (head, last)
import Data.Int (fromString)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.GraphExplorer.Types  as GET
import Gargantext.Components.GraphExplorer.Types  (SidePanelState(..), SideTab(..))
import Gargantext.Components.GraphExplorer.Legend as Legend
import Gargantext.Components.NgramsTable.Core as NTC
import Gargantext.Components.Nodes.Corpus.Graph.Tabs (tabs) as CGT
import Gargantext.Components.RandomText (words)
import Gargantext.Data.Array (mapMaybe)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType, TabSubType(..), TabType(..), TermList(..), modeTabType)
import Gargantext.Utils.Reactix as R2
import Partial.Unsafe (unsafePartial)
import Gargantext.Prelude
import Reactix as R
import Reactix.DOM.HTML as RH
import Reactix.DOM.HTML as H

type Props =
  ( frontends       :: Frontends
  , graph           :: SigmaxT.SGraph
  , graphId         :: Int
  , graphVersion    :: R.State Int
  , metaData        :: GET.MetaData
  , removedNodeIds  :: R.State SigmaxT.NodeIds
  , selectedNodeIds :: R.State SigmaxT.NodeIds
  , session         :: Session
  , showSidePanel   :: R.State GET.SidePanelState
  , treeReload      :: R.State Int
  )

sidebar :: Record Props -> R.Element
sidebar props = R.createElement sidebarCpt props []

sidebarCpt :: R.Component Props
sidebarCpt = R.hooksComponent "Sidebar" cpt
  where
    cpt {showSidePanel: (GET.Closed /\ _)} _children = do
      pure $ RH.div {} []
    cpt {showSidePanel: (GET.InitialClosed /\ _)} _children = do
      pure $ RH.div {} []
    cpt props@{metaData, showSidePanel} _children = do
      pure $ RH.div { id: "sp-container" }
        [ sideTabNav showSidePanel [SideTabLegend, SideTabSelection, SideTabPairing]
        , sideTab (fst showSidePanel) props
        ]

sideTabNav :: R.State SidePanelState -> Array SideTab -> R.Element
sideTabNav (sidePanel /\ setSidePanel) sideTabs =
  R.fragment [ H.div { className: "text-primary center"} [H.text "SideTab"]
                     , H.div {className: "nav nav-tabs"} (liItem <$> sideTabs)
                     -- , H.div {className: "center"} [ H.text "Doc sideTabs"]
             ]
    where
      liItem :: SideTab -> R.Element
      liItem  tab =
        H.div { className : "nav-item nav-link"
                          <> if (Opened tab) == sidePanel
                               then " active"
                               else ""
            , on: { click: \_ -> setSidePanel $ const (Opened tab)
                  }
            } [ H.text $ show tab ]

sideTab :: SidePanelState -> Record Props -> R.Element
sideTab (Opened SideTabLegend) props@{metaData} =
  let (GET.MetaData {legend}) = metaData
                    in Legend.legend { items: Seq.fromFoldable legend}

sideTab (Opened SideTabSelection) props =
  RH.div {} [ R2.row [ R2.col 12
              [ RH.ul { id: "myTab", className: "nav nav-tabs", role: "tablist"}
                [ RH.div { className: "tab-content" }
                  [ RH.div { className: "", role: "tabpanel" }
                           ( Seq.toUnfoldable
                           $ ( Seq.map (badge              props.selectedNodeIds)
                                       (badges props.graph props.selectedNodeIds)
                             )
                           )
                  ]
                , RH.div { className: "tab-content" }
                         [ removeButton "Remove candidate" CandidateTerm props nodesMap
                         , removeButton "Remove stop"      StopTerm      props nodesMap
                         ]

                  ]

                , RH.div { className: "col-md-12", id: "query" }
                  [ query props.frontends props.metaData props.session nodesMap props.selectedNodeIds]
                  ]
                ]
              , RH.div { className: "tab-content", id: "myTabContent" }
                [ RH.div { className: "", id: "home", role: "tabpanel" }
                  (Seq.toUnfoldable $ (Seq.map (badge props.selectedNodeIds) (neighbourBadges props.graph props.selectedNodeIds)))
                ]
              ]
    where

      nodesMap = SigmaxT.nodesGraphMap props.graph

      checkbox text =
        RH.li {}
        [ RH.span {} [ RH.text text ]
        , RH.input { type: "checkbox"
                   , className: "checkbox"
                   , checked: true
                   , title: "Mark as completed" } ]

      removeButton text rType props nodesMap =
        if Set.isEmpty $ fst props.selectedNodeIds then
          RH.div {} []
        else
          RH.button { className: "btn btn-danger"
                    , on: { click: onClickRemove rType props nodesMap }}
          [ RH.text text ]

      onClickRemove rType props nodesMap e = do
        let nodes = mapMaybe (\id -> Map.lookup id nodesMap)
                             $ Set.toUnfoldable $ fst props.selectedNodeIds
        deleteNodes { graphId: props.graphId
                    , metaData: props.metaData
                    , nodes
                    , session: props.session
                    , termList: rType
                    , treeReload: props.treeReload }
        snd props.removedNodeIds  $ const $ fst props.selectedNodeIds
        snd props.selectedNodeIds $ const SigmaxT.emptyNodeIds



sideTab _ _  = H.div {} []



-------------------------------------------
badge :: R.State SigmaxT.NodeIds -> Record SigmaxT.Node -> R.Element
badge (_ /\ setNodeIds) {id, label} =
  RH.a { className: "badge badge-light"
       , on: { click: onClick }
       } [ RH.text label ]
  where
    onClick e = do
      setNodeIds $ const $ Set.singleton id

badges :: SigmaxT.SGraph -> R.State SigmaxT.NodeIds -> Seq.Seq (Record SigmaxT.Node)
badges graph (selectedNodeIds /\ _) = SigmaxT.graphNodes $ SigmaxT.nodesById graph selectedNodeIds

neighbourBadges :: SigmaxT.SGraph -> R.State SigmaxT.NodeIds -> Seq.Seq (Record SigmaxT.Node)
neighbourBadges graph (selectedNodeIds /\ _) = SigmaxT.neighbours graph selectedNodes
  where
    selectedNodes = SigmaxT.graphNodes $ SigmaxT.nodesById graph selectedNodeIds


type DeleteNodes =
  ( graphId :: Int
  , metaData :: GET.MetaData
  , nodes :: Array (Record SigmaxT.Node)
  , session :: Session
  , termList :: TermList
  , treeReload :: R.State Int
  )

deleteNodes :: Record DeleteNodes -> Effect Unit
deleteNodes { graphId, metaData, nodes, session, termList, treeReload } = do
  launchAff_ do
    patches <- (parTraverse (deleteNode termList session metaData) nodes) :: Aff (Array NTC.VersionedNgramsPatches)
    let mPatch = last patches
    case mPatch of
      Nothing -> pure unit
      Just (NTC.Versioned patch) -> do
        liftEffect $ snd treeReload $ (+) 1

deleteNode :: TermList -> Session -> GET.MetaData -> Record SigmaxT.Node -> Aff NTC.VersionedNgramsPatches
deleteNode termList session (GET.MetaData metaData) node = NTC.putNgramsPatches coreParams versioned
  where
    nodeId :: Int
    nodeId = unsafePartial $ fromJust $ fromString node.id

    versioned :: NTC.VersionedNgramsPatches
    versioned = NTC.Versioned {version: metaData.list.version, data: np}

    coreParams :: NTC.CoreParams ()
    coreParams = {session, nodeId, listIds: [metaData.list.listId], tabType}

    tabNgramType :: CTabNgramType
    tabNgramType = modeTabType node.gargType

    tabType :: TabType
    tabType = TabCorpus (TabNgramType tabNgramType)

    term :: NTC.NgramsTerm
    term = NTC.normNgram tabNgramType node.label

    pt :: NTC.NgramsTablePatch
    pt = NTC.fromNgramsPatches np

    np :: NTC.NgramsPatches
    np = NTC.singletonPatchMap term $ NTC.NgramsPatch { patch_children: mempty, patch_list }

    patch_list :: NTC.Replace TermList
    patch_list = NTC.Replace { new: termList, old: MapTerm }

query :: Frontends -> GET.MetaData -> Session -> SigmaxT.NodesMap -> R.State SigmaxT.NodeIds -> R.Element
query _ _ _ _ (selectedNodeIds /\ _) | Set.isEmpty selectedNodeIds = RH.div {} []
query frontends (GET.MetaData metaData) session nodesMap (selectedNodeIds /\ _) =
  query' (head metaData.corpusId)
  where
    query' Nothing = RH.div {} []
    query' (Just corpusId) =
      CGT.tabs {frontends, session, query: q <$> Set.toUnfoldable selectedNodeIds, sides: [side corpusId]}
    q id = case Map.lookup id nodesMap of
      Nothing -> []
      Just n -> words n.label
    side corpusId = GET.GraphSideCorpus {
          corpusId
        , listId: metaData.list.listId
        , corpusLabel: metaData.title
        }

------------------------------------------------------------------------

            {-, RH.div { className: "col-md-12", id: "horizontal-checkbox" }
              [ RH.ul {}
                [ checkbox "Pubs"
                , checkbox "Projects"
                , checkbox "Patents"
                , checkbox "Others"
                ]
              ]
              -}



