module Gargantext.Components.GraphExplorer.Sidebar
  -- (Props, sidebar)
  where

import Control.Parallel (parTraverse)
import Data.Array (head, last, concat)
import Data.Int (fromString)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Sequence as Seq
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Reactix as R
import Reactix.DOM.HTML as RH
import Reactix.DOM.HTML as H
import Record as Record
import Record.Extra as RX
import Toestand as T

import Gargantext.Prelude

import Gargantext.Components.Lang (Lang(..))
import Gargantext.Components.Search (SearchType(..), SearchQuery(..))
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
import Gargantext.Types (CTabNgramType, NodeID, TabSubType(..), TabType(..), TermList(..), modeTabType)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Sidebar"

type Common = (
    graphId         :: NodeID
  , metaData        :: GET.MetaData
  , reloadForest    :: T.Box T2.Reload
  , removedNodeIds  :: T.Box SigmaxT.NodeIds
  , selectedNodeIds :: T.Box SigmaxT.NodeIds
  , session         :: Session
  )

type Props = (
    frontends       :: Frontends
  , graph           :: SigmaxT.SGraph
  , graphVersion    :: T2.ReloadS
  , showSidePanel   :: T.Box GET.SidePanelState
  | Common
  )

sidebar :: Record Props -> R.Element
sidebar props = R.createElement sidebarCpt props []

sidebarCpt :: R.Component Props
sidebarCpt = here.component "sidebar" cpt
  where
    cpt props@{ metaData, showSidePanel } _ = do
      showSidePanel' <- T.useLive T.unequal showSidePanel

      case showSidePanel' of
        GET.Closed -> pure $ RH.div {} []
        GET.InitialClosed -> pure $ RH.div {} []
        GET.Opened sideTabT -> do
          let sideTab' = case sideTabT of
                SideTabLegend -> sideTabLegend sideTabProps []
                SideTabData -> sideTabData sideTabProps []
                SideTabCommunity -> sideTabCommunity sideTabProps []
          pure $ RH.div { id: "sp-container" }
            [ sideTabNav { sidePanel: showSidePanel
                         , sideTabs: [SideTabLegend, SideTabData, SideTabCommunity] } []
            , sideTab'
            ]
      where
        sideTabProps = RX.pick props :: Record SideTabProps

type SideTabNavProps = (
    sidePanel :: T.Box GET.SidePanelState
  , sideTabs  :: Array SideTab
  )

sideTabNav :: R2.Component SideTabNavProps
sideTabNav = R.createElement sideTabNavCpt

sideTabNavCpt :: R.Component SideTabNavProps
sideTabNavCpt = here.component "sideTabNav" cpt
  where
    cpt { sidePanel
        , sideTabs } _ = do
      sidePanel' <- T.useLive T.unequal sidePanel

      pure $ R.fragment [ H.div { className: "text-primary center"} [H.text ""]
                        , H.div { className: "nav nav-tabs"} (liItem sidePanel' <$> sideTabs)
                            -- , H.div {className: "center"} [ H.text "Doc sideTabs"]
                        ]
      where
        liItem :: GET.SidePanelState -> SideTab -> R.Element
        liItem sidePanel' tab =
          H.div { className : "nav-item nav-link"
                            <> if (Opened tab) == sidePanel'
                                 then " active"
                                 else ""
              , on: { click: \_ -> T.write (Opened tab) sidePanel
                    }
              } [ H.text $ show tab ]

type SideTabProps = Props

sideTabLegend :: R2.Component SideTabProps
sideTabLegend = R.createElement sideTabLegendCpt

sideTabLegendCpt :: R.Component SideTabProps
sideTabLegendCpt = here.component "sideTabLegend" cpt
  where
    cpt props@{ metaData: GET.MetaData { legend } } _ = do
      pure $ H.div {}
        [ Legend.legend { items: Seq.fromFoldable legend }
        , documentation EN
        ]

sideTabData :: R2.Component SideTabProps
sideTabData = R.createElement sideTabDataCpt

sideTabDataCpt :: R.Component SideTabProps
sideTabDataCpt = here.component "sideTabData" cpt
  where
    cpt props _ = do
      selectedNodeIds' <- T.useLive T.unequal props.selectedNodeIds

      pure $ RH.div {}
        [ selectedNodes (Record.merge { nodesMap: SigmaxT.nodesGraphMap props.graph } props) []
        , neighborhood props []
        , RH.div { className: "col-md-12", id: "query" }
          [ query SearchDoc
            props.frontends
            props.metaData
            props.session
            (SigmaxT.nodesGraphMap props.graph)
            selectedNodeIds'
          ]
        ]
        where
          checkbox text = RH.li {}
                          [ RH.span {} [ RH.text text ]
                          , RH.input { type: "checkbox"
                                     , className: "checkbox"
                                     , defaultChecked: true
                                     , title: "Mark as completed" } ]


sideTabCommunity :: R2.Component SideTabProps
sideTabCommunity = R.createElement sideTabCommunityCpt

sideTabCommunityCpt :: R.Component SideTabProps
sideTabCommunityCpt = here.component "sideTabCommunity" cpt
  where
    cpt props _ = do
      selectedNodeIds' <- T.useLive T.unequal props.selectedNodeIds

      pure $ RH.div { className: "col-md-12", id: "query" }
        [ selectedNodes (Record.merge { nodesMap: SigmaxT.nodesGraphMap props.graph } props) []
        , neighborhood props []
        , query SearchContact
          props.frontends
          props.metaData
          props.session
          (SigmaxT.nodesGraphMap props.graph)
          selectedNodeIds'
        ]


-------------------------------------------
-- TODO
-- selectedNodes :: Record Props -> Map.Map String Nodes -> R.Element

type SelectedNodesProps = (
  nodesMap :: SigmaxT.NodesMap
  | Props
  )

selectedNodes :: R2.Component SelectedNodesProps
selectedNodes = R.createElement selectedNodesCpt

selectedNodesCpt :: R.Component SelectedNodesProps
selectedNodesCpt = here.component "selectedNodes" cpt
  where
    cpt props@{ graph
              , nodesMap
              , selectedNodeIds } _ = do
      selectedNodeIds' <- T.useLive T.unequal selectedNodeIds

      pure $ R2.row
        [ R2.col 12
          [ RH.ul { className: "nav nav-tabs d-flex justify-content-center"
                  , id: "myTab"
                  , role: "tablist" }
            [ RH.div { className: "tab-content" }
              [ RH.div { className: "d-flex flex-wrap justify-content-center"
                       , role: "tabpanel" }
                ( Seq.toUnfoldable
                  $ ( Seq.map (badge selectedNodeIds)
                      (badges graph selectedNodeIds')
                    )
                )
              , H.br {}
              ]
            ]
          , RH.div { className: "tab-content flex-space-between" }
            [ removeButton (Record.merge { buttonType: "primary"
                                         , rType: CandidateTerm
                                         , nodesMap
                                         , text: "Move as candidate" } commonProps) []
            , H.br {}
            , removeButton (Record.merge { buttonType: "danger"
                                         , nodesMap
                                         , rType: StopTerm
                                         , text: "Move as stop" } commonProps) []
            ]
          ]
        ]
      where
        commonProps = RX.pick props :: Record Common

neighborhood :: R2.Component Props
neighborhood = R.createElement neighborhoodCpt

neighborhoodCpt :: R.Component Props
neighborhoodCpt = here.component "neighborhood" cpt
  where
    cpt { graph
        , selectedNodeIds } _ = do
      selectedNodeIds' <- T.useLive T.unequal selectedNodeIds

      pure $ RH.div { className: "tab-content", id: "myTabContent" }
        [ RH.div { -- className: "flex-space-around d-flex justify-content-center"
             className: "d-flex flex-wrap flex-space-around"
             , id: "home"
             , role: "tabpanel"
             }
          (Seq.toUnfoldable $ Seq.map (badge selectedNodeIds)
           $ neighbourBadges graph selectedNodeIds'
          )
        ]


type RemoveButtonProps = (
    buttonType :: String
  , nodesMap   :: SigmaxT.NodesMap
  , rType      :: TermList
  , text       :: String
  | Common
  )

removeButton :: R2.Component RemoveButtonProps
removeButton = R.createElement removeButtonCpt

removeButtonCpt :: R.Component RemoveButtonProps
removeButtonCpt = here.component "removeButton" cpt
  where
    cpt { buttonType
        , graphId
        , metaData
        , nodesMap
        , reloadForest
        , removedNodeIds
        , rType
        , selectedNodeIds
        , session
        , text } _ = do
      selectedNodeIds' <- T.useLive T.unequal selectedNodeIds

      pure $ if Set.isEmpty selectedNodeIds' then
               RH.div {} []
             else
               RH.button { className: "btn btn-sm btn-" <> buttonType
                         , on: { click: onClickRemove selectedNodeIds' }
                         } [ RH.text text ]
      where
        onClickRemove selectedNodeIds' e = do
          let nodes = mapMaybe (\id -> Map.lookup id nodesMap)
                              $ Set.toUnfoldable selectedNodeIds'
          deleteNodes { graphId: graphId
                      , metaData: metaData
                      , nodes
                      , session: session
                      , termList: rType
                      , reloadForest }
          T.write_ selectedNodeIds' removedNodeIds
          T.write_ SigmaxT.emptyNodeIds selectedNodeIds



badge :: T.Box SigmaxT.NodeIds -> Record SigmaxT.Node -> R.Element
badge selectedNodeIds {id, label} =
  RH.a { className: "badge badge-pill badge-light"
       , on: { click: onClick }
       } [ RH.h6 {} [ RH.text label ] ]
  where
    onClick e = do
      T.write_ (Set.singleton id) selectedNodeIds

badges :: SigmaxT.SGraph -> SigmaxT.NodeIds -> Seq.Seq (Record SigmaxT.Node)
badges graph selectedNodeIds = SigmaxT.graphNodes $ SigmaxT.nodesById graph selectedNodeIds

neighbourBadges :: SigmaxT.SGraph -> SigmaxT.NodeIds -> Seq.Seq (Record SigmaxT.Node)
neighbourBadges graph selectedNodeIds = SigmaxT.neighbours graph selectedNodes' where
  selectedNodes' = SigmaxT.graphNodes $ SigmaxT.nodesById graph selectedNodeIds

type DeleteNodes =
  ( graphId      :: NodeID
  , metaData     :: GET.MetaData
  , nodes        :: Array (Record SigmaxT.Node)
  , reloadForest :: T.Box T2.Reload
  , session      :: Session
  , termList     :: TermList
  )

deleteNodes :: Record DeleteNodes -> Effect Unit
deleteNodes { graphId, metaData, nodes, session, termList, reloadForest } = do
  launchAff_ do
    patches <- (parTraverse (deleteNode termList session metaData) nodes) :: Aff (Array NTC.VersionedNgramsPatches)
    let mPatch = last patches
    case mPatch of
      Nothing -> pure unit
      Just (NTC.Versioned patch) -> do
        liftEffect $ T2.reload reloadForest

-- Why is this called delete node?
deleteNode :: TermList
           -> Session
           -> GET.MetaData
           -> Record SigmaxT.Node
           -> Aff NTC.VersionedNgramsPatches
deleteNode termList session (GET.MetaData metaData) node = do
    ret  <- NTC.putNgramsPatches coreParams versioned
    task <- NTC.postNgramsChartsAsync coreParams  -- TODO add task
    pure ret
  where
    nodeId :: NodeID
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

query :: SearchType
      -> Frontends
      -> GET.MetaData
      -> Session
      -> SigmaxT.NodesMap
      -> SigmaxT.NodeIds
      -> R.Element
query _ _ _ _ _ selectedNodeIds | Set.isEmpty selectedNodeIds = RH.div {} []
query searchType frontends (GET.MetaData metaData) session nodesMap selectedNodeIds =
  query' (head metaData.corpusId)
  where
    query' Nothing         = RH.div {} []
    query' (Just corpusId) =
      CGT.tabs { frontends
               , session
               , query: SearchQuery { query : concat $ toQuery <$> Set.toUnfoldable selectedNodeIds
                                    , expected: searchType
                                    }
               , sides: [side corpusId]
               }

    toQuery id = case Map.lookup id nodesMap of
      Nothing -> []
      Just n -> words n.label

    side corpusId = GET.GraphSideCorpus { corpusId
                                        , listId     : metaData.list.listId
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
--------------------------------------------------------------------------


documentation :: Lang -> R.Element
documentation _ =
  H.div {} [ H.h2 {} [ H.text "What is Graph ?"]
           , ul [ "Graph is a conveniant tool to explore your documents. "
                , "Nodes are terms selected in your Map List. "
                <> "Node size is proportional to the number of documents with the associated term. "
                , "Edges between nodes represent proximities of terms according to a specific distance between your documents. "
                <> "Link strength is proportional to the strenght of terms association."
                ]
           , H.h3 {} [ H.text "Basic Interactions:"]
           , ul [ "Click on a node to select/unselect and get its information. "
                , "In case of multiple selection, the button unselect clears all selections. "
                <> "Use your mouse scroll to zoom in and out in the graph. "
                , "Use the node filter to create a subgraph with nodes of a given size "
                <>"range (e.g. display only generic terms). "
                , "Use the edge filter so create a subgraph with links in a given range (e.g. keep the strongest association)."
                ]
           ]

  where
    ul ts = H.ul {} $ map (\t -> H.li {} [ H.text t ]) ts

{-
TODO DOC
  Conditional distance between the terms X and Y is the probability to have both terms X and Y in the same textual context.
  Distributional distance between the terms X and Y is the probability to have same others terms in the same textual context as X or Y.

Global/local view:
    The 'change level' button allows to change between global view and node centered view,
    To explore the neighborhood of a selection click on the 'change level' button.
-}

