module Gargantext.Components.GraphExplorer.Sidebar
  -- (Props, sidebar)
  where

import Gargantext.Prelude

import Control.Parallel (parTraverse)
import Data.Array (head, last, concat)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable as F
import Data.Int (fromString)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Sequence as Seq
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.GraphExplorer.Legend as Legend
import Gargantext.Components.GraphExplorer.Sidebar.Types as GEST
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.Lang (Lang(..))
import Gargantext.Components.NgramsTable.Core as NTC
import Gargantext.Components.Nodes.Corpus.Graph.Tabs (tabs) as CGT
import Gargantext.Components.RandomText (words)
import Gargantext.Components.Search (SearchType(..), SearchQuery(..))
import Gargantext.Config.REST (RESTError)
import Gargantext.Data.Array (mapMaybe)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType, FrontendError(..), NodeID, TabSubType(..), TabType(..), TermList(..), modeTabType)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Math as Math
import Partial.Unsafe (unsafePartial)
import Reactix as R
import Reactix.DOM.HTML as H
import Reactix.DOM.HTML as RH
import Record as Record
import Record.Extra as RX
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Sidebar"

type Common = (
    boxes           :: Boxes
  , graphId         :: NodeID
  , metaData        :: GET.MetaData
  , session         :: Session
  )

type Props = (
    frontends       :: Frontends
  , graph           :: SigmaxT.SGraph
  | Common
  )

sidebar :: R2.Component Props
sidebar = R.createElement sidebarCpt
sidebarCpt :: R.Component Props
sidebarCpt = here.component "sidebar" cpt
  where
    cpt props@{ boxes: { sidePanelGraph } } _ = do
      { sideTab } <- GEST.focusedSidePanel sidePanelGraph
      sideTab' <- T.useLive T.unequal sideTab

      pure $ RH.div { id: "sp-container" }
        [ sideTabNav { sideTab
                     , sideTabs: [GET.SideTabLegend, GET.SideTabData, GET.SideTabCommunity] } []
        , case sideTab' of
            GET.SideTabLegend -> sideTabLegend sideTabProps []
            GET.SideTabData -> sideTabData sideTabProps []
            GET.SideTabCommunity -> sideTabCommunity sideTabProps []
        ]
      where
        sideTabProps = RX.pick props :: Record Props

type SideTabNavProps = (
    sideTab  :: T.Box GET.SideTab
  , sideTabs :: Array GET.SideTab
  )

sideTabNav :: R2.Component SideTabNavProps
sideTabNav = R.createElement sideTabNavCpt
sideTabNavCpt :: R.Component SideTabNavProps
sideTabNavCpt = here.component "sideTabNav" cpt
  where
    cpt { sideTab, sideTabs } _ = do
      sideTab' <- T.useLive T.unequal sideTab

      pure $ R.fragment [ H.div { className: "text-primary center"} [H.text ""]
                        , H.div { className: "nav nav-tabs"} (liItem sideTab' <$> sideTabs)
                            -- , H.div {className: "center"} [ H.text "Doc sideTabs"]
                        ]
      where
        liItem :: GET.SideTab -> GET.SideTab -> R.Element
        liItem sideTab' tab =
          H.div { className : "nav-item nav-link"
                            <> if tab == sideTab'
                                 then " active"
                                 else ""
                , on: { click: \_ -> T.write_ tab sideTab }
                } [ H.text $ show tab ]

sideTabLegend :: R2.Component Props
sideTabLegend = R.createElement sideTabLegendCpt
sideTabLegendCpt :: R.Component Props
sideTabLegendCpt = here.component "sideTabLegend" cpt
  where
    cpt { metaData: GET.MetaData { legend } } _ = do
      pure $ H.div {}
        [ Legend.legend { items: Seq.fromFoldable legend }
        , documentation EN
        ]

sideTabData :: R2.Component Props
sideTabData = R.createElement sideTabDataCpt
sideTabDataCpt :: R.Component Props
sideTabDataCpt = here.component "sideTabData" cpt
  where
    cpt props@{ boxes: { sidePanelGraph } } _ = do
      { selectedNodeIds } <- GEST.focusedSidePanel sidePanelGraph
      selectedNodeIds' <- T.useLive T.unequal selectedNodeIds

      pure $ RH.div {}
        [ selectedNodes (Record.merge { nodesMap: SigmaxT.nodesGraphMap props.graph } props) []
        , neighborhood props []
        , RH.div { className: "col-md-12", id: "query" }
          [ query { frontends: props.frontends
                  , metaData: props.metaData
                  , nodesMap: SigmaxT.nodesGraphMap props.graph
                  , searchType: SearchDoc
                  , selectedNodeIds: selectedNodeIds'
                  , session: props.session
                  } []
          ]
        ]


sideTabCommunity :: R2.Component Props
sideTabCommunity = R.createElement sideTabCommunityCpt
sideTabCommunityCpt :: R.Component Props
sideTabCommunityCpt = here.component "sideTabCommunity" cpt
  where
    cpt props@{ boxes: { sidePanelGraph }
              , frontends } _ = do
      { selectedNodeIds } <- GEST.focusedSidePanel sidePanelGraph
      selectedNodeIds' <- T.useLive T.unequal selectedNodeIds

      pure $ RH.div { className: "col-md-12", id: "query" }
        [ selectedNodes (Record.merge { nodesMap: SigmaxT.nodesGraphMap props.graph } props) []
        , neighborhood props []
        , query { frontends
                , metaData: props.metaData
                , nodesMap: SigmaxT.nodesGraphMap props.graph
                , searchType: SearchContact
                , selectedNodeIds: selectedNodeIds'
                , session: props.session
                } []
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
    cpt props@{ boxes: { sidePanelGraph }
              , graph
              , nodesMap } _ = do
      { selectedNodeIds } <- GEST.focusedSidePanel sidePanelGraph
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
                  $ ( Seq.map (\node -> badge { minSize: node.size  -- same size for all badges
                                              , maxSize: node.size
                                              , node
                                              , selectedNodeIds })
                      (badges graph selectedNodeIds')
                    )
--                  $ ( Seq.map (\node -> badge { maxSize, minSize, node, selectedNodeIds }) badges')
                )
              , H.br {}
              ]
            ]
          , RH.div { className: "tab-content flex-space-between" }
            [ updateTermButton (Record.merge { buttonType: "primary"
                                             , rType: CandidateTerm
                                             , nodesMap
                                             , text: "Move as candidate" } commonProps) []
            , H.br {}
            , updateTermButton (Record.merge { buttonType: "danger"
                                             , nodesMap
                                             , rType: StopTerm
                                             , text: "Move as stop" } commonProps) []
            ]
          ]
        ]
      where
        commonProps = RX.pick props :: Record Common

data TagCloudState = Folded | Unfolded
derive instance Eq TagCloudState
flipFold :: TagCloudState -> TagCloudState
flipFold Folded = Unfolded
flipFold Unfolded = Folded

neighborhood :: R2.Component Props
neighborhood = R.createElement neighborhoodCpt
neighborhoodCpt :: R.Component Props
neighborhoodCpt = here.component "neighborhood" cpt
  where
    cpt { boxes: { sidePanelGraph }
        , graph
         } _ = do
      { selectedNodeIds } <- GEST.focusedSidePanel sidePanelGraph
      selectedNodeIds' <- T.useLive T.unequal selectedNodeIds
      state <- T.useBox Folded
      state' <- T.useLive T.unequal state

      let numberOfBadgesToShowWhenFolded = 5
          badges' = neighbourBadges graph selectedNodeIds'
          minSize = F.foldl Math.min 0.0 (Seq.map _.size (SigmaxT.graphNodes graph))
          maxSize = F.foldl Math.max 0.0 (Seq.map _.size (SigmaxT.graphNodes graph))
          orderedBadges = A.sortWith (\n -> -n.size) $ Seq.toUnfoldable badges'  -- reverse sort (largest size first)
          displayBadges = case state' of
            Folded -> A.take numberOfBadgesToShowWhenFolded orderedBadges
            Unfolded -> orderedBadges
          stateText = case state' of
            Folded -> "Show more"
            Unfolded -> "Show less"
          showFoldedTooltip = A.length orderedBadges > numberOfBadgesToShowWhenFolded

      pure $ RH.div { className: "tab-content", id: "myTabContent" }
        [ RH.div { -- className: "flex-space-around d-flex justify-content-center"
             className: "d-flex flex-wrap flex-space-around"
             , id: "home"
             , role: "tabpanel"
             }
          ((\node -> badge { maxSize, minSize, node, selectedNodeIds }) <$> displayBadges) <>
          RH.a { on: { click: toggleUnfold state} } [ RH.text stateText ]
        ]
        where
          toggleUnfold state = T.modify_ flipFold state


type UpdateTermButtonProps = (
    buttonType :: String
  , nodesMap   :: SigmaxT.NodesMap
  , rType      :: TermList
  , text       :: String
  | Common
  )

updateTermButton :: R2.Component UpdateTermButtonProps
updateTermButton = R.createElement updateTermButtonCpt
updateTermButtonCpt :: R.Component UpdateTermButtonProps
updateTermButtonCpt = here.component "updateTermButton" cpt
  where
    cpt { boxes: { errors
                 , reloadForest
                 , sidePanelGraph }
        , buttonType
        , graphId
        , metaData
        , nodesMap
        , rType
        , session
        , text } _ = do
      { removedNodeIds, selectedNodeIds } <- GEST.focusedSidePanel sidePanelGraph
      selectedNodeIds' <- T.useLive T.unequal selectedNodeIds

      pure $ if Set.isEmpty selectedNodeIds' then
               RH.div {} []
             else
               RH.button { className: "btn btn-sm btn-" <> buttonType
                         , on: { click: onClickRemove removedNodeIds selectedNodeIds selectedNodeIds' }
                         } [ RH.text text ]
      where
        onClickRemove removedNodeIds selectedNodeIds selectedNodeIds' _ = do
          let nodes = mapMaybe (\id -> Map.lookup id nodesMap)
                              $ Set.toUnfoldable selectedNodeIds'
          sendPatches { errors
                      , graphId: graphId
                      , metaData: metaData
                      , nodes
                      , session: session
                      , termList: rType
                      , reloadForest }
          T.write_ selectedNodeIds' removedNodeIds
          T.write_ SigmaxT.emptyNodeIds selectedNodeIds


type BadgeProps =
  ( maxSize         :: Number
  , minSize         :: Number
  , node            :: Record SigmaxT.Node
  , selectedNodeIds :: T.Box SigmaxT.NodeIds )

badge :: R2.Leaf BadgeProps
badge props = R.createElement badgeCpt props []
badgeCpt :: R.Component BadgeProps
badgeCpt = here.component "badge" cpt where
  cpt { maxSize, minSize, node: { id, label, size }, selectedNodeIds } _ = do
    let minFontSize = 1.0  -- "em"
    let maxFontSize = 3.0  -- "em"
    let sizeScaled = (size - minSize) / (maxSize - minSize)  -- in [0; 1] range
    let scale' = Math.log (sizeScaled + 1.0) / (Math.log 2.0)  -- in [0; 1] range
    let scale = minFontSize + scale' * (maxFontSize - minFontSize)
    let style = {
          fontSize: show scale <> "em"
          }
    
    pure $ RH.a { className: "badge badge-pill badge-light"
                , on: { click: onClick }
                } [ RH.h6 { style } [ RH.text label ] ]
    where
      onClick _ = do
        T.write_ (Set.singleton id) selectedNodeIds

badges :: SigmaxT.SGraph -> SigmaxT.NodeIds -> Seq.Seq (Record SigmaxT.Node)
badges graph selectedNodeIds = SigmaxT.graphNodes $ SigmaxT.nodesById graph selectedNodeIds

neighbourBadges :: SigmaxT.SGraph -> SigmaxT.NodeIds -> Seq.Seq (Record SigmaxT.Node)
neighbourBadges graph selectedNodeIds = SigmaxT.neighbours graph selectedNodes' where
  selectedNodes' = SigmaxT.graphNodes $ SigmaxT.nodesById graph selectedNodeIds

type SendPatches =
  ( errors       :: T.Box (Array FrontendError)
  , graphId      :: NodeID
  , metaData     :: GET.MetaData
  , nodes        :: Array (Record SigmaxT.Node)
  , reloadForest :: T2.ReloadS
  , session      :: Session
  , termList     :: TermList
  )

sendPatches :: Record SendPatches -> Effect Unit
sendPatches { errors, metaData, nodes, reloadForest, session, termList } = do
  launchAff_ do
    patches <- (parTraverse (sendPatch termList session metaData) nodes) -- :: Aff (Array NTC.VersionedNgramsPatches)
    let mPatch = last patches
    case mPatch of
      Nothing -> pure unit
      Just (Left err) -> liftEffect $ do
        T.modify_ (A.cons $ FRESTError { error: err }) errors
        here.log2 "[sendPatches] RESTError" err
      Just (Right (NTC.Versioned _patch)) -> do
        liftEffect $ T2.reload reloadForest

-- Why is this called delete node?
sendPatch :: TermList
          -> Session
          -> GET.MetaData
          -> Record SigmaxT.Node
          -> Aff (Either RESTError NTC.VersionedNgramsPatches)
sendPatch termList session (GET.MetaData metaData) node = do
    eRet  <- NTC.putNgramsPatches coreParams versioned
    case eRet of
      Left err -> pure $ Left err
      Right ret -> do
        _task <- NTC.postNgramsChartsAsync coreParams  -- TODO add task
        pure $ Right ret
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

    np :: NTC.NgramsPatches
    np = NTC.singletonPatchMap term $ NTC.NgramsPatch { patch_children: mempty, patch_list }

    patch_list :: NTC.Replace TermList
    patch_list = NTC.Replace { new: termList, old: MapTerm }

type Query =
  ( frontends       :: Frontends
  , metaData        :: GET.MetaData
  , nodesMap        :: SigmaxT.NodesMap
  , searchType      :: SearchType
  , selectedNodeIds :: SigmaxT.NodeIds
  , session         :: Session )

query :: R2.Component Query
query = R.createElement queryCpt

queryCpt :: R.Component Query
queryCpt = here.component "query" cpt where
  cpt props@{ selectedNodeIds } _ = do

    pure $ if Set.isEmpty selectedNodeIds
           then RH.div {} []
           else query' props []

query' :: R2.Component Query
query' = R.createElement queryCpt'

queryCpt' :: R.Component Query
queryCpt' = here.component "query'" cpt where
  cpt { frontends
      , metaData: GET.MetaData metaData
      , nodesMap
      , searchType
      , selectedNodeIds
      , session } _ = do
    pure $ case (head metaData.corpusId) of
      Nothing -> RH.div {} []
      Just corpusId ->
        CGT.tabs { frontends
                 , query: SearchQuery { expected: searchType
                                      , query : concat $ toQuery <$> Set.toUnfoldable selectedNodeIds
                                      }
                 , session
                 , sides: [side corpusId]
                 }

    where
      toQuery id = case Map.lookup id nodesMap of
        Nothing -> []
        Just n -> words n.label

      side corpusId = GET.GraphSideCorpus { corpusId
                                          , corpusLabel: metaData.title
                                          , listId     : metaData.list.listId
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

