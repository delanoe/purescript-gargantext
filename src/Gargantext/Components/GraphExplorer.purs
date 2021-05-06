module Gargantext.Components.GraphExplorer where

import Gargantext.Prelude hiding (max,min)

import Data.Array as A
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Nullable (null, Nullable)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple (Tuple(..))
import DOM.Simple.Types (Element)
import Effect.Aff (Aff)
import Math as Math
import Partial.Unsafe (unsafePartial)
import Reactix as R
import Reactix.DOM.HTML as RH
import Record as Record
import Record.Extra as RX
import Toestand as T

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Graph as Graph
import Gargantext.Components.GraphExplorer.Controls as Controls
import Gargantext.Components.GraphExplorer.Search (nodeSearchControl)
import Gargantext.Components.GraphExplorer.Sidebar.Types as GEST
import Gargantext.Components.GraphExplorer.ToggleButton as Toggle
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Data.Louvain as Louvain
import Gargantext.Ends (Frontends, Backend)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Routes (SessionRoute(NodeAPI), AppRoute)
import Gargantext.Sessions (Session, Sessions, get)
import Gargantext.Types as Types
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer"

type BaseProps =
  ( backend        :: T.Box (Maybe Backend)
  , boxes          :: Boxes
  , frontends      :: Frontends
  , graphId        :: GET.GraphId
  , handed         :: T.Box Types.Handed
  , route          :: T.Box AppRoute
  , sessions       :: T.Box Sessions
  , showLogin      :: T.Box Boolean
  , tasks          :: T.Box GAT.Storage
  )

type LayoutProps =
  ( session      :: Session
  | BaseProps )

type GraphWriteProps =
  ( graph          :: SigmaxT.SGraph
  , hyperdataGraph :: GET.HyperdataGraph
  , mMetaData'     :: Maybe GET.MetaData
  | LayoutProps
  )

type Props =
  ( graph          :: SigmaxT.SGraph
  , hyperdataGraph :: GET.HyperdataGraph
  | LayoutProps
  )

--------------------------------------------------------------
explorerLayout :: R2.Component LayoutProps
explorerLayout = R.createElement explorerLayoutCpt
explorerLayoutCpt :: R.Component LayoutProps
explorerLayoutCpt = here.component "explorerLayout" cpt where
  cpt props@{ backend, boxes: { graphVersion }, graphId, session } _ = do
    graphVersion' <- T.useLive T.unequal graphVersion

    useLoader graphId (getNodes session graphVersion') handler
    where
      handler loaded@(GET.HyperdataGraph { graph: hyperdataGraph }) =
        explorerWriteGraph (Record.merge props { graph, hyperdataGraph: loaded, mMetaData' }) []
        where
          Tuple mMetaData' graph = convert hyperdataGraph

explorerWriteGraph :: R2.Component GraphWriteProps
explorerWriteGraph = R.createElement explorerWriteGraphCpt
explorerWriteGraphCpt :: R.Component GraphWriteProps
explorerWriteGraphCpt = here.component "explorerWriteGraph" cpt where
  cpt props@{ boxes: { sidePanelGraph, sidePanelState }
            , graph
            , hyperdataGraph
            , mMetaData' } _ = do
      R.useEffectOnce' $ do
        T.write_ (Just { mGraph: Just graph
                       , mMetaData: mMetaData'
                       , multiSelectEnabled: false
                       , removedNodeIds: Set.empty
                       , selectedNodeIds: Set.empty
                       , showControls: false
                       , sideTab: GET.SideTabLegend }) sidePanelGraph

      pure $ explorer (RX.pick props :: Record Props) []

--------------------------------------------------------------
explorer :: R2.Component Props
explorer = R.createElement explorerCpt
explorerCpt :: R.Component Props
explorerCpt = here.component "explorer" cpt
  where
    cpt props@{ backend
              , boxes: boxes@{ graphVersion, reloadForest, showTree, sidePanelGraph, sidePanelState }
              , frontends
              , graph
              , graphId
              , handed
              , hyperdataGraph
              , route
              , session
              , sessions
              , showLogin
              , tasks
              } _ = do
      { mMetaData, sideTab } <- GEST.focusedSidePanel sidePanelGraph
      handed' <- T.useLive T.unequal handed
      graphVersion' <- T.useLive T.unequal graphVersion
      graphVersionRef <- R.useRef graphVersion'
      mMetaData' <- T.useLive T.unequal mMetaData
      -- sideTab <- T.useBox GET.SideTabLegend

      let startForceAtlas = maybe true (\(GET.MetaData { startForceAtlas: sfa }) -> sfa) mMetaData'

      let forceAtlasS = if startForceAtlas
                          then SigmaxT.InitialRunning
                          else SigmaxT.InitialStopped

      dataRef <- R.useRef graph
      graphRef <- R.useRef null
      controls <- Controls.useGraphControls { forceAtlasS
                                            , graph
                                            , graphId
                                            , hyperdataGraph
                                            , reloadForest
                                            , session
                                            , showTree
                                            , sidePanel: sidePanelGraph
                                            , sidePanelState
                                            }
      multiSelectEnabled' <- T.useLive T.unequal controls.multiSelectEnabled
      multiSelectEnabledRef <- R.useRef multiSelectEnabled'

      -- R.useEffect' $ do
      --   let readData = R.readRef dataRef
      --   let gv = R.readRef graphVersionRef
      --   if SigmaxT.eqGraph readData graph then
      --     pure unit
      --   else do
      --     -- Graph data changed, reinitialize sigma.
      --     let rSigma = R.readRef controls.sigmaRef
      --     Sigmax.cleanupSigma rSigma "explorerCpt"
      --     R.setRef dataRef graph
      --     R.setRef graphVersionRef graphVersion'
      --     -- Reinitialize bunch of state as well.
      --     T.write_ SigmaxT.emptyNodeIds controls.removedNodeIds
      --     T.write_ SigmaxT.emptyNodeIds controls.selectedNodeIds
      --     T.write_ SigmaxT.EShow controls.showEdges
      --     T.write_ forceAtlasS controls.forceAtlasState
      --     T.write_ Graph.Init controls.graphStage
      --     T.write_ Types.InitialClosed controls.sidePanelState

      pure $
        RH.div { className: "graph-meta-container" }
        [ RH.div { className: "graph-container" }
          [ inner handed'
            [ RH.div { id: "controls-container" } [ Controls.controls controls [] ]
            , RH.div { className: "row graph-row" }
              [ RH.div { ref: graphRef, id: "graph-view", className: "col-md-12" } []
              , graphView { controls
                          , elRef: graphRef
                          , graphId
                          , graph
                          , hyperdataGraph
                          , mMetaData
                          , multiSelectEnabledRef
                          } []
              ]
            ]
          ]
        ]

    inner h = RH.div { className: "container-fluid " <> hClass }
      where
        hClass = case h of
          Types.LeftHanded  -> "lefthanded"
          Types.RightHanded -> "righthanded"

type TopBar =
  (
    boxes    :: Boxes
  )

topBar :: R2.Leaf TopBar
topBar p = R.createElement topBarCpt p []
topBarCpt :: R.Component TopBar
topBarCpt = here.component "topBar" cpt where
  cpt { boxes: { showTree
               , sidePanelGraph
               , sidePanelState } } _ = do
    { mGraph, multiSelectEnabled, selectedNodeIds, showControls } <- GEST.focusedSidePanel sidePanelGraph

    mGraph' <- T.useLive T.unequal mGraph

    let search = case mGraph' of
          Just graph -> nodeSearchControl { graph
                                         , multiSelectEnabled
                                         , selectedNodeIds } []
          Nothing -> RH.div {} []

    pure $ RH.form { className: "d-flex" }
      [ Toggle.treeToggleButton { state: showTree } []
      , Toggle.controlsToggleButton { state: showControls } []
      , Toggle.sidebarToggleButton { state: sidePanelState } []
      , search
      -- [ col [ spaces [ Toggle.treeToggleButton { state: showTree } [] ]]
      -- , col [ spaces [ Toggle.controlsToggleButton { state: showControls } [] ]]
      -- , col [ spaces [ Toggle.sidebarToggleButton { state: sidePanelState } [] ]]
      -- , col [ spaces [ search ] ]
      ]
    where
      -- rowToggle  = RH.div { id: "toggle-container" }
      rowToggle  = RH.ul { className: "navbar-nav ml-auto mr-auto" }
      -- col       = RH.div { className: "col-md-4" }
      col = RH.li { className: "nav-item" }
      -- spaces    = RH.div { className: "flex-space-between" }
      spaces = RH.a { className: "nav-link" }

type GraphProps = (
    controls              :: Record Controls.Controls
  , elRef                 :: R.Ref (Nullable Element)
  , graphId               :: GET.GraphId
  , graph                 :: SigmaxT.SGraph
  , hyperdataGraph        :: GET.HyperdataGraph
  , mMetaData             :: T.Box (Maybe GET.MetaData)
  , multiSelectEnabledRef :: R.Ref Boolean
)

graphView :: R2.Component GraphProps
graphView = R.createElement graphViewCpt
graphViewCpt :: R.Component GraphProps
graphViewCpt = here.component "graphView" cpt
  where
    cpt { controls
        , elRef
        , graphId
        , graph
        , hyperdataGraph: GET.HyperdataGraph { mCamera }
        , mMetaData
        , multiSelectEnabledRef } _children = do
      edgeConfluence' <- T.useLive T.unequal controls.edgeConfluence
      edgeWeight' <- T.useLive T.unequal controls.edgeWeight
      mMetaData' <- T.useLive T.unequal mMetaData
      multiSelectEnabled' <- T.useLive T.unequal controls.multiSelectEnabled
      nodeSize' <- T.useLive T.unequal controls.nodeSize
      removedNodeIds' <- T.useLive T.unequal controls.removedNodeIds
      selectedNodeIds' <- T.useLive T.unequal controls.selectedNodeIds
      showEdges' <- T.useLive T.unequal controls.showEdges
      showLouvain' <- T.useLive T.unequal controls.showLouvain

      -- TODO Cache this?
      let louvainGraph =
            if showLouvain' then
              let louvain = Louvain.louvain unit in
              let cluster = Louvain.init louvain (SigmaxT.louvainNodes graph) (SigmaxT.louvainEdges graph) in
              SigmaxT.louvainGraph graph cluster
            else
              graph
      let transformedGraph = transformGraph louvainGraph { edgeConfluence'
                                                         , edgeWeight'
                                                         , nodeSize'
                                                         , removedNodeIds'
                                                         , selectedNodeIds'
                                                         , showEdges' }
      let startForceAtlas = maybe true (\(GET.MetaData { startForceAtlas: sfa }) -> sfa) mMetaData'

      R.useEffect1' multiSelectEnabled' $ do
        R.setRef multiSelectEnabledRef multiSelectEnabled'

      pure $ Graph.graph { elRef
                         , forceAtlas2Settings: Graph.forceAtlas2Settings
                         , graph
                         , mCamera
                         , multiSelectEnabledRef
                         , selectedNodeIds: controls.selectedNodeIds
                         , showEdges: controls.showEdges
                         , sigmaRef: controls.sigmaRef
                         , sigmaSettings: Graph.sigmaSettings
                         , stage: controls.graphStage
                         , startForceAtlas
                         , transformedGraph
                         } []

convert :: GET.GraphData -> Tuple (Maybe GET.MetaData) SigmaxT.SGraph
convert (GET.GraphData r) = Tuple r.metaData $ SigmaxT.Graph {nodes, edges}
  where
    nodes = foldMapWithIndex nodeFn r.nodes
    nodeFn _i nn@(GET.Node n) =
      Seq.singleton {
          borderColor: color
        , color : color
        , equilateral: { numPoints: 3 }
        , gargType
        , hidden : false
        , id    : n.id_
        , label : n.label
        , size  : Math.log (toNumber n.size + 1.0)
        , type  : modeGraphType gargType
        , x     : n.x -- cos (toNumber i)
        , y     : n.y -- sin (toNumber i)
        , _original: nn
        }
      where
        cDef (GET.Cluster {clustDefault}) = clustDefault
        color = GET.intColor (cDef n.attributes)
        gargType =  unsafePartial $ fromJust $ Types.modeFromString n.type_
    nodesMap = SigmaxT.nodesMap nodes
    edges = foldMapWithIndex edgeFn $ A.sortWith (\(GET.Edge {weight}) -> weight) r.edges
    edgeFn i ee@(GET.Edge e) =
      Seq.singleton
        { id : e.id_
        , color
        , confluence : e.confluence
        , hidden : false
        , size: 1.0
        , source : e.source
        , sourceNode
        , target : e.target
        , targetNode
        , weight : e.weight
        , weightIdx: i
        , _original: ee
        }
      where
        sourceNode = unsafePartial $ fromJust $ Map.lookup e.source nodesMap
        targetNode = unsafePartial $ fromJust $ Map.lookup e.target nodesMap
        color = sourceNode.color

-- | See sigmajs/plugins/sigma.renderers.customShapes/shape-library.js
modeGraphType :: Types.Mode -> String
modeGraphType Types.Authors = "square"
modeGraphType Types.Institutes = "equilateral"
modeGraphType Types.Sources = "star"
modeGraphType Types.Terms = "def"


getNodes :: Session -> T2.Reload -> GET.GraphId -> Aff GET.HyperdataGraph
getNodes session graphVersion graphId =
  get session $ NodeAPI Types.Graph
                        (Just graphId)
                        ("?version=" <> (show graphVersion))

type LiveProps = (
    edgeConfluence'  :: Range.NumberRange
  , edgeWeight'      :: Range.NumberRange
  , nodeSize'        :: Range.NumberRange
  , removedNodeIds'  :: SigmaxT.NodeIds
  , selectedNodeIds' :: SigmaxT.NodeIds
  , showEdges'       :: SigmaxT.ShowEdgesState
  )

transformGraph :: SigmaxT.SGraph -> Record LiveProps -> SigmaxT.SGraph
transformGraph graph { edgeConfluence'
                     , edgeWeight'
                     , nodeSize'
                     , removedNodeIds'
                     , selectedNodeIds'
                     , showEdges' } = SigmaxT.Graph {nodes: newNodes, edges: newEdges}
  where
    edges = SigmaxT.graphEdges graph
    nodes = SigmaxT.graphNodes graph
    selectedEdgeIds =
      Set.fromFoldable
        $ Seq.map _.id
        $ SigmaxT.neighbouringEdges graph selectedNodeIds'
    hasSelection = not $ Set.isEmpty selectedNodeIds'

    newEdges' = Seq.filter edgeFilter $ Seq.map (
      edgeHideWeight <<< edgeHideConfluence <<< edgeShowFilter <<< edgeMarked
      ) edges
    newNodes  = Seq.filter nodeFilter $ Seq.map (nodeMarked <<< nodeHideSize) nodes
    newEdges  = Seq.filter (edgeInGraph $ Set.fromFoldable $ Seq.map _.id newNodes) newEdges'

    edgeFilter e = true
    nodeFilter n = nodeRemovedFilter n

    nodeSizeFilter :: Record SigmaxT.Node -> Boolean
    nodeSizeFilter node@{ size } = Range.within nodeSize' size

    nodeRemovedFilter node@{ id } = not $ Set.member id removedNodeIds'

    edgeConfluenceFilter :: Record SigmaxT.Edge -> Boolean
    edgeConfluenceFilter edge@{ confluence } = Range.within edgeConfluence' confluence
    edgeWeightFilter :: Record SigmaxT.Edge -> Boolean
    edgeWeightFilter edge@{ weightIdx } = Range.within edgeWeight' $ toNumber weightIdx

    edgeHideConfluence :: Record SigmaxT.Edge -> Record SigmaxT.Edge
    edgeHideConfluence edge@{ confluence } =
      if Range.within edgeConfluence' confluence then
        edge
      else
        edge { hidden = true }

    edgeHideWeight :: Record SigmaxT.Edge -> Record SigmaxT.Edge
    edgeHideWeight edge@{ weightIdx } =
      if Range.within edgeWeight' $ toNumber weightIdx then
        edge
      else
        edge { hidden = true }

    edgeShowFilter :: Record SigmaxT.Edge -> Record SigmaxT.Edge
    edgeShowFilter edge =
      if SigmaxT.edgeStateHidden showEdges' then
        edge { hidden = true }
      else
        edge

    edgeInGraph :: SigmaxT.NodeIds -> Record SigmaxT.Edge -> Boolean
    edgeInGraph nodeIds e = (Set.member e.source nodeIds) && (Set.member e.target nodeIds)

    edgeMarked :: Record SigmaxT.Edge -> Record SigmaxT.Edge
    edgeMarked edge@{ id, sourceNode } = do
      let isSelected = Set.member id selectedEdgeIds
      case Tuple hasSelection isSelected of
        Tuple false true  -> edge { color = "#ff0000" }
        Tuple true  true  -> edge { color = sourceNode.color }
        Tuple true false  -> edge { color = "rgba(221, 221, 221, 0.5)" }
        _                 -> edge

    nodeMarked :: Record SigmaxT.Node -> Record SigmaxT.Node
    nodeMarked node@{ id } =
      if Set.member id selectedNodeIds' then
        node { borderColor = "#000", type = "selected" }
      else
        node

    nodeHideSize :: Record SigmaxT.Node -> Record SigmaxT.Node
    nodeHideSize node@{ size } =
      if Range.within nodeSize' size then
        node
      else
        node { hidden = true }
