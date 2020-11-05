module Gargantext.Components.GraphExplorer where

import Gargantext.Prelude hiding (max,min)

import DOM.Simple.Types (Element)
import Data.Array as A
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Nullable (null, Nullable)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple (fst, snd, Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Math (log)
import Partial.Unsafe (unsafePartial)
import Reactix as R
import Reactix.DOM.HTML as RH
import Record as Record

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest (forest)
import Gargantext.Components.Graph as Graph
import Gargantext.Components.GraphExplorer.Controls as Controls
import Gargantext.Components.GraphExplorer.Sidebar as Sidebar
import Gargantext.Components.GraphExplorer.ToggleButton as Toggle
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Data.Louvain as Louvain
import Gargantext.Ends (Frontends, Backend)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Routes (SessionRoute(NodeAPI), AppRoute)
import Gargantext.Sessions (Session, Sessions, get)
import Gargantext.Types as Types
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2

thisModule :: String
thisModule = "Gargantext.Components.GraphExplorer"

type LayoutProps = (
    asyncTasksRef :: R.Ref (Maybe GAT.Reductor)
  , backend       :: R.State (Maybe Backend)
  , frontends     :: Frontends
  , graphId       :: GET.GraphId
  , handed        :: Types.Handed
  , mCurrentRoute :: AppRoute
  , session       :: Session
  , sessions      :: Sessions
  , showLogin     :: R.State Boolean
  )

type Props =
  ( graph          :: SigmaxT.SGraph
  , graphVersion   :: R.State Int
  , hyperdataGraph :: GET.HyperdataGraph
  , mMetaData      :: Maybe GET.MetaData
  | LayoutProps
  )

--------------------------------------------------------------
explorerLayout :: Record LayoutProps -> R.Element
explorerLayout props = R.createElement explorerLayoutCpt props []

explorerLayoutCpt :: R.Component LayoutProps
explorerLayoutCpt = R.hooksComponentWithModule thisModule "explorerLayout" cpt
  where
    cpt props _ = do
      graphVersion <- R.useState' 0
      pure $ explorerLayoutView graphVersion props

explorerLayoutView :: R.State Int -> Record LayoutProps -> R.Element
explorerLayoutView graphVersion p = R.createElement el p []
  where
    el = R.hooksComponentWithModule thisModule "explorerLayoutView" cpt
    cpt props@{ graphId, session } _ = do
      useLoader graphId (getNodes session graphVersion) handler
      where
        handler loaded =
          explorer (Record.merge props { graph, graphVersion, hyperdataGraph: loaded, mMetaData })
          where
            GET.HyperdataGraph { graph: hyperdataGraph } = loaded
            Tuple mMetaData graph = convert hyperdataGraph

--------------------------------------------------------------
explorer :: Record Props -> R.Element
explorer props = R.createElement explorerCpt props []

explorerCpt :: R.Component Props
explorerCpt = R.hooksComponentWithModule thisModule "explorer" cpt
  where
    cpt props@{ asyncTasksRef
              , frontends
              , graph
              , graphId
              , graphVersion
              , handed
              , hyperdataGraph
              , mCurrentRoute
              , mMetaData
              , session
              , sessions
              , showLogin
              , backend
              } _ = do

      let startForceAtlas = maybe true (\(GET.MetaData { startForceAtlas }) -> startForceAtlas) mMetaData

      let forceAtlasS = if startForceAtlas
                          then SigmaxT.InitialRunning
                          else SigmaxT.InitialStopped

      dataRef <- R.useRef graph
      graphRef <- R.useRef null
      graphVersionRef       <- R.useRef (fst graphVersion)
      treeReload <- R.useState' 0
      controls   <- Controls.useGraphControls { forceAtlasS
                                              , graph
                                              , graphId
                                              , hyperdataGraph
                                              , session
                                              , treeReload: \_ -> (snd treeReload) $ (+) 1
                                              }
      multiSelectEnabledRef <- R.useRef $ fst controls.multiSelectEnabled

      R.useEffect' $ do
        let readData = R.readRef dataRef
        let gv = R.readRef graphVersionRef
        if SigmaxT.eqGraph readData graph then
          pure unit
        else do
          -- Graph data changed, reinitialize sigma.
          let rSigma = R.readRef controls.sigmaRef
          Sigmax.cleanupSigma rSigma "explorerCpt"
          R.setRef dataRef graph
          R.setRef graphVersionRef (fst graphVersion)
          -- Reinitialize bunch of state as well.
          snd controls.removedNodeIds  $ const SigmaxT.emptyNodeIds
          snd controls.selectedNodeIds $ const SigmaxT.emptyNodeIds
          snd controls.showEdges       $ const SigmaxT.EShow
          snd controls.forceAtlasState $ const forceAtlasS
          snd controls.graphStage      $ const Graph.Init
          snd controls.showSidePanel   $ const GET.InitialClosed

      pure $
        RH.div
          { id: "graph-explorer" }
          [ rowToggle
                  [ col [ spaces [ Toggle.treeToggleButton controls.showTree         ]]
                  , col [ spaces [ Toggle.controlsToggleButton controls.showControls ]]
                  , col [ spaces [ Toggle.sidebarToggleButton controls.showSidePanel ]]
                  ], R2.row
            [ outer
              [ inner handed
                [ rowControls [ Controls.controls controls ]
                , R2.row $ mainLayout handed $
                    tree { asyncTasksRef
                         , backend
                         , frontends
                         , handed
                         , mCurrentRoute
                         , reload: treeReload
                         , sessions
                         , show: fst controls.showTree
                         , showLogin: snd showLogin }
                    /\
                    RH.div { ref: graphRef, id: "graph-view", className: "col-md-12" } []
                    /\
                    graphView { controls
                              , elRef: graphRef
                              , graphId
                              , graph
                              , hyperdataGraph
                              , mMetaData
                              , multiSelectEnabledRef
                              }
                    /\
                    mSidebar mMetaData { frontends
                                        , graph
                                        , graphId
                                        , graphVersion
                                        , removedNodeIds : controls.removedNodeIds
                                        , session
                                        , selectedNodeIds: controls.selectedNodeIds
                                        , showSidePanel  :   controls.showSidePanel
                                        , treeReload
                                        }
                ]
              ]
            ]
          ]

    mainLayout Types.RightHanded (tree' /\ gc /\ gv /\ sdb) = [tree', gc, gv, sdb]
    mainLayout Types.LeftHanded  (tree' /\ gc /\ gv /\ sdb) = [sdb, gc, gv, tree']

    outer = RH.div { className: "col-md-12" }
    inner h = RH.div { className: "container-fluid " <> hClass }
      where
        hClass = case h of
          Types.LeftHanded  -> "lefthanded"
          Types.RightHanded -> "righthanded"
    rowToggle  = RH.div { id: "toggle-container" }
    rowControls = RH.div { id: "controls-container" }
    col       = RH.div { className: "col-md-4" }
    pullLeft  = RH.div { className: "pull-left"  }
    pullRight = RH.div { className: "pull-right" }
    spaces    = RH.div { className: "flex-space-between" }


    tree :: Record TreeProps -> R.Element
    tree { show: false } = RH.div { id: "tree" } []
    tree { asyncTasksRef, backend, frontends, handed, mCurrentRoute: route, reload, sessions, showLogin } =
      RH.div {className: "col-md-2 graph-tree"} [
        forest { asyncTasksRef, backend, frontends, handed, reload, route, sessions, showLogin }
      ]

    mSidebar :: Maybe GET.MetaData
             -> Record MSidebarProps
             -> R.Element
    mSidebar  Nothing            _ = RH.div {} []
    mSidebar (Just metaData) props =
      Sidebar.sidebar (Record.merge props { metaData })

type TreeProps =
  (
    asyncTasksRef :: R.Ref (Maybe GAT.Reductor)
  , backend       :: R.State (Maybe Backend)
  , frontends     :: Frontends
  , handed        :: Types.Handed
  , mCurrentRoute :: AppRoute
  , reload        :: R.State Int
  , sessions      :: Sessions
  , show          :: Boolean
  , showLogin     :: R.Setter Boolean
  )

type MSidebarProps =
  ( frontends       :: Frontends
  , graph           :: SigmaxT.SGraph
  , graphId         :: GET.GraphId
  , graphVersion    :: R.State Int
  , removedNodeIds  :: R.State SigmaxT.NodeIds
  , showSidePanel   :: R.State GET.SidePanelState
  , selectedNodeIds :: R.State SigmaxT.NodeIds
  , session         :: Session
  , treeReload      :: R.State Int
  )

type GraphProps = (
    controls              :: Record Controls.Controls
  , elRef                 :: R.Ref (Nullable Element)
  , graphId               :: GET.GraphId
  , graph                 :: SigmaxT.SGraph
  , hyperdataGraph        :: GET.HyperdataGraph
  , mMetaData             :: Maybe GET.MetaData
  , multiSelectEnabledRef :: R.Ref Boolean
)

graphView :: Record GraphProps -> R.Element
--graphView sigmaRef props = R.createElement (R.memo el memoCmp) props []
graphView props = R.createElement graphViewCpt props []

graphViewCpt :: R.Component GraphProps
graphViewCpt = R.hooksComponentWithModule thisModule "graphView" cpt
  where
    cpt { controls
        , elRef
        , graphId
        , graph
        , hyperdataGraph: GET.HyperdataGraph { mCamera }
        , mMetaData
        , multiSelectEnabledRef } _children = do
      -- TODO Cache this?
      let louvainGraph =
            if (fst controls.showLouvain) then
              let louvain = Louvain.louvain unit in
              let cluster = Louvain.init louvain (SigmaxT.louvainNodes graph) (SigmaxT.louvainEdges graph) in
              SigmaxT.louvainGraph graph cluster
            else
              graph
      let transformedGraph = transformGraph controls louvainGraph
      let startForceAtlas = maybe true (\(GET.MetaData { startForceAtlas }) -> startForceAtlas) mMetaData

      R.useEffect1' (fst controls.multiSelectEnabled) $ do
        R.setRef multiSelectEnabledRef $ fst controls.multiSelectEnabled

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
                         }

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
        , size  : log (toNumber n.size + 1.0)
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


getNodes :: Session -> R.State Int -> GET.GraphId -> Aff GET.HyperdataGraph
getNodes session (graphVersion /\ _) graphId =
  get session $ NodeAPI Types.Graph
                        (Just graphId)
                        ("?version=" <> show graphVersion)


transformGraph :: Record Controls.Controls -> SigmaxT.SGraph -> SigmaxT.SGraph
transformGraph controls graph = SigmaxT.Graph {nodes: newNodes, edges: newEdges}
  where
    edges = SigmaxT.graphEdges graph
    nodes = SigmaxT.graphNodes graph
    selectedEdgeIds =
      Set.fromFoldable
        $ Seq.map _.id
        $ SigmaxT.neighbouringEdges graph (fst controls.selectedNodeIds)
    hasSelection = not $ Set.isEmpty (fst controls.selectedNodeIds)

    newEdges' = Seq.filter edgeFilter $ Seq.map (
      edgeHideWeight <<< edgeHideConfluence <<< edgeShowFilter <<< edgeMarked
      ) edges
    newNodes  = Seq.filter nodeFilter $ Seq.map (nodeMarked <<< nodeHideSize) nodes
    newEdges  = Seq.filter (edgeInGraph $ Set.fromFoldable $ Seq.map _.id newNodes) newEdges'

    edgeFilter e = true
    nodeFilter n = nodeRemovedFilter n

    nodeSizeFilter :: Record SigmaxT.Node -> Boolean
    nodeSizeFilter node@{ size } = Range.within (fst controls.nodeSize) size

    nodeRemovedFilter node@{ id } = not $ Set.member id $ fst controls.removedNodeIds

    edgeConfluenceFilter :: Record SigmaxT.Edge -> Boolean
    edgeConfluenceFilter edge@{ confluence } = Range.within (fst controls.edgeConfluence) confluence
    edgeWeightFilter :: Record SigmaxT.Edge -> Boolean
    edgeWeightFilter edge@{ weightIdx } = Range.within (fst controls.edgeWeight) $ toNumber weightIdx

    edgeHideConfluence :: Record SigmaxT.Edge -> Record SigmaxT.Edge
    edgeHideConfluence edge@{ confluence } =
      if Range.within (fst controls.edgeConfluence) confluence then
        edge
      else
        edge { hidden = true }

    edgeHideWeight :: Record SigmaxT.Edge -> Record SigmaxT.Edge
    edgeHideWeight edge@{ weightIdx } =
      if Range.within (fst controls.edgeWeight) $ toNumber weightIdx then
        edge
      else
        edge { hidden = true }

    edgeShowFilter :: Record SigmaxT.Edge -> Record SigmaxT.Edge
    edgeShowFilter edge =
      if (SigmaxT.edgeStateHidden $ fst controls.showEdges) then
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
      if Set.member id (fst controls.selectedNodeIds) then
        node { borderColor = "#000", type = "selected" }
      else
        node

    nodeHideSize :: Record SigmaxT.Node -> Record SigmaxT.Node
    nodeHideSize node@{ size } =
      if Range.within (fst controls.nodeSize) size then
        node
      else
        node { hidden = true }
