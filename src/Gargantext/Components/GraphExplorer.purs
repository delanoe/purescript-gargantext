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
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Math (log)
import Partial.Unsafe (unsafePartial)
import Reactix as R
import Reactix.DOM.HTML as RH
import Record as Record
import Record.Extra as RX
import Toestand as T

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest (forest)
import Gargantext.Components.Graph as Graph
import Gargantext.Components.GraphExplorer.Controls as Controls
import Gargantext.Components.GraphExplorer.Search (nodeSearchControl)
import Gargantext.Components.GraphExplorer.Sidebar as Sidebar
import Gargantext.Components.GraphExplorer.ToggleButton as Toggle
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Data.Louvain as Louvain
import Gargantext.Ends (Frontends, Backend)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Routes (SessionRoute(NodeAPI), AppRoute)
import Gargantext.Sessions (OpenNodes, Session, Sessions, get)
import Gargantext.Types as Types
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer"

type BaseProps =
  ( backend       :: T.Box (Maybe Backend)
  , frontends     :: Frontends
  , graphId       :: GET.GraphId
  , handed        :: T.Box Types.Handed
  , route         :: T.Box AppRoute
  , sessions      :: T.Box Sessions
  , showLogin     :: T.Box Boolean
  , tasks         :: T.Box (Maybe GAT.Reductor)
  )

type LayoutLoaderProps = ( session :: R.Context Session | BaseProps )

type LayoutProps =
  ( graphVersion :: T2.ReloadS
  , session :: Session
  | BaseProps )

type Props =
  ( graph          :: SigmaxT.SGraph
  , hyperdataGraph :: GET.HyperdataGraph
  , mMetaData      :: Maybe GET.MetaData
  | LayoutProps
  )

--------------------------------------------------------------
explorerLayoutLoader :: R2.Component LayoutLoaderProps
explorerLayoutLoader = R.createElement explorerLayoutLoaderCpt

explorerLayoutLoaderCpt :: R.Component LayoutLoaderProps
explorerLayoutLoaderCpt = here.component "explorerLayoutLoader" cpt where
  cpt props _ = do
    graphVersion <- T.useBox T2.newReload
    session <- R.useContext props.session -- todo: ugh, props fiddling
    let base = RX.pick props :: Record BaseProps
    let props' = Record.merge base { graphVersion, session }
    pure $ explorerLayout props' []
    
explorerLayout :: R2.Component LayoutProps
explorerLayout = R.createElement explorerLayoutCpt

explorerLayoutCpt :: R.Component LayoutProps
explorerLayoutCpt = here.component "explorerLayout" cpt where
  cpt props@{ backend, graphId, graphVersion, session } _ = do
    graphVersion' <- T.useLive T.unequal graphVersion

    useLoader graphId (getNodes session graphVersion') handler
    where
      handler loaded = explorer (Record.merge props { graph, hyperdataGraph: loaded, mMetaData }) []
        -- explorer (Record.merge props { graph, graphVersion, hyperdataGraph: loaded, mMetaData })
        where
          GET.HyperdataGraph { graph: hyperdataGraph } = loaded
          Tuple mMetaData graph = convert hyperdataGraph

--------------------------------------------------------------
explorer :: R2.Component Props
explorer = R.createElement explorerCpt

explorerCpt :: R.Component Props
explorerCpt = here.component "explorer" cpt
  where
    cpt props@{ backend
              , frontends
              , graph
              , graphId
              , graphVersion
              , handed
              , hyperdataGraph
              , mMetaData
              , route
              , session
              , sessions
              , showLogin
              , tasks
              } _ = do
      handed' <- T.useLive T.unequal handed
      graphVersion' <- T.useLive T.unequal graphVersion
      graphVersionRef <- R.useRef graphVersion'

      let startForceAtlas = maybe true (\(GET.MetaData { startForceAtlas: sfa }) -> sfa) mMetaData

      let forceAtlasS = if startForceAtlas
                          then SigmaxT.InitialRunning
                          else SigmaxT.InitialStopped

      dataRef <- R.useRef graph
      graphRef <- R.useRef null
      reloadForest <- T.useBox T2.newReload
      controls <- Controls.useGraphControls { forceAtlasS
                                            , graph
                                            , graphId
                                            , hyperdataGraph
                                            , reloadForest: \_ -> T2.reload reloadForest
                                            , session
                                            }
      multiSelectEnabled' <- T.useLive T.unequal controls.multiSelectEnabled
      showTree' <- T.useLive T.unequal controls.showTree
      multiSelectEnabledRef <- R.useRef multiSelectEnabled'

      forestOpen <- T.useBox $ Set.empty
      R.useEffectOnce' $ do
        R2.loadLocalStorageState R2.openNodesKey forestOpen
        T.listen (R2.listenLocalStorageState R2.openNodesKey) forestOpen

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
          R.setRef graphVersionRef graphVersion'
          -- Reinitialize bunch of state as well.
          T.write_ SigmaxT.emptyNodeIds controls.removedNodeIds
          T.write_ SigmaxT.emptyNodeIds controls.selectedNodeIds
          T.write_ SigmaxT.EShow controls.showEdges
          T.write_ forceAtlasS controls.forceAtlasState
          T.write_ Graph.Init controls.graphStage
          T.write_ GET.InitialClosed controls.showSidePanel

      pure $
        RH.div { className: "graph-meta-container" } [
          RH.div { className: "fixed-top navbar navbar-expand-lg"
                 , id: "graph-explorer" }
            [ rowToggle
                    [ col [ spaces [ Toggle.treeToggleButton { state: controls.showTree } [] ]]
                    , col [ spaces [ Toggle.controlsToggleButton { state: controls.showControls } [] ]]
                    , col [ spaces [ Toggle.sidebarToggleButton { state: controls.showSidePanel } [] ]]
                    , col [ spaces [ nodeSearchControl { graph
                                                       , multiSelectEnabled: controls.multiSelectEnabled
                                                       , selectedNodeIds: controls.selectedNodeIds } [] ] ]
                    ]
            ]
        , RH.div { className: "graph-container" } [
            inner handed' [
              rowControls [ Controls.controls controls ]
            , RH.div { className: "row graph-row" } $ mainLayout handed' $
                tree { backend
                     , forestOpen
                     , frontends
                     , handed
                     , reload: reloadForest
                     , route
                     , reloadForest
                     , sessions
                     , show: showTree'
                     , showLogin: showLogin
                     , tasks
                     }
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
                                   , reloadForest
                                   }
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
    -- rowToggle  = RH.div { id: "toggle-container" }
    rowToggle  = RH.ul { className: "navbar-nav ml-auto mr-auto" }
    rowControls = RH.div { id: "controls-container" }
    -- col       = RH.div { className: "col-md-4" }
    col = RH.li { className: "nav-item" }
    pullLeft  = RH.div { className: "pull-left"  }
    pullRight = RH.div { className: "pull-right" }
    -- spaces    = RH.div { className: "flex-space-between" }
    spaces = RH.a { className: "nav-link" }


    tree :: Record TreeProps -> R.Element
    tree { show: false } = RH.div { id: "tree" } []
    tree { backend, forestOpen, frontends, handed, reload, route, sessions, showLogin, reloadForest, tasks } =
      RH.div {className: "col-md-2 graph-tree"} [
        forest { backend
               , forestOpen
               , frontends
               , handed
               , reloadForest
               , reloadRoot: reload
               , route
               , sessions
               , showLogin
               , tasks } []
      ]

    mSidebar :: Maybe GET.MetaData
             -> Record MSidebarProps
             -> R.Element
    mSidebar  Nothing            _ = RH.div {} []
    mSidebar (Just metaData) props =
      Sidebar.sidebar (Record.merge props { metaData })

type TreeProps = (
    backend      :: T.Box (Maybe Backend)
  , forestOpen   :: T.Box OpenNodes
  , frontends    :: Frontends
  , handed       :: T.Box Types.Handed
  , reload       :: T.Box T2.Reload
  , reloadForest :: T.Box T2.Reload
  , route        :: T.Box AppRoute
  , sessions     :: T.Box Sessions
  , show         :: Boolean
  , showLogin    :: T.Box Boolean
  , tasks        :: T.Box (Maybe GAT.Reductor)
  )

type MSidebarProps =
  ( frontends       :: Frontends
  , graph           :: SigmaxT.SGraph
  , graphId         :: GET.GraphId
  , graphVersion    :: T2.ReloadS
  , reloadForest    :: T.Box T2.Reload
  , removedNodeIds  :: T.Box SigmaxT.NodeIds
  , selectedNodeIds :: T.Box SigmaxT.NodeIds
  , session         :: Session
  , showSidePanel   :: T.Box GET.SidePanelState
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
      let startForceAtlas = maybe true (\(GET.MetaData { startForceAtlas: sfa }) -> sfa) mMetaData

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
