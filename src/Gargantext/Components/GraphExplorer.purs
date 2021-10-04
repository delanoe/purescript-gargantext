module Gargantext.Components.GraphExplorer where

import Gargantext.Prelude hiding (max, min)

import Data.Array as A
import Data.Either (Either)
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

import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Graph as Graph
import Gargantext.Components.GraphExplorer.Controls as Controls
import Gargantext.Components.GraphExplorer.Sidebar.Types as GEST
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Config.REST (RESTError)
import Gargantext.Data.Louvain as Louvain
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, get)
import Gargantext.Types as Types
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer"

type BaseProps =
  ( boxes          :: Boxes
  , graphId        :: GET.GraphId
  )

type LayoutProps =
  ( session      :: Session
  | BaseProps )

type Props =
  ( graph          :: SigmaxT.SGraph
  , hyperdataGraph :: GET.HyperdataGraph
  | LayoutProps
  )

type GraphWriteProps =
  ( mMetaData'     :: Maybe GET.MetaData
  | Props
  )

--------------------------------------------------------------
explorerLayout :: R2.Component LayoutProps
explorerLayout = R.createElement explorerLayoutCpt
explorerLayoutCpt :: R.Component LayoutProps
explorerLayoutCpt = here.component "explorerLayout" cpt where
  cpt props@{ boxes: { graphVersion }, graphId, session } _ = do
    graphVersion' <- T.useLive T.unequal graphVersion

    useLoader { errorHandler
              , loader: getNodes session graphVersion'
              , path: graphId
              , render: handler }
    where
      errorHandler err = here.log2 "[explorerLayout] RESTError" err
      handler loaded@(GET.HyperdataGraph { graph: hyperdataGraph }) =
        explorerWriteGraph (Record.merge props { graph, hyperdataGraph: loaded, mMetaData' }) []
        where
          Tuple mMetaData' graph = convert hyperdataGraph

explorerWriteGraph :: R2.Component GraphWriteProps
explorerWriteGraph = R.createElement explorerWriteGraphCpt
explorerWriteGraphCpt :: R.Component GraphWriteProps
explorerWriteGraphCpt = here.component "explorerWriteGraph" cpt where
  cpt props@{ boxes: { sidePanelGraph }
            , graph
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
    cpt props@{ boxes: { graphVersion, handed, reloadForest, showTree, sidePanelGraph, sidePanelState }
        , graph
        , graphId
        , hyperdataGraph
        , session
        } _ = do
      { mMetaData } <- GEST.focusedSidePanel sidePanelGraph
      _graphVersion' <- T.useLive T.unequal graphVersion
      handed' <- T.useLive T.unequal handed
      mMetaData' <- T.useLive T.unequal mMetaData

      let startForceAtlas = maybe true (\(GET.MetaData { startForceAtlas: sfa }) -> sfa) mMetaData'

      let forceAtlasS = if startForceAtlas
                          then SigmaxT.InitialRunning
                          else SigmaxT.InitialStopped

      _dataRef <- R.useRef graph
      graphRef <- R.useRef null
      controls <- Controls.useGraphControls { forceAtlasS
                                            , graph
                                            , graphId
                                            , hyperdataGraph
                                            , reloadForest
                                            , session
                                            , showTree
                                            , sidePanel: sidePanelGraph
                                            , sidePanelState }

      -- graphVersionRef <- R.useRef graphVersion'
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
          [ RH.div { className: "container-fluid " <> hClass handed' }
            [ RH.div { id: "controls-container" } [ Controls.controls controls [] ]
            , RH.div { className: "row graph-row" }
              [ RH.div { ref: graphRef, id: "graph-view", className: "col-md-12" } []
              , graphView { boxes: props.boxes
                          , controls
                          , elRef: graphRef
                          , graph
                          , hyperdataGraph
                          , mMetaData
                          } []
              ]
            ]
          ]
        ]

    hClass h = case h of
      Types.LeftHanded  -> "lefthanded"
      Types.RightHanded -> "righthanded"

type GraphProps =
  ( boxes          :: Boxes
  , controls       :: Record Controls.Controls
  , elRef          :: R.Ref (Nullable Element)
  , graph          :: SigmaxT.SGraph
  , hyperdataGraph :: GET.HyperdataGraph
  , mMetaData      :: T.Box (Maybe GET.MetaData)
)

graphView :: R2.Component GraphProps
graphView = R.createElement graphViewCpt
graphViewCpt :: R.Component GraphProps
graphViewCpt = here.component "graphView" cpt
  where
    cpt { boxes
        , controls
        , elRef
        , graph
        , hyperdataGraph: GET.HyperdataGraph { mCamera }
        , mMetaData } _children = do
      edgeConfluence' <- T.useLive T.unequal controls.edgeConfluence
      edgeWeight' <- T.useLive T.unequal controls.edgeWeight
      mMetaData' <- T.useLive T.unequal mMetaData
      multiSelectEnabled' <- T.useLive T.unequal controls.multiSelectEnabled
      nodeSize' <- T.useLive T.unequal controls.nodeSize
      removedNodeIds' <- T.useLive T.unequal controls.removedNodeIds
      selectedNodeIds' <- T.useLive T.unequal controls.selectedNodeIds
      showEdges' <- T.useLive T.unequal controls.showEdges
      showLouvain' <- T.useLive T.unequal controls.showLouvain

      multiSelectEnabledRef <- R.useRef multiSelectEnabled'

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

      pure $ Graph.graph { boxes
                         , elRef
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


getNodes :: Session -> T2.Reload -> GET.GraphId -> Aff (Either RESTError GET.HyperdataGraph)
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
                     , selectedNodeIds' } = SigmaxT.Graph {nodes: newNodes, edges: newEdges}
  where
    edges = SigmaxT.graphEdges graph
    nodes = SigmaxT.graphNodes graph
    selectedEdgeIds =
      Set.fromFoldable
        $ Seq.map _.id
        $ SigmaxT.neighbouringEdges graph selectedNodeIds'
    hasSelection = not $ Set.isEmpty selectedNodeIds'

    newEdges' = Seq.filter edgeFilter $ Seq.map (
      -- NOTE We don't use edgeShowFilter anymore because of
      -- https://gitlab.iscpif.fr/gargantext/purescript-gargantext/issues/304
      -- edgeHideWeight <<< edgeHideConfluence <<< edgeShowFilter <<< edgeMarked
      edgeHideWeight <<< edgeHideConfluence <<< edgeMarked
      ) edges
    newNodes  = Seq.filter nodeFilter $ Seq.map (nodeMarked <<< nodeHideSize) nodes
    newEdges  = Seq.filter (edgeInGraph $ Set.fromFoldable $ Seq.map _.id newNodes) newEdges'

    edgeFilter _e = true
    nodeFilter n = nodeRemovedFilter n

    nodeRemovedFilter { id } = not $ Set.member id removedNodeIds'

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
