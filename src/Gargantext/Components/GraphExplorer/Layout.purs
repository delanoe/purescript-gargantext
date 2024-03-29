module Gargantext.Components.GraphExplorer.Layout where

import Gargantext.Prelude hiding (max, min)

import DOM.Simple.Types (Element)
import Data.Array as A
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Hashable as Hashable
import Data.HashSet as HashSet
import Data.Int (floor, toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Nullable (null, Nullable)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Gargantext.Components.App.Store as AppStore
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.GraphExplorer.Frame.DocFocus (docFocus)
import Gargantext.Components.GraphExplorer.GraphTypes as GEGT
import Gargantext.Components.GraphExplorer.Resources as Graph
import Gargantext.Components.GraphExplorer.Sidebar as GES
import Gargantext.Components.GraphExplorer.Store as GraphStore
import Gargantext.Components.GraphExplorer.Toolbar.Controls as Controls
import Gargantext.Components.GraphExplorer.TopBar as GETB
import Gargantext.Components.GraphExplorer.Types (GraphSideDoc)
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.GraphExplorer.Utils as GEU
import Gargantext.Config (defaultFrontends)
import Gargantext.Hooks.Session (useSession)
import Gargantext.Hooks.Sigmax.ForceAtlas2 as ForceAtlas
import Gargantext.Hooks.Sigmax.Noverlap as Noverlap
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Types as GT
import Gargantext.Types as Types
import Gargantext.Utils (getter, (?))
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2
import Partial.Unsafe (unsafePartial)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Layout"

type Props =
  ( fa2Ref   :: R.Ref (Maybe ForceAtlas.FA2Layout)
  , noverlapRef :: R.Ref (Maybe Noverlap.NoverlapLayout)
  , sigmaRef :: R.Ref Sigmax.Sigma
  )

layout :: R2.Leaf Props
layout = R2.leaf layoutCpt
layoutCpt :: R.Component Props
layoutCpt = here.component "layout" cpt where
  cpt { fa2Ref
      , noverlapRef
      , sigmaRef
      } _ = do
    -- | States
    -- |
    { reloadForest
    } <- AppStore.use

    { showSidebar
    , showDoc
    , mMetaData
    , showControls
    , graphId
    } <- GraphStore.use

    session <- useSession

    showSidebar'  <- R2.useLive' showSidebar
    showDoc'      <- R2.useLive' showDoc
    mMetaData'    <- R2.useLive' mMetaData
    showControls' <- R2.useLive' showControls
    graphId'      <- R2.useLive' graphId

    -- _dataRef <- R.useRef graph
    graphRef <- R.useRef null

    -- | Hooks
    -- |

    topBarPortalKey <- pure $ "portal-topbar::" <> show graphId'

    mTopBarHost <- R.unsafeHooksEffect $ R2.getElementById "portal-topbar"


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

    -- | Computed
    -- |
    let
      closeDoc :: Unit -> Effect Unit
      closeDoc _ = T.write_ Nothing showDoc

    -- | Render
    -- |

    pure $

      H.div
      { className: "graph-layout" }
      [
        -- Topbar
        R2.createPortal' mTopBarHost
        [
          R2.fragmentWithKey topBarPortalKey
          [
            GETB.topBar
            {}
          ]
        ]
      ,
        -- Sidebar + Focus frame
        H.div
        { className: "graph-layout__frame" }
        [
          -- Doc focus
          R2.fromMaybe showDoc' \(graphSideDoc :: GraphSideDoc) ->

            H.div
            { className: "graph-layout__focus" }
            [
              H.div
              { className: "graph-layout__focus__inner" }
              [
                docFocus
                { session
                , graphSideDoc
                , closeCallback: closeDoc
                , key: show $ getter _.docId graphSideDoc
                }
              ]
            ]
        ,
          -- Sidebar
          H.div
          { className: "graph-layout__sidebar"
          -- @XXX: ReactJS lack of "keep-alive" feature workaround solution
          -- @link https://github.com/facebook/react/issues/12039
          , style: { display: showSidebar' == GT.Opened ? "block" $ "none" }
          }
          [
            H.div
            { className: "graph-layout__sidebar__inner" }
            [
              case mMetaData' of

                Nothing ->
                  B.caveat
                  {}
                  [ H.text "The current node does not contain any meta data" ]

                Just metaData ->
                  GES.sidebar
                  { frontends: defaultFrontends
                  , metaData
                  , session
                  }
            ]
          ]
        ]
      ,
        -- Toolbar
        H.div
        { className: "graph-layout__toolbar"
        -- @XXX: ReactJS lack of "keep-alive" feature workaround solution
        -- @link https://github.com/facebook/react/issues/12039
        , style: { display: showControls' ? "block" $ "none" }
        }
        [
          Controls.controls
          { fa2Ref
          , noverlapRef
          , reloadForest: reloadForest
          , session
          , sigmaRef
          }
        ]
      ,
        -- Content
        H.div
        { ref: graphRef
        , className: "graph-layout__content"
        }
        [
          graphView
          { elRef: graphRef
          , fa2Ref
          , noverlapRef
          , sigmaRef
          }
        ]
      ]

--------------------------------------------------------------

type GraphProps =
  ( elRef    :: R.Ref (Nullable Element)
  , fa2Ref   :: R.Ref (Maybe ForceAtlas.FA2Layout)
  , noverlapRef :: R.Ref (Maybe Noverlap.NoverlapLayout)
  , sigmaRef :: R.Ref Sigmax.Sigma
  )

graphView :: R2.Leaf GraphProps
graphView = R2.leaf graphViewCpt
graphViewCpt :: R.Memo GraphProps
graphViewCpt = R.memo' $ here.component "graphView" cpt where
  cpt { elRef
      , fa2Ref
      , noverlapRef
      , sigmaRef
      } _ = do
    -- | States
    -- |
    { edgeConfluence
    , edgeWeight
    , graph
    , nodeSize
    , removedNodeIds
    , selectedNodeIds
    , showEdges
    , transformedGraph
    } <- GraphStore.use

    -- edgeConfluence'     <- R2.useLive' edgeConfluence
    -- edgeWeight'         <- R2.useLive' edgeWeight
    -- nodeSize'           <- R2.useLive' nodeSize
    -- removedNodeIds'     <- R2.useLive' removedNodeIds
    -- selectedNodeIds'    <- R2.useLive' selectedNodeIds
    -- showEdges'          <- R2.useLive' showEdges
    -- graph'              <- R2.useLive' graph

    -- | Computed
    -- |

    -- let transformParams = { edgeConfluence'
    --                       , edgeWeight'
    --                       , nodeSize'
    --                       , removedNodeIds'
    --                       , selectedNodeIds'
    --                       , showEdges' }
    -- -- let transformedGraph = transformGraph graph' transformParams
    -- transformedGraphS <- T.useBox $ transformGraph graph' transformParams

    -- todo Cache this?
    -- R.useEffect' $ do
    --   --here.log2 "[graphView] transformedGraph" $ transformGraph graph' transformParams

    --   --let louvain = Louvain.louvain unit in
    --   --let cluster = Louvain.init louvain (SigmaxT.louvainNodes graph') (SigmaxT.louvainEdges graph') in
    --   --SigmaxT.louvainGraph graph' cluster
    --   Sigmax.dependOnSigma (R.readRef sigmaRef) "[graphView (louvainGraph)] no sigma" $ \sigma -> do
    --     newGraph <- Louvain.assignVisible (SigmaxS.graph sigma) {}
    --     -- here.log2 "[graphView] newGraph" newGraph
    --     -- here.log2 "[graphView] nodes" $ A.fromFoldable $ Graphology.nodes newGraph
    --     let cluster = Louvain.cluster newGraph :: DLouvain.LouvainCluster
    --     let lgraph = SigmaxT.louvainGraph graph' cluster :: SigmaxT.SGraph
    --     --T.write_ (transformGraph lgraph transformParams) transformedGraphS
    --     -- apply colors
    --     -- traverse_ (\{ id, color } ->
    --     --   Graphology.mergeNodeAttributes (SigmaxS.graph sigma) id { color }
    --     -- ) (SigmaxT.graphNodes lgraph)
    --     T.write_ lgraph transformedGraphS

    -- transformedGraph <- R2.useLive' transformedGraphS

    -- R.useEffect' $ do
    --   let (SigmaxT.Graph { edges: e }) = transformedGraph
    --   here.log2 "[graphView] transformedGraph edges" $ A.fromFoldable e
    --   here.log2 "[graphView] hidden edges" $ A.filter(_.hidden) $ A.fromFoldable e

    hooksTransformGraph

    -- | Render
    -- |
    pure $

      Graph.drawGraph
      { elRef
      , fa2Ref
      , noverlapRef
      , forceAtlas2Settings: Graph.forceAtlas2Settings
      , sigmaRef
      , sigmaSettings: Graph.sigmaSettings
      }

--------------------------------------------------------

convert :: GET.GraphData -> Tuple (Maybe GET.MetaData) SigmaxT.SGraph
convert (GET.GraphData r) = Tuple r.metaData $ SigmaxT.Graph {nodes, edges}
  where
    normalizedNodes :: Array GEGT.Node
    normalizedNodes = (\n -> GEGT.Node (n { size = floor n.size })) <$>
                      (GEU.normalizeNodeSizeDefault $ (\(GEGT.Node n) -> n { size = toNumber n.size }) <$> r.nodes)
    nodes :: Seq.Seq (Record SigmaxT.Node)
    nodes = foldMapWithIndex nodeFn normalizedNodes
    nodeFn :: Int -> GEGT.Node -> Seq.Seq (Record SigmaxT.Node)
    nodeFn _i nn@(GEGT.Node n) =
      let (GEGT.Cluster { clustDefault }) = n.attributes in
      Seq.singleton {
          borderColor: color
        , children: n.children
        , color : color
        , community : clustDefault  -- for the communities-louvain graphology plugin
        , equilateral: { numPoints: 3 }
        , gargType
        , hidden : false
        , highlighted: false
        , id    : n.id_
        , label : n.label
        , size  : toNumber n.size
        --, size: toNumber n.size
        , type  : modeGraphType gargType
        , x     : n.x -- cos (toNumber i)
        , y     : n.y -- sin (toNumber i)
        , _original: nn
        }
      where
        cDef (GEGT.Cluster {clustDefault}) = clustDefault
        color = GET.intColor (cDef n.attributes)
        gargType =  unsafePartial $ fromJust $ Types.modeFromString n.type_
    nodesMap = SigmaxT.nodesMap nodes
    edges = foldMapWithIndex edgeFn $ A.sortWith (\(GEGT.Edge {weight}) -> weight) r.edges
    edgeFn i ee@(GEGT.Edge e) =
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

--------------------------------------------------------------

-- | See sigmajs/plugins/sigma.renderers.customShapes/shape-library.js
modeGraphType :: Types.Mode -> String
modeGraphType Types.Authors     = "triangle"
modeGraphType Types.Institutes  = "square"
modeGraphType Types.Sources     = "diamond"
--modeGraphType Types.Terms       = "def"
--modeGraphType Types.Terms       = "circle"
modeGraphType Types.Terms       = "ccircle"

--------------------------------------------------------------


type LiveProps = (
    edgeConfluence'  :: Range.NumberRange
  , edgeWeight'      :: Range.NumberRange
  , nodeSize'        :: Range.NumberRange
  , removedNodeIds'  :: SigmaxT.NodeIds
  , selectedNodeIds' :: SigmaxT.NodeIds
  , showEdges'       :: SigmaxT.ShowEdgesState
  )

hashLiveProps :: Record LiveProps -> Int
hashLiveProps p = Hashable.hash { edgeConfluence': p.edgeConfluence'
                                , edgeWeight': p.edgeWeight'
                                , nodeSize: p.nodeSize'
                                , removedNodeIds': HashSet.fromFoldable p.removedNodeIds'
                                , selectedNodeIds': HashSet.fromFoldable p.selectedNodeIds'
                                , showEdges': p.showEdges' }

transformGraphStoreParams :: R.Hooks (Record LiveProps)
transformGraphStoreParams = do
  store <- GraphStore.use

  edgeConfluence' <- R2.useLive' store.edgeConfluence
  edgeWeight' <- R2.useLive' store.edgeWeight
  nodeSize' <- R2.useLive' store.nodeSize
  removedNodeIds' <- R2.useLive' store.removedNodeIds
  selectedNodeIds' <- R2.useLive' store.selectedNodeIds
  showEdges' <- R2.useLive' store.showEdges

  pure { edgeConfluence'
       , edgeWeight'
       , nodeSize'
       , removedNodeIds'
       , selectedNodeIds'
       , showEdges' }

hooksTransformGraph :: R.Hooks Unit
hooksTransformGraph = do
  store <- GraphStore.use

  params <- transformGraphStoreParams
  graph' <- R2.useLive' store.graph

  R.useEffect2' (hashLiveProps params) graph' $ do
    T.write_ (transformGraph graph' params) store.transformedGraph

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
        $ SigmaxT.neighboringEdges graph selectedNodeIds'
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
        node { borderColor = "#000", highlighted = true, type = "selected" }
      else
        node { highlighted = false }

    nodeHideSize :: Record SigmaxT.Node -> Record SigmaxT.Node
    nodeHideSize node@{ size } =
      if Range.within nodeSize' size then
        node
      else
        node { hidden = true }
