module Gargantext.Components.GraphExplorer where

import Gargantext.Prelude hiding (max,min)

import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import Data.Tuple (fst,snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as RH

import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Hooks.Sigmax (Sigma)
import Gargantext.Hooks.Sigmax.Types as Sigmax
import Gargantext.Components.GraphExplorer.Controls as Controls
import Gargantext.Components.GraphExplorer.Sidebar as Sidebar
import Gargantext.Components.GraphExplorer.ToggleButton as Toggle
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.Graph as Graph
import Gargantext.Components.Forest (forest)
import Gargantext.Ends (Frontends)
import Gargantext.Routes (SessionRoute(NodeAPI), AppRoute)
import Gargantext.Sessions (Session, Sessions(..), get)
import Gargantext.Types (NodeType(Graph))

type GraphId = Int

type LayoutProps =
  ( graphId :: GraphId
  , mCurrentRoute :: AppRoute
  , treeId :: Maybe Int
  , session :: Session
  , sessions :: Sessions
  , frontends :: Frontends 
  )

type Props = ( graph :: Maybe Graph.Graph | LayoutProps )

--------------------------------------------------------------
explorerLayout :: Record LayoutProps -> R.Element
explorerLayout props = R.createElement explorerLayoutCpt props []

explorerLayoutCpt :: R.Component LayoutProps
explorerLayoutCpt = R.hooksComponent "G.C.GraphExplorer.explorerLayout" cpt
  where
    cpt {graphId, mCurrentRoute, treeId, session, sessions, frontends} _ =
      useLoader graphId (getNodes session) handler
      where
        handler loaded = explorer {graphId, mCurrentRoute, treeId, session, sessions, graph, frontends}
          where graph = Just (convert loaded)

--------------------------------------------------------------
explorer :: Record Props -> R.Element
explorer props = R.createElement explorerCpt props []

explorerCpt :: R.Component Props
explorerCpt = R.hooksComponent "G.C.GraphExplorer.explorer" cpt
  where
    cpt {sessions, session, graphId, mCurrentRoute, treeId, graph, frontends} _ = do
      controls <- Controls.useGraphControls
      state <- useExplorerState
      showLogin <- snd <$> R.useState' true
      pure $
        RH.div
          { id: "graph-explorer" }
          [ row
            [ outer
              [ inner
                [ row1
                  [ col [ pullLeft [ Toggle.treeToggleButton controls.showTree ] ]
                  , col [ Toggle.controlsToggleButton controls.showControls ]
                  , col [ pullRight [ Toggle.sidebarToggleButton controls.showSidePanel ] ]
                  ]
                , row [ Controls.controls controls ]
                , row [ tree {mCurrentRoute, treeId} controls showLogin
                      , mGraph controls.sigmaRef {graphId, graph}
                      , Sidebar.sidebar {showSidePanel: fst controls.showSidePanel} ]
                , row [ ]
                ]
              ]
            ]
          ]
      where
        -- tree {treeId: Nothing} _ _ = RH.div { id: "tree" } []
        tree _ {showTree: false /\ _} _ = RH.div { id: "tree" } []
        tree {mCurrentRoute: route, treeId: root} _ showLogin= 
          RH.div {className: "col-md-2", style: {paddingTop: "60px"}}
          [forest {sessions, route, frontends, showLogin}]
    outer = RH.div { className: "col-md-12" }
    inner = RH.div { className: "container-fluid", style: { paddingTop: "90px" } }
    row1  = RH.div { className: "row", style: { paddingBottom: "10px", marginTop: "-24px" } }
    row   = RH.div { className: "row" }
    col       = RH.div { className: "col-md-4" }
    pullLeft  = RH.div { className: "pull-left" }
    pullRight = RH.div { className: "pull-right" }


    mGraph :: R.Ref (Maybe Sigma) -> {graphId :: GraphId, graph :: Maybe Graph.Graph} -> R.Element
    mGraph _ {graph: Nothing} = RH.div {} []
    mGraph sigmaRef {graphId, graph: Just graph} = graphView sigmaRef {graphId, graph}

useExplorerState :: R.Hooks (Record GET.State)
useExplorerState = do pure {}
{-   corpusId <- R.useState' 0
  cursorSize <- R.useState' 0.0
  filePath <- R.useState' ""
  graphData <- R.useState' initialGraphData
  legendData <- R.useState' []
  multiNodeSelection <- R.useState' false
  selectedNodes <- R.useState' Set.empty
  showControls <- R.useState' false
  showSidePanel <- R.useState' false
  showTree <- R.useState' false
  sigmaGraphData <- R.useState' (Nothing :: Maybe Graph.Graph)
  sigmaSettings <- R.useState' Graph.sigmaSettings
  treeId <- R.useState' (Nothing :: Maybe TreeId) -}

  --treeId : Nothing

type GraphProps = (
    graphId :: GraphId
  , graph :: Graph.Graph
)

graphView :: R.Ref (Maybe Sigma) -> Record GraphProps -> R.Element
--graphView sigmaRef props = R.createElement (R.memo el memoCmp) props []
graphView sigmaRef props = R.createElement el props []
  where
    --memoCmp props1 props2 = props1.graphId == props2.graphId
    el = R.hooksComponent "GraphView" cpt
    cpt {graphId, graph} _children = do
      pure $
        RH.div { id: "graph-view", className: "col-md-12" }
        [
          Graph.graph {
               forceAtlas2Settings: Graph.forceAtlas2Settings
             , graph
             , sigmaSettings: Graph.sigmaSettings
             , sigmaRef: sigmaRef
             }
        ]

convert :: GET.GraphData -> Graph.Graph
convert (GET.GraphData r) = Sigmax.Graph {nodes, edges}
  where
    nodes = foldMapWithIndex nodeFn r.nodes
    nodeFn i (GET.Node n) =
      Seq.singleton
        { id    : n.id_
        , size  : toNumber n.size
        , label : n.label
        , x     : n.x -- cos (toNumber i)
        , y     : n.y -- sin (toNumber i)
        , color : GET.intColor (cDef n.attributes)
        }
      where
        cDef (GET.Cluster {clustDefault}) = clustDefault
    edges = foldMap edgeFn r.edges
    edgeFn (GET.Edge e) = Seq.singleton {id : e.id_, source : e.source, target : e.target}

defaultPalette :: Array String
defaultPalette = ["#5fa571","#ab9ba2","#da876d","#bdd3ff","#b399df","#ffdfed","#33c8f3","#739e9a","#caeca3","#f6f7e5","#f9bcca","#ccb069","#c9ffde","#c58683","#6c9eb0","#ffd3cf","#ccffc7","#52a1b0","#d2ecff","#99fffe","#9295ae","#5ea38b","#fff0b3","#d99e68"]

-- clusterColor :: Cluster -> Color
-- clusterColor (Cluster {clustDefault}) = unsafePartial $ fromJust $ defaultPalette !! (clustDefault `mod` length defaultPalette)
    
--               div [className "col-md-12", style {"padding-bottom" : "10px"}]
--             [ menu [_id "toolbar"]
--               [ ul'
--                 [
--                 --  li' [ button [className "btn btn-success btn-sm"] [text "Change Type"] ]
--                 -- ,
--                 -- , li' [ button [className "btn btn-primary btn-sm"] [text "Change Level"] ]
--                 {- ,li [style {display : "inline-block"}]
--                   [ form'
--                     [ input [_type "file"
--                             , name "file"
--                          --   , onChange (\e -> d $ SetFile (getFile e) (unsafeCoerce $ d <<< SetProgress))
--                             , className "btn btn-primary"]

--                     -- , text $ show st.readyState
--                     ]
--                   ]
--                 -}
--                 {-, li' [ input [_type "button"
--                               , className "btn btn-warning btn-sm"
--                               ,value "Run Demo"
--                             --  , onClick \_ -> d SetGraph, disabled (st.readyState /= DONE)
--                               ]
--                       ]
--                       -}
--                 {-, li'
--                   [ form'
--                     [ div [className "col-lg-2"]
--                       [
--                         div [className "input-group"]
--                         [
--                           span [className "input-group-btn"]
--                           [
--                             button [className "btn btn-primary", _type "button"]
--                             [ span [className "glyphicon glyphicon-search"] []
--                             ]
--                           ]
--                           , input [_type "text", className "form-control", placeholder "select topics"]
--                         ]
--                       ]

--                     ]
--                   ]
--                 -}
--                  li [className "col-md-1"]
--                   [ span [] [text "Selector"]
--                   , input [ _type "range"
--                           , _id "cursorSizeRange"
--                           , min "0"
--                           , max "100"
--                           , defaultValue (show st.cursorSize)
--                           , onChange \e -> d $ ChangeCursorSize (numberTargetValue e)
--                           ]
--                   ]
--                 , li [className "col-md-1"]
--                   [ span [] [text "Labels"],input [_type "range"
--                                                  , _id "labelSizeRange"
--                                                  , max "4"
--                                                  , defaultValue <<< show $ sigmaSettings ^. _labelSizeRatio
--                                                  , min "1"
--                                                  , onChange \e -> d $ ChangeLabelSize (numberTargetValue e)
--                                                  ]
--                   ]

--                 , li [className "col-md-1"]
--                   [ span [] [text "Nodes"],input [_type "range"
--                                                  , _id "nodeSizeRange"
--                                                  , max "15"
--                                                  , defaultValue <<< show $ sigmaSettings ^. _minNodeSize
--                                                  , min "5"
--                                                  , onChange \e -> d $ ChangeNodeSize (numberTargetValue e)
--                                                  ]
--                   ]
--                 {-, li [className "col-md-2"]
--                   [ span [] [text "Edges"],input [_type "range", _id "myRange", value "90"]
--                   ]
--                 -}
--                 -- , li'
--                   -- [ button [ className "btn btn-primary"
--                   --          , onClick \_ -> modCamera0 (const {x: 0.0, y: 0.0, ratio: 1.0})
--                   --          ] [text "Center"]
--                   -- ]
--                 -- , li [className "col-md-1"]
--                 --   [ span [] [text "Zoom"],input [ _type "range"
--                 --                                 , _id "cameraRatio"
--                 --                                 , max "100"
--                 --                                 , defaultValue "0"
--                 --                                 , min "0"
--                 --                                 , onChange \e -> do
--                 --                                     let ratio = (100.0 - numberTargetValue e) / 100.0pa
--                 --                                     modCamera0 (const {ratio})
--                 --                                 ]
--                 --   ]
--                 , li [className "col-md-1"]
--                   [ span [] [text "MultiNode"]
--                   , input
--                     [ _type "checkbox"
--                     , className "checkbox"
--                     -- , checked
--                     , onChange $ const $ d ToggleMultiNodeSelection
--                     ]
--                   ]
--                 , li'
--                   [ button [ className "btn btn-primary"
--                            , onClick \_ -> pauseForceAtlas2
--                            ] [text "Spatialization"]
--                   ]
--                 {-, li'
--                   [ button [className "btn btn-primary"
--                             , onClick \_ -> do
--                                              _ <- log "Hey there" -- $ show st.camera
--                                              pure unit
--                            ] [text "Save"] -- TODO: Implement Save!
--                   ]
--                 -}
--                 ]
--               ]


getNodes :: Session -> GraphId -> Aff GET.GraphData
getNodes session graphId = get session $ NodeAPI Graph (Just graphId) ""
