module Gargantext.Pages.Corpus.Graph where

import Effect.Unsafe
import Gargantext.Prelude hiding (max,min)

import Affjax (defaultRequest, request)
import Affjax.ResponseFormat (ResponseFormat(..), printResponseFormatError)
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Cont.Trans (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.?), (.??), (:=), (~>))
import Data.Argonaut (decodeJson)
import Data.Array (fold, length, mapWithIndex, (!!))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Int (fromString, toNumber)
import Data.Int as Int
import Data.Lens (Lens, Lens', over, (%~), (+~), (.~), (^.))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isNothing)
import Data.Newtype (class Newtype)
import Data.String (joinWith)
import Data.Symbol (SProxy(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Uncurried (runEffectFn1, runEffectFn2)
import Gargantext.Components.GraphExplorer.Sigmajs (Color(Color), SigmaEasing, SigmaGraphData(SigmaGraphData), SigmaNode, SigmaSettings, canvas, edgeShape, edgeShapes, forceAtlas2, setSigmaRef, getSigmaRef, cameras, CameraProps, getCameraProps, goTo, sStyle, sigma, sigmaEasing, sigmaEdge, sigmaEnableWebGL, sigmaNode, sigmaSettings)
import Gargantext.Components.GraphExplorer.Types (Cluster(..), MetaData(..), Edge(..), GraphData(..), Legend(..), Node(..), getLegendData)
import Gargantext.Components.Login.Types (AuthData(..), TreeId)
import Gargantext.Components.RandomText (words)
import Gargantext.Components.Tree as Tree
import Gargantext.Config as Config
import Gargantext.Config.REST (get, post)
import Gargantext.Pages.Corpus.Graph.Tabs as GT
import Gargantext.Prelude (flip)
import Gargantext.Types (class Optional)
import Gargantext.Utils (getter)
import Math (cos, sin)
import Partial.Unsafe (unsafePartial)
import React (ReactElement)
import React.DOM (a, br', h2, button, div, form', input, li, li', menu, option, p, select, span, text, ul, ul')
import React.DOM.Props (_id, _type, checked, className, defaultValue, href, max, min, name, onChange, onClick, placeholder, style, title, value)
import Thermite (PerformAction, Render, Spec, _render, cmapProps, createClass, defaultPerformAction, defaultRender, modifyState, modifyState_, noState, simpleSpec, withState)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem)


data Action
  = LoadGraph Int
  | SelectNode SelectedNode
  | ShowSidePanel Boolean
  | ToggleControls
  | ToggleTree
  | ChangeLabelSize Number
  | ChangeNodeSize Number
  | DisplayEdges
--  | Zoom Boolean

newtype SelectedNode = SelectedNode {id :: String, label :: String}

derive instance eqSelectedNode :: Eq SelectedNode
derive instance newtypeSelectedNode :: Newtype SelectedNode _

-- _settings :: forall s t a b. Lens { settings :: a | s } { settings :: b | t } a b
_settings :: forall s a. Lens' { settings :: a | s } a
_settings = prop (SProxy :: SProxy "settings")

_labelSizeRatio' :: forall s a. Lens' { labelSizeRatio :: a | s } a
_labelSizeRatio' = prop (SProxy :: SProxy "labelSizeRatio")

_labelSizeRatio :: Lens' SigmaSettings Number
_labelSizeRatio f = unsafeCoerce $ _labelSizeRatio' f

_maxNodeSize' :: forall s a. Lens' { maxNodeSize :: a | s} a
_maxNodeSize' = prop (SProxy :: SProxy "maxNodeSize")

_maxNodeSize :: Lens' SigmaSettings Number
_maxNodeSize f = unsafeCoerce $ _maxNodeSize' f

_minNodeSize' :: forall s a. Lens' { minNodeSize :: a | s} a
_minNodeSize' = prop (SProxy :: SProxy "minNodeSize")

_minNodeSize :: Lens' SigmaSettings Number
_minNodeSize f = unsafeCoerce $ _minNodeSize' f

_drawEdges' :: forall s a. Lens' { drawEdges :: a | s} a
_drawEdges' = prop (SProxy :: SProxy "drawEdges")

_drawEdges :: Lens' SigmaSettings Boolean
_drawEdges f = unsafeCoerce $ _drawEdges' f

-- TODO remove newtype here
newtype State = State
  { graphData :: GraphData
  , filePath :: String
  , sigmaGraphData :: Maybe SigmaGraphData
  , legendData :: Array Legend
  , selectedNode :: Maybe SelectedNode
  , showSidePanel :: Boolean
  , showControls :: Boolean
  , showTree :: Boolean
  , corpusId :: Int
  , treeId :: Maybe TreeId
  , settings :: SigmaSettings
  }

initialState :: State
initialState = State
  { graphData : GraphData {nodes: [], edges: [], sides: [], metaData : Just $ MetaData{title : "", legend : [], corpusId : []}}
  , filePath : ""
  , sigmaGraphData : Nothing
  , legendData : []
  , selectedNode : Nothing
  , showSidePanel : false
  , showControls : false
  , showTree : false
  , corpusId : 0
  , treeId : Nothing
  , settings : mySettings
  }

-- This one is not used: specOld is the one being used.
-- TODO: code duplication
  {-
graphSpec :: Spec State {} Action
graphSpec = simpleSpec performAction render
-}

performAction :: PerformAction State {} Action
performAction (LoadGraph fp) _ _ = void do
  _ <- logs fp
  _ <- modifyState \(State s) -> State s {corpusId = fp, sigmaGraphData = Nothing}
  resp <- lift $ getNodes fp
  treeResp <- liftEffect $ getAuthData
  case treeResp of
    Just (AuthData {token,tree_id }) ->
      modifyState \(State s) -> State s {graphData = resp, sigmaGraphData = Just $ convert resp, legendData = getLegendData resp, treeId = Just tree_id}
    Nothing ->
      modifyState \(State s) -> State s { graphData = resp, sigmaGraphData = Just $ convert resp, legendData = getLegendData resp, treeId = Nothing}
      -- TODO: here one might `catchError getNodes` to visually empty the
      -- graph.
  --modifyState \(State s) -> State s {graphData = resp, sigmaGraphData = Just $ convert resp, legendData = getLegendData resp}

performAction (SelectNode (SelectedNode node)) _ (State state) =
  modifyState_ $ \(State s) -> State s {selectedNode = pure $ SelectedNode node}

performAction (ShowSidePanel b) _ (State state) = void do
  modifyState $ \(State s) -> State s {showSidePanel = b }


performAction (ToggleControls) _ (State state) = void do
  modifyState $ \(State s) -> State s {showControls = not (state.showControls) }

performAction (ToggleTree) _ (State state) = void do
  modifyState $ \(State s) -> State s {showTree = not (state.showTree) }

performAction (ChangeLabelSize size) _ _ =
  modifyState_ $ \(State s) ->
    State $ ((_settings <<< _labelSizeRatio) .~ size) s

performAction (ChangeNodeSize size) _ _ =
  modifyState_ $ \(State s) -> do
    let maxNoded = ((_settings <<< _maxNodeSize) .~ size) s
    State $ ((_settings <<< _minNodeSize) .~ (size * 0.10)) maxNoded

performAction DisplayEdges _ _ =
  modifyState_ $ \(State s) -> do
    State $ ((_settings <<< _drawEdges) %~ not) s

--performAction (Zoom True) _ _ =
--  modifyState_ $ \() -> do
--    State $

convert :: GraphData -> SigmaGraphData
convert (GraphData r) = SigmaGraphData {nodes, edges}
  where
    nodes = mapWithIndex nodeFn r.nodes
    nodeFn i (Node n) =
      sigmaNode
        { id    : n.id_
        , size  : toNumber n.size
        , label : n.label
        , x     : cos (toNumber i)
        , y     : sin (toNumber i)
        , color : intColor $ cDef n.attributes
        }
      where
        cDef (Cluster {clustDefault}) = clustDefault
    edges = map edgeFn r.edges
    edgeFn (Edge e) = sigmaEdge {id : e.id_, source : e.source, target : e.target}
{--
render :: Render State {} Action
render d p (State {sigmaGraphData, settings, legendData}) c =
  case sigmaGraphData of
    Nothing -> []
    Just graph ->
      [ sigma { graph
              , settings
              , renderer : canvas
              , style : sStyle { height : "96%"}
              , ref: saveSigmaRef
              , onClickNode : \e -> unsafePerformEffect $ do
                 _ <- log "this should be deleted"
                 -- _ <- logs $ unsafeCoerce e
                 _ <- d $ SelectNode $ SelectedNode {id : (unsafeCoerce e).data.node.id, label : (unsafeCoerce e).data.node.label}
                 pure unit
              -- TODO: fix this!
              }
        [ sigmaEnableWebGL
        , forceAtlas2 forceAtlas2Config
        , edgeShapes {"default" : edgeShape.curve}
        ]
      ]
  -- TODO clean unused code: this seems to be not used
  -- <>
  -- [dispLegend legendData]
--}

forceAtlas2Config :: { slowDown :: Number
                    , startingIterations :: Number
                    , iterationsPerRender :: Number
                    , barnesHutOptimize :: Boolean
                    , linLogMode :: Boolean
                    , edgeWeightInfluence :: Number
                    , gravity :: Number
                    , strongGravityMode :: Boolean
                    , scalingRatio :: Number
                    , skipHidden :: Boolean
                    , adjustSizes :: Boolean
                    , outboundAttractionDistribution :: Boolean
                    }
forceAtlas2Config = { -- fixedY : false
                       slowDown : 0.7
                      , startingIterations : 2.0
                      , iterationsPerRender : 4.0
                      , barnesHutOptimize   : true
                      , linLogMode : true  -- false
                      , edgeWeightInfluence : 0.0
                      , gravity : 1.0
                      , strongGravityMode : false
                      , scalingRatio : 4.0
                      , skipHidden: false
                      , adjustSizes : false
                      , outboundAttractionDistribution: false
                      }

mySettings :: SigmaSettings
mySettings = sigmaSettings { verbose : true
                           , drawLabels: true
                           , drawEdgeLabels: true
                           , drawEdges: false
                           , drawNodes: true
                           , labelSize : "proportional"
                           --, nodesPowRatio: 0.3
                           , batchEdgesDrawing: false
                           , hideEdgesOnMove: true

                           , enableHovering: true
                           , singleHover: true
                           , enableEdgeHovering: false

                           , autoResize: true
                           , autoRescale: true
                           , rescaleIgnoreSize: false

                           , mouseEnabled: true
                           , touchEnabled: true

                           , animationsTime: 1500.0

                           , defaultNodeColor: "#ddd"
                           , twNodeRendBorderSize: 0.5          -- node borders (only iff ourRendering)
                           , twNodeRendBorderColor: "#222"

                          -- edges
                          , minEdgeSize: 0.0              -- in fact used in tina as edge size
                          , maxEdgeSize: 0.0
                          --, defaultEdgeType: "curve"      -- 'curve' or 'line' (curve only iff ourRendering)
                          , twEdgeDefaultOpacity: 0.4       -- initial opacity added to src/tgt colors
                          , minNodeSize: 1.0
                          , maxNodeSize: 10.0
--
--  -- labels
                          , font: "Droid Sans"                -- font params
                          , fontStyle: "bold"
                          , defaultLabelColor: "#000"         -- labels text color
                          , labelSizeRatio: 2.0               -- label size in ratio of node size
                          , labelThreshold: 2.0               -- min node cam size to start showing label
                          , labelMaxSize: 3.0                -- (old tina: showLabelsIfZoom)

                          -- hovered nodes
                          , defaultHoverLabelBGColor: "#fff"
                          , defaultHoverLabelColor: "#000"
                          , borderSize: 3.0                   -- for ex, bigger border when hover
                          , nodeBorderColor: "node"           -- choices: 'default' color vs. node color
                          , defaultNodeBorderColor: "black"   -- <- if nodeBorderColor = 'default'
                          -- selected nodes <=> special label
                          , twSelectedColor: "default"     -- "node" for a label bg like the node color, "default" for white background
                         -- not selected <=> (1-greyness)


                          , twNodesGreyOpacity: 5.5           -- smaller value: more grey
                          , twBorderGreyColor: "rgba(100, 100, 100, 0.5)"
                          , twEdgeGreyColor: "rgba(100, 100, 100, 0.25)"
                          , zoomMin: 0.0
                          , zoomMax: 1.7
                          , zoomingRatio: 3.2
                          , mouseZoomDuration: 150.0
                          }


defaultPalette :: Array Color
defaultPalette = map Color defaultPalette'

defaultPalette' :: Array String
defaultPalette' = ["#5fa571","#ab9ba2","#da876d","#bdd3ff","#b399df","#ffdfed","#33c8f3","#739e9a","#caeca3","#f6f7e5","#f9bcca","#ccb069","#c9ffde","#c58683","#6c9eb0","#ffd3cf","#ccffc7","#52a1b0","#d2ecff","#99fffe","#9295ae","#5ea38b","#fff0b3","#d99e68"]

-- clusterColor :: Cluster -> Color
-- clusterColor (Cluster {clustDefault}) = unsafePartial $ fromJust $ defaultPalette !! (clustDefault `mod` length defaultPalette)


intColor :: Int -> Color
intColor i = unsafePartial $ fromJust $ defaultPalette !! (i `mod` length defaultPalette)

modCamera0 :: forall o. Optional o CameraProps =>
              (Record CameraProps -> Record o) -> Effect Unit
modCamera0 f = do
  s <- getSigmaRef
  for_ (cameras s !! 0) $ \cam ->
    void $ goTo cam (f $ getCameraProps cam)

type NOverlapConfig =
  { nodes :: Array SigmaNode
  , nodeMargin :: Number
  , scaleNodes :: Number
  , gridSize :: Number
  , permittedExpansion :: Number
  , speed :: Number
  , maxIterations :: Number
  , easing :: SigmaEasing
  , duration :: Number
  }


nOverlap :: Array SigmaNode -> NOverlapConfig
nOverlap ns =  { nodes : ns
                   ,  nodeMargin : 0.4
                   , scaleNodes : 1.5
                   , gridSize : 300.0
                   , permittedExpansion : 1.0
                   , speed : 7.0
                   , maxIterations : 8.0
                   , easing : sigmaEasing.quadraticOut
                   , duration : 1500.0
                   }

dispLegend :: Array Legend -> ReactElement
dispLegend ary = div [] $ map dl ary
  where
    dl (Legend {id_, color, label}) =
      p []
      [ span [style {width : 10, height : 10, backgroundColor : intColor id_ , display: "inline-block"}] []
      , text $ " " <> label
      ]


specOld :: Spec State {} Action
specOld = fold [treespec treeSpec, graphspec $ simpleSpec performAction render']
  where
    treespec = over _render \frender d p (State s) c ->

                [ div [ className "col-md-1", _id "graph-tree", style {marginTop:"151px"}] $
                  [
                     button [className "btn btn-primary" , onClick \_ -> d ToggleTree]
                     [text $ if s.showTree then "Hide Tree" else "Show Tree"]
                  ]
                  <>
                  if s.showTree then (frender d p (State s) c) else []
                ]



    graphspec   = over _render \frender d p s c -> [
         div [ className "col-md-9"] (frender d p s c)
      ]
    treeSpec :: Spec State {} Action
    treeSpec = withState \(State st) ->
      case st.treeId of
        Nothing ->
          simpleSpec defaultPerformAction defaultRender
        Just treeId ->
          (cmapProps (const {root: treeId}) (noState Tree.treeview))
    render' :: Render State {} Action
    render' d _ (State st@{settings, graphData: GraphData {sides,metaData  }}) _ =
      [ div [className "container-fluid", style {"padding-top" : "100px"}]
      [ div [ className "row"]
        [ h2 [ style {textAlign : "center", position : "relative", top: "-38px"}]
          [-- :  MetaData {title}
            case metaData of
              Just( MetaData {title }) ->
                text $ "Graph " <> title
              Nothing ->
                text "Title"
          ]
        ]
       , div [className "row", style {"padding-bottom" : "10px", marginTop : "-24px"}]
      [
           div [className "col-md-4"]
           [
            ]
          , div [className "col-md-4"]
           [
             button [className "btn btn-primary center-block"
             , onClick \_ -> d ToggleControls
             ]
             [text $ if st.showControls then "Hide Controls" else "Show Controls"]

            ]
          , div [className "col-md-4"]
          [ div [className "pull-right"]
            [ button [className "btn btn-primary"
               ,onClick \_ -> d $ ShowSidePanel $ not st.showSidePanel
               ] [text $ if st.showSidePanel then "Hide Side Panel" else "Show Side Panel"]
            ]
          ]
      ],
      div [className "row"]
      [
           if (st.showControls) then
              div [className "col-md-12", style {"padding-bottom" : "10px"}]
            [ menu [_id "toolbar"]
              [ ul'
                [
                  li'
                  [ button [className "btn btn-success btn-sm"] [text "Change Type"]
                  ]
                ,
                  li'
                  [ button [
                         className "btn btn-primary btn-sm"
                       , onClick \_ -> d DisplayEdges
                           ]
                    [text "Toggle Edges"]
                  ]
                , li'
                  [ button [className "btn btn-primary btn-sm"] [text "Change Level"]
                  ]
                 ,li [style {display : "inline-block"}]
                  [ form'
                    [ input [_type "file"
                            , name "file"
                         --   , onChange (\e -> d $ SetFile (getFile e) (unsafeCoerce $ d <<< SetProgress))
                            , className "btn btn-primary"]

                    -- , text $ show st.readyState
                    ]
                  ]
                , li' [ input [_type "button"
                              , className "btn btn-warning btn-sm"
                              ,value "Run Demo"
                            --  , onClick \_ -> d SetGraph, disabled (st.readyState /= DONE)
                              ]
                      ]

                , li'
                  [ form'
                    [ div [className "col-lg-2"]
                      [
                        div [className "input-group"]
                        [
                          span [className "input-group-btn"]
                          [
                            button [className "btn btn-primary", _type "button"]
                            [ span [className "glyphicon glyphicon-search"] []
                            ]
                          ]
                          ,input [_type "text", className "form-control", placeholder "select topics"]
                        ]
                      ]

                    ]
                  ]
                , li [className "col-md-2"]
                  [ span [] [text "Selector"],input [_type "range", _id "myRange", value "90"]
                  ]
                , li [className "col-md-2"]
                  [ span [] [text "Labels"],input [_type "range"
                                                 , _id "labelSizeRange"
                                                 , max "4"
                                                 , defaultValue <<< show $ settings ^. _labelSizeRatio
                                                 , min "0"
                                                 , onChange \e -> d $ ChangeLabelSize (unsafeCoerce e).target.value
                                                 ]
                  ]

                , li [className "col-md-2"]
                  [ span [] [text "Nodes"],input [_type "range"
                                                 , _id "nodeSizeRange"
                                                 , max "20"
                                                 , defaultValue <<< show $ settings ^. _maxNodeSize
                                                 , min "0"
                                                 , onChange \e -> d $ ChangeNodeSize (unsafeCoerce e).target.value
                                                 ]
                  ]
                , li [className "col-md-2"]
                  [ span [] [text "Edges"],input [_type "range", _id "myRange", value "90"]
                  ]
                , li'
                  [ button [ className "btn btn-primary"
                           , onClick \_ -> modCamera0 (const {x: 0.0, y: 0.0, ratio: 1.0})
                           ] [text "Center"]
                  ]
                , li'
                  [ button [className "btn btn-primary"
                            , onClick \_ -> do
                                             _ <- log "Hey there" -- $ show st.camera
                                             pure unit
                           ] [text "Save"] -- TODO: Implement Save!
                  ]
                ]
              ]
            ]
            else div [] []
         ]
         , div [className "row"]
           [div [if (st.showSidePanel && st.showTree) then className "col-md-10" else if (st.showSidePanel || st.showTree) then className "col-md-10" else className "col-md-12"]
             [ div [style {height: "90%"}] $
               [
               ]
               <>
               case st.sigmaGraphData of
                   Nothing -> []
                   Just graph ->
                     [ sigma { graph, settings
                             , renderer : canvas
                             , style : sStyle { height : "95%"}
                             , ref: setSigmaRef
                             , onClickNode : \e ->
                             unsafePerformEffect $ do
                               modCamera0 $ \{ratio} -> {ratio: ratio / 2.0}
                               _ <- d $ ShowSidePanel true
                               _ <- d $ SelectNode $ SelectedNode {id : (unsafeCoerce e).data.node.id, label : (unsafeCoerce e).data.node.label}
                               pure unit
                             }
                       [ sigmaEnableWebGL
                       , forceAtlas2 forceAtlas2Config
                       , edgeShapes {"default" : edgeShape.curve}
                       ]
                     ]
                 <>
                 if length st.legendData > 0 then [div [style {position : "absolute", bottom : "10px", border: "1px solid black", boxShadow : "rgb(0, 0, 0) 0px 2px 6px", marginLeft : "10px", padding:  "16px"}] [dispLegend st.legendData]] else []
             ]
         --, button [onClick \_ -> d ShowSidePanel, className "btn btn-primary", style {right:"39px",position : "relative",zIndex:"1000", top: "-59px"}] [text "Show SidePanel"]
         , if (st.showSidePanel) then
            div [_id "sp-container", className "col-md-2", style {border : "1px white solid", backgroundColor : "white"}]
             [ div [className "row"] $
--             , div [className "col-md-12"]
--               [
--                 ul [className "nav nav-tabs"
--                    , _id "myTab"
--                    , role "tablist"
--                    , style {marginBottom : "18px", marginTop : "18px"}
--                    ]
--                 [
--                   li [className "nav-item"]
--                   [
--                     a [className "nav-link active"
--                       , _id "home-tab"
--                       ,  _data {toggle : "tab"}
--                       , href "#home"
--                       , role "tab"
--                       , aria {controls :"home" , selected : "true"}
--                       ] [text "Neighbours"]
--                   ]
--                 ]
--                 , div [className "tab-content", _id "myTabContent", style {borderBottom : "1px solid black", paddingBottom : "19px"}]
--                   [ div [ className "", _id "home", role "tabpanel" ]
--                     [ a [ className "badge badge-light"][text "objects"]
--                     , a [ className "badge badge-light"][text "evaluation"]
--                     , a [ className "badge badge-light"][text "dynamics"]
--                     , a [ className "badge badge-light"][text "virtual environments"]
--                     , a [ className "badge badge-light"][text "virtual reality"]
--                     , a [ className "badge badge-light"][text "performance analysis"]
--                     , a [ className "badge badge-light"][text "software engineering"]
--                     , a [ className "badge badge-light"][text "complex systems"]
--                     , a [ className "badge badge-light"][text "wireless communications"]
--
--                     ]
--                   ]
--                 ]
             {-, div [className "col-md-12", _id "horizontal-checkbox"]
               [ ul [ style {display: "inline",float : "left" }]
                 [ li []
                   [ span [] [text "Pubs"]
                     ,input [ _type "checkbox"
                           , className "checkbox"
                           , checked $ true
                           , title "Mark as completed"
                             --  , onChange $ dispatch <<< ( const $ SetMap $ not (getter _._type state.term == MapTerm))
                           ]

                   ]
                 , li []
                   [ span [] [text "Projects"]
                     ,input [ _type "checkbox"
                           , className "checkbox"
                           , checked $ false
                           , title "Mark as completed"
                             --  , onChange $ dispatch <<< ( const $ SetMap $ not (getter _._type state.term == MapTerm))
                           ]
                   ]
                 , li []
                   [ span [] [text "Patents"]
                     ,input [ _type "checkbox"
                           , className "checkbox"
                           , checked $ false
                           , title "Mark as completed"
                             --  , onChange $ dispatch <<< ( const $ SetMap $ not (getter _._type state.term == MapTerm))
                           ]
                   ]
                 , li []
                   [ span [] [text "Others"]
                     ,input [ _type "checkbox"
                           , className "checkbox"
                           , checked $ false
                           , title "Mark as completed"
                             --  , onChange $ dispatch <<< ( const $ SetMap $ not (getter _._type state.term == MapTerm))
                           ]
                   ]
                 ]

               ] --}

              [ div []
                [ p [] []
                , div [className "col-md-12"]
                  [ case st.selectedNode of
                      Just (SelectedNode {label}) ->
                        GT.tabsElt {query: words label, sides}
                      Nothing -> p [] []
                  , p [] []
                  ]
                ]
              ]
             ]
            else
              div [] []   -- ends sidepanel column here
           ]
         ]
      ]


getNodes :: Int -> Aff GraphData
getNodes graphId = get $ Config.toUrl Config.Back Config.Graph $ Just graphId

getAuthData :: Effect (Maybe AuthData)
getAuthData = do
  w  <- window
  ls <- localStorage w
  mto <- getItem "token" ls
  mti <- getItem "tree_id" ls
  pure do
    token <- mto
    tree_id <- Int.fromString =<< mti
    pure $ AuthData {token, tree_id}
