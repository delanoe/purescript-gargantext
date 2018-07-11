module Graph where

import Prelude hiding (div)

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import DOM (DOM)
import Data.Argonaut (decodeJson)
import Data.Array (length, mapWithIndex, (!!))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.MediaType.Common (applicationJSON)
import Data.Newtype (class Newtype)
import Gargantext.Components.GraphExplorer.Sigmajs (Color(Color), SigmaEasing, SigmaGraphData(SigmaGraphData), SigmaNode, SigmaSettings, canvas, edgeShape, edgeShapes, forceAtlas2, sStyle, sigma, sigmaEasing, sigmaEdge, sigmaEnableWebGL, sigmaNode, sigmaSettings)
import Gargantext.Components.GraphExplorer.Types (Cluster(..), Edge(..), GraphData(..), Legend(..), Node(..), getLegendData)
import Gargantext.Utils (getter)
import Math (cos, sin)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Partial.Unsafe (unsafePartial)
import React (ReactElement)
import React.DOM (a, br', button, div, form', input, li, li', menu, option, p, select, span, text, ul, ul')
import React.DOM.Props (_id, _type, checked, className, href, name, onChange, placeholder, style, title, value)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)
import Unsafe.Coerce (unsafeCoerce)

data Action = NoOp
  | LoadGraph String
  | SelectNode SelectedNode

newtype SelectedNode = SelectedNode {id :: String, label :: String}

derive instance eqSelectedNode :: Eq SelectedNode
derive instance newtypeSelectedNode :: Newtype SelectedNode _

newtype State = State
  { graphData :: GraphData
  , filePath :: String
  , sigmaGraphData :: Maybe SigmaGraphData
  , legendData :: Array Legend
  , selectedNode :: Maybe SelectedNode
  }

initialState :: State
initialState = State
  { graphData : GraphData {nodes : [], edges : []}
  , filePath : ""
  , sigmaGraphData : Nothing
  , legendData : []
  , selectedNode : Nothing
  }

graphSpec :: forall eff props. Spec (ajax :: AJAX, console :: CONSOLE, dom :: DOM | eff) State props Action
graphSpec = simpleSpec performAction render

performAction :: forall eff props. PerformAction (ajax :: AJAX, console :: CONSOLE , dom :: DOM | eff) State props Action
performAction (LoadGraph fp) _ _ = void do
  _ <- liftEff $ log fp
  case fp of
    "" -> do
      modifyState \(State s) -> State s {filePath = fp, graphData = GraphData {nodes : [], edges : []}, sigmaGraphData = Nothing}
    _  -> do
      _ <- modifyState \(State s) -> State s {filePath = fp, sigmaGraphData = Nothing}
      gd <- lift $ getGraphData fp
      case gd of
        Left err -> do
          modifyState \(State s) -> State s {filePath = fp, graphData = GraphData {nodes : [], edges : []}}
        Right gd' -> do
          modifyState \(State s) -> State s {filePath = fp, graphData = gd', sigmaGraphData = Just $ convert gd', legendData = getLegendData gd'}

performAction (SelectNode node) _ _ = void do
  modifyState $ \(State s) -> State s {selectedNode = pure node}

performAction NoOp _ _ = void do
  modifyState id


convert :: GraphData -> SigmaGraphData
convert (GraphData r) = SigmaGraphData { nodes, edges}
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

render :: forall props. Render State props Action
render d p (State s) c =
  [ select [ onChange $ \e -> d $ LoadGraph (unsafeCoerce e).target.value, value s.filePath]
    [ option [value ""] [text ""]
    , option [value "example_01_clean.json"] [text "example_01_clean.json"]
    , option [value "example_01_conditional.json"] [text "example_01_conditional.json"]
    , option [value "example_01_distributional.json"] [text "example_01_distributional.json"]
    , option [value "example_02.json"] [text "example_02.json"]
    , option [value "example_02_clean.json"] [text "example_02_clean.json"]
    , option [value "example_03.json"] [text "example_03.json"]
    , option [value "example_03_clean.json"] [text "example_03_clean.json"]
    , option [value "imtNew.json"] [text "imtNew.json"]
    -- , option [value "exemplePhyloBipartite.gexf"] [text "exemplePhyloBipartite.gexf"]
    ]
  ]
  <>
  case s.sigmaGraphData of
    Nothing -> []
    Just gData ->
      [ sigma { graph: gData
              , renderer : canvas
              , settings : mySettings
              , style : sStyle { height : "95%"}
              , onClickNode : \e -> unsafePerformEff $ do
                log $ unsafeCoerce e
                d $ SelectNode $ SelectedNode {id : (unsafeCoerce e).data.node.id, label : (unsafeCoerce e).data.node.label}
                pure unit
              }
        [ sigmaEnableWebGL
        , forceAtlas2 forceAtlas2Config
        , edgeShapes {"default" : edgeShape.curve}
        ]
      ]
  <>
  [dispLegend s.legendData]

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
                           , drawEdges: true
                           , drawNodes: true
                           , labelSize : "proportional"
                           --, nodesPowRatio: 0.3
                           ,  batchEdgesDrawing: false
                           ,  hideEdgesOnMove: true

                           ,  enableHovering: true
                           ,  singleHover: true
                           ,  enableEdgeHovering: false

                           ,  autoResize: true
                           ,  autoRescale: true
                           ,  rescaleIgnoreSize: false

                           ,  mouseEnabled: true
                           ,  touchEnabled: true

                           ,  animationsTime: 1500.0

                           , defaultNodeColor: "#ddd"
                           , twNodeRendBorderSize: 0.5          -- node borders (only iff ourRendering)
                           , twNodeRendBorderColor: "#222"

                          -- edges
                          , minEdgeSize: 1.0              -- in fact used in tina as edge size
                          --, defaultEdgeType: "curve"      -- 'curve' or 'line' (curve only iff ourRendering)
                          , twEdgeDefaultOpacity: 0.4       -- initial opacity added to src/tgt colors
--
--  -- labels
                          , font: "Droid Sans"                -- font params
                          , fontStyle: "bold"
                          , defaultLabelColor: "#000"         -- labels text color
                          , labelSizeRatio: 2.0               -- label size in ratio of node size
                          , labelThreshold: 2.0               -- min node cam size to start showing label
                          , labelMaxSize: 10.0                -- (old tina: showLabelsIfZoom)

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


-- loadJSON  {path : "http://localhost:2015/examples/sites_coords.json"}
getGraphData :: forall eff. String -> Aff (console :: CONSOLE, ajax :: AJAX , dom :: DOM | eff ) (Either String GraphData)
getGraphData fp = do
  resp <- liftAff $ attempt $ affjax defaultRequest
          { url =("http://localhost:2015/examples/" <> fp)
          , method = Left GET
          , headers =
            [ ContentType applicationJSON
            , Accept applicationJSON
            ]
          }
  case resp of
    Left err -> do
      liftEff $ log $ show err
      pure $ Left $ show err
    Right a -> do
      liftEff $ log $ show a.response
      let gd = decodeJson a.response
      pure gd



defaultPalette :: Array Color
defaultPalette = map Color defaultPalette'

defaultPalette' :: Array String
defaultPalette' = ["#5fa571","#ab9ba2","#da876d","#bdd3ff","#b399df","#ffdfed","#33c8f3","#739e9a","#caeca3","#f6f7e5","#f9bcca","#ccb069","#c9ffde","#c58683","#6c9eb0","#ffd3cf","#ccffc7","#52a1b0","#d2ecff","#99fffe","#9295ae","#5ea38b","#fff0b3","#d99e68"]

-- clusterColor :: Cluster -> Color
-- clusterColor (Cluster {clustDefault}) = unsafePartial $ fromJust $ defaultPalette !! (clustDefault `mod` length defaultPalette)


intColor :: Int -> Color
intColor i = unsafePartial $ fromJust $ defaultPalette !! (i `mod` length defaultPalette)


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
    dl (Legend {id_, label}) =
      p []
      [ span [style {width : 10, height : 10, backgroundColor : intColor id_, display: "inline-block"}] []
      , text $ " " <> label
      ]


specOld :: forall eff props. Spec (console :: CONSOLE, dom :: DOM, ajax :: AJAX | eff) State props Action
specOld = simpleSpec performAction render
  where
    render :: Render State props Action
    render d _ (State st) _ =
      [  div [className "row"] [
            div [className "col-md-12", style {marginTop : "21px", marginBottom : "21px"}]
            [ menu [_id "toolbar"]
              [ ul'
                [
                  li'
                  [ button [className "btn btn-success btn-sm"] [text "Change Type"]
                  ]
                , li'
                  [ button [className "btn btn-primary btn-sm"] [text "Change Level"]
                  ]


                 ,li [style {display : "inline-block"}]
                  [ form'
                    [ input [_type "file"
                            , name "file"
                         --   , onChange (\e -> d $ SetFile (getFile e) (unsafeCoerce $ d <<< SetProgress))
                            , className "btn btn-primary"] []

                    -- , text $ show st.readyState
                    ]
                  ]
                , li' [ input [_type "button"
                              , className "btn btn-warning btn-sm"
                              ,value "Run Demo"
                            --  , onClick \_ -> d SetGraph, disabled (st.readyState /= DONE)
                              ] []
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
                          ,input [_type "text", className "form-control", placeholder "select topics"] []
                        ]
                      ]

                    ]
                  ]
                , li [className "col-md-2"]
                  [ span [] [text "selector size"],input [_type "range", _id "myRange", value "90"] []
                  ]
                , li [className "col-md-2"]
                  [ span [] [text "label size"],input [_type "range", _id "myRange", value "90"] []
                  ]

                , li [className "col-md-2"]
                  [ span [] [text "Nodes"],input [_type "range", _id "myRange", value "90"] []
                  ]
                , li [className "col-md-2"]
                  [ span [] [text "Edges"],input [_type "range", _id "myRange", value "90"] []
                  ]
                , li'
                  [ button [className "btn btn-primary"] [text "Save"] -- TODO: Implement Save!
                  ]
                ]
              ]
            ]
           ]
         , div [className "row"]
           [ div [className "col-md-9"]
             [ div [style {border : "1px black solid", height: "90%"}] $
               [ select [ onChange $ \e -> d $ LoadGraph (unsafeCoerce e).target.value
                        , value st.filePath
                        ]
                 [ option [value ""] [text ""]
                 , option [value "example_01_clean.json"] [text "example_01_clean.json"]
                 , option [value "example_01_conditional.json"] [text "example_01_conditional.json"]
                 , option [value "example_01_distributional.json"] [text "example_01_distributional.json"]
                 , option [value "example_02.json"] [text "example_02.json"]
                 , option [value "example_02_clean.json"] [text "example_02_clean.json"]
                 , option [value "example_03.json"] [text "example_03.json"]
                 , option [value "example_03_clean.json"] [text "example_03_clean.json"]
                 , option [value "imtNew.json"] [text "imtNew.json"]
                   -- , option [value "exemplePhyloBipartite.gexf"] [text "exemplePhyloBipartite.gexf"]
                 ]
               ]
               <>
               case st.sigmaGraphData of
                   Nothing -> []
                   Just gData ->
                     [ sigma { graph: gData
                             , renderer : canvas
                             , settings : mySettings
                             , style : sStyle { height : "95%"}
                             , onClickNode : \e -> unsafePerformEff $ do
                               log $ unsafeCoerce e
                               d $ SelectNode $ SelectedNode {id : (unsafeCoerce e).data.node.id, label : (unsafeCoerce e).data.node.label}
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
         , div [className "col-md-3", style {border : "1px black solid", backgroundColor : "beige"}]
             [ div [className "row"]
               [ div [_id "sidepanel" , className "col-md-12", style {borderBottom : "1px solid black"}]
               [ case st.selectedNode of
                    Nothing -> span [] []
                    Just selectedNode -> p [] [text $ "selected Node : " <> getter _.label selectedNode
                                              , br' []
                                              , p [] [button [className "btn btn-primary", style {marginBottom : "18px"}] [text "Remove"]]
                                              ]
               ]
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
             , div [className "col-md-12", _id "horizontal-checkbox"]
               [ ul [ style {display: "inline",float : "left" }]
                 [ li []
                   [ span [] [text "Pubs"]
                     ,input [ _type "checkbox"
                           , className "checkbox"
                           , checked $ true
                           , title "Mark as completed"
                             --  , onChange $ dispatch <<< ( const $ SetMap $ not (getter _._type state.term == MapTerm))
                           ] []

                   ]
                 , li []
                   [ span [] [text "Projects"]
                     ,input [ _type "checkbox"
                           , className "checkbox"
                           , checked $ false
                           , title "Mark as completed"
                             --  , onChange $ dispatch <<< ( const $ SetMap $ not (getter _._type state.term == MapTerm))
                           ] []
                   ]
                 , li []
                   [ span [] [text "Patents"]
                     ,input [ _type "checkbox"
                           , className "checkbox"
                           , checked $ false
                           , title "Mark as completed"
                             --  , onChange $ dispatch <<< ( const $ SetMap $ not (getter _._type state.term == MapTerm))
                           ] []
                   ]
                 , li []
                   [ span [] [text "Others"]
                     ,input [ _type "checkbox"
                           , className "checkbox"
                           , checked $ false
                           , title "Mark as completed"
                             --  , onChange $ dispatch <<< ( const $ SetMap $ not (getter _._type state.term == MapTerm))
                           ] []
                   ]
                 ]

               ]
              , div []
                [ p [] []
                , div [className "col-md-12"][ case st.selectedNode of
                    Nothing -> span [] []
                    Just selectedNode -> p [] [text $ "BOUKLI HACENE Ghouthi, GRIPON Vincent, FARRUGIA Nicolas, ARZEL Matthieu, ", a [href "http://localhost:2015/#/userPage/1"][text "JEZEQUEL Michel. "], text "Finding All Matches in a Database using Binary Neural Networks. COGNITIVE 2017 : The Ninth International Conference on Advanced Cognitive Technologies and Applications, 19-23 february 2017, AthÃ¨nes, Greece, 2017, pp. 59-64"]
                 , p []
                 [
                 ]
               ]
                ]
               ]
             ]
           ]
         ]
