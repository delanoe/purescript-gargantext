module Gargantext.Pages.Corpus.Graph where

import Effect.Unsafe (unsafePerformEffect)
import Gargantext.Prelude hiding (max,min)

import Control.Monad.Cont.Trans (lift)
import Data.Array (fold, length, (!!), null)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Int as Int
import Data.Lens (Lens', over, (%~), (.~), (^.))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Number as Num
import Data.Sequence as Seq
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Gargantext.Hooks.Sigmax.Types as Sigmax
import Gargantext.Hooks.Sigmax.Sigmajs (CameraProps, SigmaNode, cameras, getCameraProps, goTo, pauseForceAtlas2, sigmaOnMouseMove)
import Gargantext.Components.GraphExplorer.Types (Cluster(..), MetaData(..), Edge(..), GraphData(..), Legend(..), Node(..), getLegendData)
import Gargantext.Components.Login.Types (AuthData(..), TreeId)
import Gargantext.Components.RandomText (words)
import Gargantext.Components.Graph as Graph
import Gargantext.Components.Tree as Tree
import Gargantext.Config as Config
import Gargantext.Config.REST (get)
import Gargantext.Pages.Corpus.Graph.Tabs as GT
import Gargantext.Types (class Optional)
import Gargantext.Utils (toggleSet)
import Gargantext.Utils.Reactix (scuff)
import Partial.Unsafe (unsafePartial)
import React (ReactElement)
import React.DOM (button, div, input, li, li', menu, p, span, text, ul')
import React.DOM.Props (_id, _type, className, defaultValue, max, min, onChange, onClick, style, onMouseMove)
import React.SyntheticEvent (SyntheticUIEvent, target)
import Thermite (PerformAction, Render, Spec, _render, cmapProps, defaultPerformAction, defaultRender, modifyState, modifyState_, noState, simpleSpec, withState)
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
  | ToggleMultiNodeSelection
  | ChangeCursorSize Number
--  | Zoom Boolean

newtype SelectedNode = SelectedNode {id :: String, label :: String}

derive instance eqSelectedNode :: Eq SelectedNode
derive instance newtypeSelectedNode :: Newtype SelectedNode _
derive instance ordSelectedNode :: Ord SelectedNode

instance showSelectedNode :: Show SelectedNode where
  show (SelectedNode node) = node.label

_cursorSize :: forall s a. Lens' { cursorSize :: a | s } a
_cursorSize = prop (SProxy :: SProxy "cursorSize")

_multiNodeSelection :: forall s a. Lens' { multiNodeSelection :: a | s } a
_multiNodeSelection = prop (SProxy :: SProxy "multiNodeSelection")

-- _sigmaSettings :: forall s t a b. Lens { settings :: a | s } { settings :: b | t } a b
_sigmaSettings :: forall s a. Lens' { sigmaSettings :: a | s } a
_sigmaSettings = prop (SProxy :: SProxy "sigmaSettings")

_labelSizeRatio' :: forall s a. Lens' { labelSizeRatio :: a | s } a
_labelSizeRatio' = prop (SProxy :: SProxy "labelSizeRatio")

_labelSizeRatio :: Lens' {|Graph.SigmaSettings} Number
_labelSizeRatio f = unsafeCoerce $ _labelSizeRatio' f

_maxNodeSize' :: forall s a. Lens' { maxNodeSize :: a | s} a
_maxNodeSize' = prop (SProxy :: SProxy "maxNodeSize")

_maxNodeSize :: Lens' {|Graph.SigmaSettings} Number
_maxNodeSize f = unsafeCoerce $ _maxNodeSize' f

_minNodeSize' :: forall s a. Lens' { minNodeSize :: a | s} a
_minNodeSize' = prop (SProxy :: SProxy "minNodeSize")

_minNodeSize :: Lens' {|Graph.SigmaSettings} Number
_minNodeSize f = unsafeCoerce $ _minNodeSize' f

_drawEdges' :: forall s a. Lens' { drawEdges :: a | s} a
_drawEdges' = prop (SProxy :: SProxy "drawEdges")

_drawEdges :: Lens' {|Graph.SigmaSettings} Boolean
_drawEdges f = unsafeCoerce $ _drawEdges' f

numberTargetValue :: SyntheticUIEvent -> Number
numberTargetValue e =
  unsafePartial (fromJust (Num.fromString ((unsafeCoerce (unsafePerformEffect (target e))).value)))

-- TODO remove newtype here
newtype State = State
  { graphData :: GraphData
  , filePath :: String
  , sigmaGraphData :: Maybe Graph.Graph
  , legendData :: Array Legend
  , selectedNodes :: Set SelectedNode
  , cursorSize :: Number
  , multiNodeSelection :: Boolean
  , showSidePanel :: Boolean
  , showControls :: Boolean
  , showTree :: Boolean
  , corpusId :: Int
  , treeId :: Maybe TreeId
  , sigmaSettings :: {|Graph.SigmaSettings}
  }

derive instance newtypeState :: Newtype State _

initialState :: State
initialState = State
  { graphData : GraphData {nodes: [], edges: [], sides: [], metaData : Just $ MetaData{title : "", legend : [], corpusId : [], listId : 0}}
  , filePath : ""
  , sigmaGraphData : Nothing
  , legendData : []
  , selectedNodes : Set.empty
  , cursorSize : 0.0
  , multiNodeSelection : false
  , showSidePanel : false
  , showControls : false
  , showTree : false
  , corpusId : 0
  , treeId : Nothing
  , sigmaSettings : Graph.sigmaSettings
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

performAction (SelectNode selectedNode@(SelectedNode node)) _ (State state) =
  modifyState_ $ \(State s) ->
    State s {selectedNodes = toggleSet selectedNode
                              (if s.multiNodeSelection then s.selectedNodes
                                                       else Set.empty) }

performAction (ShowSidePanel b) _ (State state) = void do
  modifyState $ \(State s) -> State s {showSidePanel = b }


performAction (ToggleControls) _ (State state) = void do
  modifyState $ \(State s) -> State s {showControls = not (state.showControls) }

performAction (ToggleTree) _ (State state) = void do
  modifyState $ \(State s) -> State s {showTree = not (state.showTree) }

performAction (ChangeLabelSize size) _ _ =
  modifyState_ $ \(State s) ->
    State $ ((_sigmaSettings <<< _labelSizeRatio) .~ size) s

performAction (ChangeNodeSize size) _ _ =
  modifyState_ $ \(State s) ->
    s # _sigmaSettings <<< _maxNodeSize .~ (size * 10.0)
      # _sigmaSettings <<< _minNodeSize .~ size
      # State

performAction DisplayEdges _ _ =
  modifyState_ $ \(State s) -> do
    State $ ((_sigmaSettings <<< _drawEdges) %~ not) s

performAction ToggleMultiNodeSelection _ _ =
  modifyState_ $ \(State s) -> do
    State $ s # _multiNodeSelection %~ not

performAction (ChangeCursorSize size) _ _ =
  modifyState_ $ \(State s) ->
    State $ s # _cursorSize .~ size


--performAction (Zoom True) _ _ =
--  modifyState_ $ \() -> do
--    State $


convert :: GraphData -> Graph.Graph
convert (GraphData r) = Sigmax.Graph {nodes, edges}
  where
    nodes = foldMapWithIndex nodeFn r.nodes
    nodeFn i (Node n) =
      Seq.singleton
        { id    : n.id_
        , size  : toNumber n.size
        , label : n.label
        , x     : n.x -- cos (toNumber i)
        , y     : n.y -- sin (toNumber i)
        , color : intColor (cDef n.attributes)
        }
      where
        cDef (Cluster {clustDefault}) = clustDefault
    edges = foldMap edgeFn r.edges
    edgeFn (Edge e) = Seq.singleton {id : e.id_, source : e.source, target : e.target}
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




defaultPalette :: Array String
defaultPalette = ["#5fa571","#ab9ba2","#da876d","#bdd3ff","#b399df","#ffdfed","#33c8f3","#739e9a","#caeca3","#f6f7e5","#f9bcca","#ccb069","#c9ffde","#c58683","#6c9eb0","#ffd3cf","#ccffc7","#52a1b0","#d2ecff","#99fffe","#9295ae","#5ea38b","#fff0b3","#d99e68"]

-- clusterColor :: Cluster -> Color
-- clusterColor (Cluster {clustDefault}) = unsafePartial $ fromJust $ defaultPalette !! (clustDefault `mod` length defaultPalette)


intColor :: Int -> String
intColor i = unsafePartial $ fromJust $ defaultPalette !! (i `mod` length defaultPalette)

-- modCamera0 :: forall o. Optional o CameraProps =>
--               (Record CameraProps -> Record o) -> Effect Unit
-- modCamera0 f = do
--   s <- getSigmaRef
--   for_ (cameras s !! 0) $ \cam ->
--     void $ goTo cam (f $ getCameraProps cam)

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

                [ div [ className "col-md-2", _id "graph-tree", style {marginTop: "65px"}] $
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
    render' d _ (State st@{sigmaSettings, graphData: GraphData {sides,metaData  }}) _ =
      [ div [className "container-fluid", style {"padding-top" : "90px" }]
      [ {-div [ className "row"]
        [ h2 [ style {textAlign : "center", position : "relative", top: "-1px"}]
          [-- :  MetaData {title}
            case metaData of
              Just( MetaData {title }) ->
                text $ "Graph " <> title
              Nothing ->
                text "Title"
          ]
        ]
        , -} div [className "row", style {"padding-bottom" : "10px", marginTop : "-24px"}]
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
                --  li' [ button [className "btn btn-success btn-sm"] [text "Change Type"] ]
                -- ,
                  li'
                  [ button [
                         className "btn btn-primary btn-sm"
                       , onClick \_ -> d DisplayEdges
                           ]
                    [text "Toggle Edges"]
                  ]
                -- , li' [ button [className "btn btn-primary btn-sm"] [text "Change Level"] ]
                {- ,li [style {display : "inline-block"}]
                  [ form'
                    [ input [_type "file"
                            , name "file"
                         --   , onChange (\e -> d $ SetFile (getFile e) (unsafeCoerce $ d <<< SetProgress))
                            , className "btn btn-primary"]

                    -- , text $ show st.readyState
                    ]
                  ]
                -}
                {-, li' [ input [_type "button"
                              , className "btn btn-warning btn-sm"
                              ,value "Run Demo"
                            --  , onClick \_ -> d SetGraph, disabled (st.readyState /= DONE)
                              ]
                      ]
                      -}
                {-, li'
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
                          , input [_type "text", className "form-control", placeholder "select topics"]
                        ]
                      ]

                    ]
                  ]
                -}
                , li [className "col-md-1"]
                  [ span [] [text "Selector"]
                  , input [ _type "range"
                          , _id "cursorSizeRange"
                          , min "0"
                          , max "100"
                          , defaultValue (show st.cursorSize)
                          , onChange \e -> d $ ChangeCursorSize (numberTargetValue e)
                          ]
                  ]
                , li [className "col-md-1"]
                  [ span [] [text "Labels"],input [_type "range"
                                                 , _id "labelSizeRange"
                                                 , max "4"
                                                 , defaultValue <<< show $ sigmaSettings ^. _labelSizeRatio
                                                 , min "1"
                                                 , onChange \e -> d $ ChangeLabelSize (numberTargetValue e)
                                                 ]
                  ]

                , li [className "col-md-1"]
                  [ span [] [text "Nodes"],input [_type "range"
                                                 , _id "nodeSizeRange"
                                                 , max "15"
                                                 , defaultValue <<< show $ sigmaSettings ^. _minNodeSize
                                                 , min "5"
                                                 , onChange \e -> d $ ChangeNodeSize (numberTargetValue e)
                                                 ]
                  ]
                {-, li [className "col-md-2"]
                  [ span [] [text "Edges"],input [_type "range", _id "myRange", value "90"]
                  ]
                -}
                -- , li'
                  -- [ button [ className "btn btn-primary"
                  --          , onClick \_ -> modCamera0 (const {x: 0.0, y: 0.0, ratio: 1.0})
                  --          ] [text "Center"]
                  -- ]
                -- , li [className "col-md-1"]
                --   [ span [] [text "Zoom"],input [ _type "range"
                --                                 , _id "cameraRatio"
                --                                 , max "100"
                --                                 , defaultValue "0"
                --                                 , min "0"
                --                                 , onChange \e -> do
                --                                     let ratio = (100.0 - numberTargetValue e) / 100.0pa
                --                                     modCamera0 (const {ratio})
                --                                 ]
                --   ]
                , li [className "col-md-1"]
                  [ span [] [text "MultiNode"]
                  , input
                    [ _type "checkbox"
                    , className "checkbox"
                    -- , checked
                    , onChange $ const $ d ToggleMultiNodeSelection
                    ]
                  ]
                , li'
                  [ button [ className "btn btn-primary"
                           , onClick \_ -> pauseForceAtlas2
                           ] [text "Spatialization"]
                  ]
                {-, li'
                  [ button [className "btn btn-primary"
                            , onClick \_ -> do
                                             _ <- log "Hey there" -- $ show st.camera
                                             pure unit
                           ] [text "Save"] -- TODO: Implement Save!
                  ]
                -}
                ]
              ]
            ]
            else div [] []
         ]
         , div [className "row"]
           [div [if (st.showSidePanel && st.showTree) then className "col-md-10" else if (st.showSidePanel || st.showTree) then className "col-md-10" else className "col-md-12"]
             [ div [style {height: "95%"}
                   ,onMouseMove (sigmaOnMouseMove {cursorSize: st.cursorSize})] $
               [
               ]
               <>
               case st.sigmaGraphData of
                   Nothing -> []
                   Just graph ->
                     let forceAtlas2Settings = Graph.forceAtlas2Settings in
                     let opts = { graph, sigmaSettings, forceAtlas2Settings } in
                     [ scuff $ Graph.graph opts ]
                                          
                     -- [ sigma { graph, settings
                     --         , style : sStyle { height : "95%"}
                     --         , onClickNode : \e ->
                     --         unsafePerformEffect $ do
                     --           _ <- d $ ShowSidePanel true
                     --           let {id, label} = (unsafeCoerce e).data.node
                     --           _ <- d $ SelectNode $ SelectedNode {id, label}
                     --           pure unit
                     -- ]
                 <>
                 if length st.legendData > 0 then [div [style {position : "absolute", bottom : "10px", border: "1px solid black", boxShadow : "rgb(0, 0, 0) 0px 2px 6px", marginLeft : "10px", padding:  "16px"}] [dispLegend st.legendData]] else []
             ]
         --, button [onClick \_ -> d ShowSidePanel, className "btn btn-primary", style {right:"39px",position : "relative",zIndex:"1000", top: "-59px"}] [text "Show SidePanel"]
         , if (st.showSidePanel) then
            div [_id "sp-container", className "col-md-2", style {border : "1px white solid", backgroundColor : "white"}]
             [ div [className "row"] $
--             , div [className "col-md-12"]
--               [a
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
                  [ let query = (\(SelectedNode {label}) -> words label) <$> Set.toUnfoldable st.selectedNodes in
                    if null query then
                      p [] []
                    else
                      GT.tabsElt {query, sides}
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
