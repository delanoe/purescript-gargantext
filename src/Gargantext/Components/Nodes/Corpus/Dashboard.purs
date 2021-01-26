module Gargantext.Components.Nodes.Corpus.Dashboard where

import Gargantext.Components.Nodes.Types
import Gargantext.Prelude

import DOM.Simple.Console (log, log2)
import Data.Array as A
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.Nodes.Corpus (fieldsCodeEditor)
import Gargantext.Components.Nodes.Corpus.Chart.Predefined as P
import Gargantext.Components.Nodes.Dashboard.Types as DT
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Types (NodeID)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Reload as GUR
import Reactix as R
import Reactix.DOM.HTML as H

thisModule :: String
thisModule = "Gargantext.Components.Nodes.Corpus.Dashboard"

type Props =
  ( nodeId :: NodeID
  , session :: Session
  )

dashboardLayout :: R2.Component Props
dashboardLayout = R.createElement dashboardLayoutCpt
  where
    dashboardLayoutCpt :: R.Component Props
    dashboardLayoutCpt = R.hooksComponentWithModule thisModule "dashboardLayout" cpt

    cpt { nodeId, session } _ = do
      let sid = sessionId session

      pure $ dashboardLayoutWithKey { key: show sid <> "-" <> show nodeId, nodeId, session } []

type KeyProps = (
  key :: String
  | Props
  )

dashboardLayoutWithKey :: R2.Component KeyProps
dashboardLayoutWithKey = R.createElement dashboardLayoutWithKeyCpt
  where
    dashboardLayoutWithKeyCpt :: R.Component KeyProps
    dashboardLayoutWithKeyCpt = R.hooksComponentWithModule thisModule "dashboardLayoutWithKey" cpt

    cpt { nodeId, session } _ = do
      reload <- GUR.new

      useLoader {nodeId, reload: GUR.value reload, session} DT.loadDashboardWithReload $
        \dashboardData@{hyperdata: DT.Hyperdata h, parentId} -> do
          let { charts, fields } = h
          dashboardLayoutLoaded { charts
                                , corpusId: parentId
                                , defaultListId: 0
                                , fields
                                , key: show $ GUR.value reload
                                , nodeId
                                , onChange: onChange nodeId reload (DT.Hyperdata h)
                                , session } []
      where
        onChange :: NodeID -> GUR.ReloadS -> DT.Hyperdata -> { charts :: Array P.PredefinedChart
                                                         , fields :: List.List FTField } -> Effect Unit
        onChange nodeId' reload (DT.Hyperdata h) { charts, fields } = do
          launchAff_ do
            DT.saveDashboard { hyperdata: DT.Hyperdata $ h { charts = charts, fields = fields }
                             , nodeId:nodeId'
                             , session }
            liftEffect $ GUR.bump reload

type LoadedProps =
  ( charts :: Array P.PredefinedChart
  , corpusId :: NodeID
  , defaultListId :: Int
  , fields :: List.List FTField
  , key :: String
  , onChange :: { charts :: Array P.PredefinedChart
               , fields :: List.List FTField } -> Effect Unit
  | Props
  )

dashboardLayoutLoaded :: R2.Component LoadedProps
dashboardLayoutLoaded = R.createElement dashboardLayoutLoadedCpt
  where
    dashboardLayoutLoadedCpt :: R.Component LoadedProps
    dashboardLayoutLoadedCpt = R.hooksComponentWithModule thisModule "dashboardLayoutLoaded" cpt

    cpt props@{ charts, corpusId, defaultListId, fields, nodeId, onChange, session } _ = do
      pure $ H.div {}
        [ H.div { className: "row" }
          [ H.div { className: "col-12" }
            ([ H.h1 {} [ H.text "Board" ]
             , H.p {}  [ H.text "Summary of all your charts here" ]
             ] <> chartsEls <> [addNew])
          ]
        , dashboardCodeEditor { fields
                              , nodeId
                              , onChange: \fs -> onChange { charts, fields: fs }
                              , session } []
        ]
      where
        addNew = H.div { className: "row" } [
          H.span { className: "btn btn-secondary"
                 , on: { click: onClickAddChart }} [ H.span { className: "fa fa-plus" } [] ]
          ]
          where
            onClickAddChart _ = onChange { charts: A.cons P.CDocsHistogram charts
                                         , fields }
        chartsEls = A.mapWithIndex chartIdx charts
        chartIdx idx chart =
          renderChart { chart, corpusId, defaultListId, onChange: onChangeChart, onRemove, session } []
          where
            onChangeChart c = do
              onChange { charts: fromMaybe charts (A.modifyAt idx (\_ -> c) charts)
                       , fields }
            onRemove _ = onChange { charts: fromMaybe charts $ A.deleteAt idx charts
                                  , fields }

type CodeEditorProps =
  ( fields :: List.List FTField
  , onChange :: List.List FTField -> Effect Unit
  | Props
  )

dashboardCodeEditor :: R2.Component CodeEditorProps
dashboardCodeEditor = R.createElement el
  where
    el :: R.Component CodeEditorProps
    el = R.hooksComponentWithModule thisModule "dashboardCodeEditor" cpt

    cpt props@{ fields, nodeId, onChange, session } _ = do
      let fieldsWithIndex = List.mapWithIndex (\idx -> \t -> Tuple idx t) fields
      fieldsS <- R.useState' fieldsWithIndex
      fieldsRef <- R.useRef fields

      -- handle props change of fields
      R.useEffect1' fields $ do
        if R.readRef fieldsRef == fields then
          pure unit
        else do
          R.setRef fieldsRef fields
          snd fieldsS $ const fieldsWithIndex

      pure $ R.fragment
        [ H.div { className: "row" }
          [ H.div { className: "btn btn-secondary " <> (saveEnabled fieldsWithIndex fieldsS)
                  , on: { click: onClickSave fieldsS }
                  }
            [ H.span { className: "fa fa-floppy-o" } [  ]
            ]
          ]
        , H.div { className: "row" }
          [ H.div { className: "col-12" }
            [ fieldsCodeEditor { fields: fieldsS
                               , nodeId
                               , session} []
            ]
          ]
        , H.div { className: "row" }
          [ H.div { className: "btn btn-secondary"
                  , on: { click: onClickAddField fieldsS }
                  }
            [ H.span { className: "fa fa-plus" } [  ]
            ]
          ]
        ]
      where
        saveEnabled :: FTFieldsWithIndex -> R.State FTFieldsWithIndex -> String
        saveEnabled fs (fsS /\ _) = if fs == fsS then "disabled" else "enabled"

        onClickSave :: forall e. R.State FTFieldsWithIndex -> e -> Effect Unit
        onClickSave (fields /\ _) _ = do
          log "[dashboardCodeEditor] saving (TODO)"
          onChange $ snd <$> fields
          -- launchAff_ do
            -- saveCorpus $ { hyperdata: Hyperdata {fields: (\(Tuple _ f) -> f) <$> fieldsS}
            --             , nodeId
            --             , session }

        onClickAddField :: forall e. R.State FTFieldsWithIndex -> e -> Effect Unit
        onClickAddField (_ /\ setFieldsS) _ = do
          setFieldsS $ \fieldsS -> List.snoc fieldsS $ Tuple (List.length fieldsS) defaultField

type PredefinedChartProps =
  ( chart :: P.PredefinedChart
  , corpusId :: NodeID
  , defaultListId :: Int
  , onChange :: P.PredefinedChart -> Effect Unit
  , onRemove :: Unit -> Effect Unit
  , session :: Session
  )

renderChart :: R2.Component PredefinedChartProps
renderChart = R.createElement renderChartCpt
  where
    renderChartCpt :: R.Component PredefinedChartProps
    renderChartCpt = R.hooksComponentWithModule thisModule "renderChart" cpt

    cpt { chart, corpusId, defaultListId, onChange, onRemove, session } _ = do
      pure $ H.div { className: "row chart card" }
        [ H.div { className: "card-header" }
          [ H.div { className: "row" }
            [ H.div { className: "col-2" }
              [ R2.select { defaultValue: show chart
                          , on: { change: onSelectChange }
                          } (option <$> P.allPredefinedCharts)
              ]
            , H.div { className: "col-9" } []
            , H.div { className: "col-1" }
              [ H.span { className: "btn btn-danger"
                       , on: { click: onRemoveClick }} [ H.span { className: "fa fa-trash" } [] ]
              ]
            ]
          ]
        , H.div { className: "card-body" }
          [ H.div { className: "row" }
            [ H.div { className: "col-12 chart" }
              [ P.render chart params ]
            ]
          ]
        ]
      where
        option pc =
          H.option { value: show pc } [ H.text $ show pc ]
        onSelectChange e = onChange $ fromMaybe P.CDocsHistogram $ read value
          where
            value = R.unsafeEventValue e
        onRemoveClick _ = onRemove unit
        params = { corpusId
                 , limit: Just 1000
                 , listId: Just defaultListId
                 , session
                 }

    -- aSchool school = H.div {className: "col-md-4 content"} [ chart $ focus school ]
    -- schools = [ "Télécom Bretagne", "Mines Nantes", "Eurecom" ]
    -- myData =
    --   [seriesBarD1 {name: "Bar Data"}
    --    [ dataSerie {name: "val1", value: 50.0}
    --    , dataSerie {name: "val2", value: 70.0}
    --    , dataSerie {name: "val3", value: 80.0} ] ]
    -- focus :: String -> Options
    -- focus school =
    --   Options
    --   { mainTitle : "Focus " <> school
    --   , subTitle  : "Total scientific publications"
    --   , xAxis     : xAxis' ["2015", "2016", "2017"]
    --   , yAxis     : yAxis' { position: "left", show: false, min : 0 }
    --   , series    : myData
    --   , addZoom   : false
    --   , tooltip   : tooltipTriggerAxis } -- Necessary?

-----------------------------------------------------------------------------------------------------------

-- naturePublis_x :: Array String
-- naturePublis_x = ["Com","Articles","Thèses","Reports"]

-- naturePublis_y' :: Array Int
-- naturePublis_y' = [23901,17417,1188,1176]

-- naturePublis_y :: Array DataD1
-- naturePublis_y = zipWith (\n v -> dataSerie {name: n, value: toNumber v }) naturePublis_x naturePublis_y'

-- naturePublis :: Options
-- naturePublis = Options
--   { mainTitle : "Nature of publications"
--   , subTitle  : "Distribution by type"
--   , xAxis     : xAxis' []
--   , yAxis     : yAxis' { position: "left", show: false, min:0}
--   , series    : [seriesFunnelD1 { name: "Funnel Data" } naturePublis_y]
--   , addZoom   : false
--   , tooltip   : tooltipTriggerAxis -- Necessary?
--   }

-----------------------------------------------------------------------------------------------------------

-- globalPublis_x :: Array Int
-- globalPublis_x = [1982,1986,1987,1988,1990,1993,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017]
-- globalPublis_y :: Array Int
-- globalPublis_y = [1,4,2,1,1,2,1,1,8,38,234,76,40,82,75,202,1475,1092,1827,2630,4978,3668,4764,5915,4602,5269,6814,4018]


-- globalPublis :: Options
-- globalPublis = Options
--   { mainTitle : "Histogram"
--   , subTitle  : "Distribution of publications over time"
--   , xAxis     : xAxis' (map show globalPublis_x)
--   , yAxis     : yAxis' { position: "left", show: true, min:0}
--   , series    : [seriesBarD1 {name: "Number of publication / year"} $ map (\n -> dataSerie {name: "", value: toNumber n }) globalPublis_y]
--   , addZoom   : true
--   , tooltip   : tooltipTriggerAxis -- Necessary?
--   }



-- distriBySchool_y :: Array (Tuple String Int)
-- distriBySchool_y = [Tuple "Télécom Bretagne" 1150,Tuple "Télécom SudParis" 946,Tuple "Mines Nantes" 547,Tuple "Télécom ParisTech" 429,Tuple "IMT Atlantique" 205,Tuple "Mines Alès" 56
--                    ,Tuple "Télécom Ecole de Management" 52,Tuple "Mines Albi-Carmaux" 6]

-- distriBySchool :: Options
-- distriBySchool = Options
--   { mainTitle : "School production in 2017"
--   , subTitle  : "Distribution by school"
--   , xAxis     : xAxis' []
--   , yAxis     : yAxis' { position : "", show: false, min:0}
--   , series    : [ seriesPieD1 {name: "Pie data"} (map (\(Tuple n v) -> dataSerie {name: n, value: toNumber v}) distriBySchool_y)]
--   , addZoom   : false
--   , tooltip   : tooltipTriggerAxis -- Necessary?
--   }

-- scatterEx :: Options
-- scatterEx = Options
--   { mainTitle : "Scatter test"
--   , subTitle  : "Scatter subtitle"
--   , xAxis     : xAxis' []
--   , yAxis     : yAxis' { position: "", show: true, min:0}
--   , series    : [ seriesScatterD2 {name: "name1", symbolSize: 10.0} (dataSerieV <$> [[2.0,3.0],[3.0,4.0]])
--                 , seriesScatterD2 {name: "name2", symbolSize: 5.0 } (dataSerieV <$> [[1.0,3.0],[5.0,4.0]])
--                 , seriesScatterD2 {name: "name3", symbolSize: 10.0} (dataSerieV <$> [[10.0,3.0],[8.0,4.0]])
--                 ]
--   , addZoom   : false
--   , tooltip   : tooltipTriggerAxis -- Necessary?
--   }

-- sankeyEx :: Options
-- sankeyEx = Options
--   { mainTitle : ""
--   , subTitle  : ""
--   , xAxis     : xAxis' []
--   , yAxis     : yAxis' { position: "", show: false, min:0}
--   , series    :
--      [ seriesSankey
--          { "data":
--              [ {name : "a"}, {name : "b"}
--              , {name:"c"},   {name:"d"} ]
--          , links:
--              [ {source : "a", target : "b", value :2.0}
--              , {source : "a", target : "c", value :1.0}
--              , {source : "b", target : "c", value :1.0}
--              , {source : "b", target : "d", value :3.0}
--              ]
--          , layout: "none"
--          }
--      ]
--   , tooltip   : tooltipTriggerAxis -- Necessary?
--   , addZoom   : false
--   }

-- treeData :: Array TreeNode
-- treeData =
--   [ treeNode "nodeA" 10
--     [ treeNode "nodeAa" 4 []
--     , treeNode "nodeAb" 5 []
--     , treeNode "nodeAc" 1
--       [ treeNode "nodeAca" 5 []
--       , treeNode "nodeAcb" 5 [] ] ]
--   , treeNode "nodeB" 20
--     [ treeNode "nodeBa" 20
--       [ treeNode "nodeBa1" 20 [] ]]
--   , treeNode "nodeC" 20
--     [ treeNode "nodeCa" 20
--       [ treeNode "nodeCa1" 10 []
--       , treeNode "nodeCa2" 10 [] ]
--     , treeNode "nodeD" 20
--       [ treeNode "nodeDa" 20
--         [ treeNode "nodeDa1" 2 []
--         , treeNode "nodeDa2" 2 []
--         , treeNode "nodeDa3" 2 []
--         , treeNode "nodeDa4" 2 []
--         , treeNode "nodeDa5" 2 []
--         , treeNode "nodeDa6" 2 []
--         , treeNode "nodeDa7" 2 []
--         , treeNode "nodeDa8" 2 []
--         , treeNode "nodeDa9" 2 []
--         , treeNode "nodeDa10" 2 [] ]]]]

-- treeData' :: Array TreeNode
-- treeData' =
--   [ treeNode "nodeA" 10
--     [ treeLeaf "nodeAa" 4
--     , treeLeaf "nodeAb" 5
--     , treeNode "nodeAc" 1 [ treeLeaf "nodeAca" 5, treeLeaf "nodeAcb" 5 ]]
--   , treeNode "nodeB" 20 [ treeNode "nodeBa" 20 [ treeLeaf "nodeBa1" 20]]
--   , treeNode "nodeC" 20 [ treeNode "nodeBa" 20 [ treeLeaf "nodeBa1" 20]]
--   , treeNode "nodeD" 20 [ treeNode "nodeBa" 20 [ treeLeaf "nodeBa1" 20]]
--   , treeNode "nodeE" 20 [ treeNode "nodeBa" 20 [ treeLeaf "nodeBa1" 20]]
--   , treeNode "nodeF" 20 [ treeNode "nodeBa" 20 [ treeLeaf "nodeBa1" 20]]
--   , treeNode "nodeG" 20 [ treeNode "nodeBa" 20 [ treeLeaf "nodeBa1" 20]]
--   , treeNode "nodeH" 20 [ treeNode "nodeBa" 20 [ treeLeaf "nodeBa1" 20]]]

-- treeMapEx :: Options
-- treeMapEx = Options
--   { mainTitle : ""
--   , subTitle  : ""
--   , xAxis     : xAxis' []
--   , yAxis     : yAxis' { position: "", show: false, min:0}
--   , series    : [mkTree TreeMap treeData]
--   , addZoom   : false
--   , tooltip   : tooltipTriggerAxis -- Necessary?
--   }

-- treeEx :: Options
-- treeEx = Options
--   { mainTitle : "Tree"
--   , subTitle  : "Radial"
--   , xAxis     : xAxis' []
--   , yAxis     : yAxis' { position: "", show: false, min:0}
--   , series    : [mkTree TreeRadial treeData']
--   , addZoom   : false
--   , tooltip   : tooltipTriggerAxis -- Necessary?
--   }
