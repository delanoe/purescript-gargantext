module Gargantext.Components.Nodes.Corpus.Dashboard where

import Data.Array as A
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.Nodes.Corpus (fieldsCodeEditor)
import Gargantext.Components.Nodes.Corpus.Chart.Predefined as P
import Gargantext.Components.Nodes.Dashboard.Types as DT
import Gargantext.Components.Nodes.Types (FTFieldList(..), FTFieldsWithIndex(..), defaultField)
import Gargantext.Config.REST (logRESTError)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude (Unit, bind, discard, pure, read, show, unit, ($), (<$>), (<>), (==))
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Types (NodeID)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Dashboard"

type Props =
  ( boxes   :: Boxes
  , nodeId  :: NodeID
  , session :: Session )

dashboardLayout :: R2.Component Props
dashboardLayout = R.createElement dashboardLayoutCpt
dashboardLayoutCpt :: R.Component Props
dashboardLayoutCpt = here.component "dashboardLayout" cpt where
  cpt props@{ nodeId, session } content = do
    pure $ dashboardLayoutWithKey (Record.merge props { key }) content
      where
        key = show (sessionId session) <> "-" <> show nodeId

type KeyProps =
  ( key     :: String
  | Props
  )

dashboardLayoutWithKey :: R2.Component KeyProps
dashboardLayoutWithKey = R.createElement dashboardLayoutWithKeyCpt
dashboardLayoutWithKeyCpt :: R.Component KeyProps
dashboardLayoutWithKeyCpt = here.component "dashboardLayoutWithKey" cpt
  where
    cpt { boxes, nodeId, session } _ = do
      reload <- T.useBox T2.newReload
      reload' <- T.useLive T.unequal reload

      useLoader { errorHandler
                , loader: DT.loadDashboardWithReload
                , path: { nodeId, reload: reload', session }
                , render: \(DT.DashboardData { hyperdata: DT.Hyperdata h, parentId }) -> do
                      let { charts, fields } = h
                      dashboardLayoutLoaded { boxes
                                            , charts
                                            , corpusId: parentId
                                            , defaultListId: 0
                                            , fields
                                            , nodeId
                                            , onChange: onChange nodeId reload (DT.Hyperdata h)
                                            , session } [] }
      where
        errorHandler = logRESTError here "[dashboardLayoutWithKey]"
        onChange :: NodeID -> T2.ReloadS -> DT.Hyperdata -> { charts :: Array P.PredefinedChart
                                                            , fields :: FTFieldList } -> Effect Unit
        onChange nodeId' reload (DT.Hyperdata h) { charts, fields } = do
          launchAff_ do
            res <- DT.saveDashboard { hyperdata: DT.Hyperdata $ h { charts = charts, fields = fields }
                                    , nodeId:nodeId'
                                    , session }
            liftEffect $ do
              _ <- case res of
                Left err -> here.warn2 "[dashboardLayoutWithKey] onChange RESTError" err
                _ -> pure unit
              T2.reload reload

type LoadedProps =
  ( boxes         :: Boxes
  , charts        :: Array P.PredefinedChart
  , corpusId      :: NodeID
  , defaultListId :: Int
  , fields        :: FTFieldList
  , onChange      :: { charts :: Array P.PredefinedChart
                  , fields :: FTFieldList } -> Effect Unit
  , nodeId        :: NodeID
  , session       :: Session
  )

dashboardLayoutLoaded :: R2.Component LoadedProps
dashboardLayoutLoaded = R.createElement dashboardLayoutLoadedCpt
dashboardLayoutLoadedCpt :: R.Component LoadedProps
dashboardLayoutLoadedCpt = here.component "dashboardLayoutLoaded" cpt
  where
    cpt { boxes
        , charts
        , corpusId
        , defaultListId
        , fields
        , nodeId
        , onChange
        , session } _ = do
      pure $ H.div {}
        [ dashboardCodeEditor { fields
                              , nodeId
                              , onChange: \fs -> onChange { charts, fields: fs }
                              , session } []
        ,  H.div { className: "row" }
          [ H.div { className: "col-12" }
            ([ H.h1 {} [ H.text "Board" ]
             , H.p {}  [ H.text "Summary of all your charts here" ]
             ] <> chartsEls <> [addNew])
          ]
        ]
      where

        addNew = H.div { className: "row" } [
          H.span { className: "btn btn-primary"
                 , on: { click: onClickAddChart }} [ H.span { className: "fa fa-plus" } [] ]
          ]
          where
            onClickAddChart _ = onChange { charts: A.cons P.CDocsHistogram charts
                                         , fields }
        chartsEls = mapWithIndex chartIdx charts
        chartIdx idx chart =
          renderChart { boxes
                      , chart
                      , corpusId
                      , defaultListId
                      , onChange: onChangeChart
                      , onRemove
                      , session } []
          where
            onChangeChart c = do
              onChange { charts: fromMaybe charts (A.modifyAt idx (\_ -> c) charts)
                       , fields }
            onRemove _ = onChange { charts: fromMaybe charts $ A.deleteAt idx charts
                                  , fields }

type CodeEditorProps =
  ( fields   :: FTFieldList
  , onChange :: FTFieldList -> Effect Unit
  , nodeId   :: NodeID
  , session  :: Session
  )

dashboardCodeEditor :: R2.Component CodeEditorProps
dashboardCodeEditor = R.createElement dashboardCodeEditorCpt
dashboardCodeEditorCpt :: R.Component CodeEditorProps
dashboardCodeEditorCpt = here.component "dashboardCodeEditor" cpt
  where
    cpt { fields: FTFieldList fields, nodeId, onChange, session } _ = do
      let fieldsWithIndex = FTFieldsWithIndex $ mapWithIndex (\idx -> \ftField -> { idx, ftField }) fields
      fieldsS <- T.useBox fieldsWithIndex
      fields' <- T.useLive T.unequal fieldsS
      fieldsRef <- R.useRef fields'

      -- handle props change of fields
      R.useEffect1' fields $ do
        if R.readRef fieldsRef == fields' then
          pure unit
        else do
          R.setRef fieldsRef fields'
          T.write_ fieldsWithIndex fieldsS

      pure $ R.fragment
        [ H.div { className: "row" }
          [ H.div { className: "btn btn-primary " <> (saveEnabled fieldsWithIndex fields')
                  , on: { click: onClickSave fields' }
                  }
            [ H.span { className: "fa fa-floppy-o" } [  ]
            ]
          ]
        , H.div { className: "row" }
          [ H.div { className: "col-12" }
            [ fieldsCodeEditor { fields: fieldsS
                               , nodeId
                               , session } []
            ]
          ]
        , H.div { className: "row" }
          [ H.div { className: "btn btn-primary"
                  , on: { click: onClickAddField fieldsS }
                  }
            [ H.span { className: "fa fa-plus" } [  ]
            ]
          ]
        ]
      where
        saveEnabled :: FTFieldsWithIndex -> FTFieldsWithIndex -> String
        saveEnabled fs fsS = if fs == fsS then "disabled" else "enabled"

        onClickSave :: forall e. FTFieldsWithIndex -> e -> Effect Unit
        onClickSave (FTFieldsWithIndex fields') _ = do
          onChange $ FTFieldList $ (_.ftField) <$> fields'

        onClickAddField :: forall e. T.Box FTFieldsWithIndex -> e -> Effect Unit
        onClickAddField fieldsS _ = do
          T.modify_ (\(FTFieldsWithIndex fs) -> FTFieldsWithIndex $
            List.snoc fs $ { idx: List.length fs, ftField: defaultField }) fieldsS

type PredefinedChartProps =
  ( boxes         :: Boxes
  , chart         :: P.PredefinedChart
  , corpusId      :: NodeID
  , defaultListId :: Int
  , onChange      :: P.PredefinedChart -> Effect Unit
  , onRemove      :: Unit -> Effect Unit
  , session       :: Session
  )

renderChart :: R2.Component PredefinedChartProps
renderChart = R.createElement renderChartCpt
renderChartCpt :: R.Component PredefinedChartProps
renderChartCpt = here.component "renderChart" cpt
  where
    cpt { boxes
        , chart
        , corpusId
        , defaultListId
        , onChange
        , onRemove
        , session } _ = do
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
        params = { boxes
                 , corpusId
                 , limit: Just 1000
                 , listId: Just defaultListId
                 , onClick: Nothing
                 , onInit: Nothing
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
