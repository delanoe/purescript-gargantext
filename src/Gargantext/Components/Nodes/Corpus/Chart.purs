module Gargantext.Components.Nodes.Corpus.Chart where

import Data.Maybe (Maybe(..))
import Reactix as R

import Gargantext.Components.Nodes.Corpus.Chart.Histo (histo)
import Gargantext.Components.Nodes.Corpus.Chart.Metrics (metrics)
import Gargantext.Components.Nodes.Corpus.Chart.Pie  (pie, bar)
import Gargantext.Components.Nodes.Corpus.Chart.Tree (tree)
import Gargantext.Components.Nodes.Corpus.Chart.Types (Path, ListPath, Props)
import Gargantext.Types (ChartType(..))

getChartFunction :: ChartType -> Maybe (Record (Props Path) -> R.Element)
getChartFunction Histo = Just histo
getChartFunction ChartBar = Just bar
getChartFunction ChartPie = Just pie
getChartFunction _ = Nothing

getChartFunctionWithList :: ChartType -> Maybe (Record (Props ListPath) -> R.Element)
getChartFunctionWithList Scatter = Just metrics
getChartFunctionWithList ChartTree = Just tree
getChartFunctionWithList _ = Nothing
