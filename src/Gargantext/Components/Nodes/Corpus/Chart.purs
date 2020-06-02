module Gargantext.Components.Nodes.Corpus.Chart where

import Reactix as R

import Gargantext.Components.Nodes.Corpus.Chart.Histo (histo)
import Gargantext.Components.Nodes.Corpus.Chart.Metrics (metrics)
import Gargantext.Components.Nodes.Corpus.Chart.Pie  (pie, bar)
import Gargantext.Components.Nodes.Corpus.Chart.Tree (tree)
import Gargantext.Components.Nodes.Corpus.Chart.Types (Path, Props)
import Gargantext.Types (ChartType(..))

getChartFunction :: ChartType -> (Record Props -> R.Element)
getChartFunction Histo = histo
getChartFunction ChartBar = bar
getChartFunction ChartPie = pie
getChartFunction Scatter = metrics
getChartFunction ChartTree = tree
