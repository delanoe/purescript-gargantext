module Gargantext.Components.Nodes.Corpus.Chart.API where

import Gargantext.Config.REST (AffRESTError)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, post)
import Gargantext.Types as T

recomputeChart :: Session -> T.ChartType -> T.CTabNgramType -> Int -> Int -> AffRESTError (Array Int)
recomputeChart session chartType ngramType corpusId listId =
  post session (RecomputeListChart chartType ngramType corpusId listId) {}
