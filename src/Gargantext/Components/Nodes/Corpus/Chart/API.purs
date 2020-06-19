module Gargantext.Components.Nodes.Corpus.Chart.API where

import Effect.Aff (Aff)

import Gargantext.Prelude
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, post)
import Gargantext.Types as T


recomputeChart :: Session -> T.ChartType -> T.CTabNgramType -> Int -> Int -> Aff (Array Int)
recomputeChart session chartType ngramType corpusId listId =
  post session (RecomputeListChart chartType ngramType corpusId listId) {}
