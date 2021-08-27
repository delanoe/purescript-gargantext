module Gargantext.Components.Nodes.Corpus.Chart.API where

import Data.Either (Either)
import Effect.Aff (Aff)

import Gargantext.Config.REST (RESTError)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, post)
import Gargantext.Types as T

recomputeChart :: Session -> T.ChartType -> T.CTabNgramType -> Int -> Int -> Aff (Either RESTError (Array Int))
recomputeChart session chartType ngramType corpusId listId =
  post session (RecomputeListChart chartType ngramType corpusId listId) {}
