module Gargantext.Components.Nodes.Corpus.Chart.Common where

import Data.Tuple (fst, Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R

import Gargantext.Prelude

import Gargantext.Components.Nodes.Corpus.Chart.Types
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Sessions (Session)

type MetricsLoadViewProps a = (
    getMetrics :: Session -> Tuple Reload (Record Path) -> Aff a
  , loaded :: Session -> Record Path -> R.State Reload -> a -> R.Element
  | MetricsProps
  )

metricsLoadView :: forall a. Record (MetricsLoadViewProps a) -> R.Element
metricsLoadView p = R.createElement metricsLoadViewCpt p []

metricsLoadViewCpt :: forall a. R.Component (MetricsLoadViewProps a)
metricsLoadViewCpt = R.hooksComponent "G.C.N.C.C.metricsLoadView" cpt
  where
    cpt {getMetrics, loaded, path, reload, session} _ = do
      useLoader (fst reload /\ path) (getMetrics session) $ \l ->
        loaded session path reload l
