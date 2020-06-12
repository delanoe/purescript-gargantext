module Gargantext.Components.Nodes.Corpus.Chart.Utils where

import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude
import Gargantext.Components.Nodes.Corpus.Chart.Types (Path)
import Gargantext.Sessions (Session)
import Gargantext.Types as T

reloadButtonWrap :: R.State Int -> R.Element -> R.Element
reloadButtonWrap setReload el = H.div {} [
    reloadButton setReload
  , el
  ]

reloadButton :: R.State Int -> R.Element
reloadButton (_ /\ setReload) = H.a { className
                                    , on: { click: onClick }
                                    , title: "Reload" } []
  where
    className = "reload-btn fa fa-refresh"
    onClick _ = setReload $ (_ + 1)


type ChartUpdateButtonProps = (
    chartType :: T.ChartType
  , path :: Record Path
  , reload :: R.State Int
  , session :: Session
  )

chartUpdateButton :: Record ChartUpdateButtonProps -> R.Element
chartUpdateButton p = R.createElement chartUpdateButtonCpt p []

chartUpdateButtonCpt :: R.Component ChartUpdateButtonProps
chartUpdateButtonCpt = R.hooksComponent "G.C.N.C.C.U.chartUpdateButton" cpt
  where
    cpt { path: { chartType, corpusId, listId, tabType }, reload: (_ /\ setReload), session } _ = do
      R.useEffect' $ do
        log2 "[chartUpdateButton] tabType" tabType

      pure $ H.a { className: "chart-update-button fa fa-database"
                 , on: { click: onClick }
                 , title: "Update chart data" } []
      where
        onClick :: forall a. a -> Effect Unit
        onClick _ = do
          setReload $ (_ + 1)
