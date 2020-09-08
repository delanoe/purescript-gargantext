module Gargantext.Components.Nodes.Corpus.Chart.Utils where

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn1)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude
import Gargantext.Components.Nodes.Corpus.Chart.API (recomputeChart)
import Gargantext.Components.Nodes.Corpus.Chart.Types (Path)
import Gargantext.Sessions (Session)
import Gargantext.Types as T
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.Nodes.Corpus.Chart.Utils"

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
chartUpdateButtonCpt = R2.hooksComponent thisModule "chartUpdateButton" cpt
  where
    cpt { chartType
        , path: { corpusId, listId, tabType }
        , reload: (_ /\ setReload), session } _ = do

      pure $ H.a { className: "chart-update-button fa fa-database"
                 , on: { click: onClick }
                 , title: "Update chart data" } []
      where
        onClick :: forall a. a -> Effect Unit
        onClick _ = do
          launchAff_ $ do
            case mNgramsType of
              Just ngramsType -> do
                _ <- recomputeChart session chartType ngramsType corpusId listId
                liftEffect $ setReload $ (_ + 1)
              Nothing -> pure unit

        mNgramsType = case tabType of
            T.TabCorpus (T.TabNgramType ngramType)   -> Just ngramType
            T.TabCorpus _                            -> Nothing
            T.TabDocument (T.TabNgramType ngramType) -> Just ngramType
            T.TabDocument _                          -> Nothing
            T.TabPairing _                           -> Nothing
