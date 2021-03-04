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
import Gargantext.Utils.Reload as GUR

here = R2.here "Gargantext.Components.Nodes.Corpus.Chart.Utils"

reloadButtonWrap :: GUR.ReloadS -> R.Element -> R.Element
reloadButtonWrap setReload el = H.div {} [
    reloadButton setReload
  , el
  ]

reloadButton :: GUR.ReloadS -> R.Element
reloadButton reloadS = H.a { className
                           , on: { click: onClick }
                           , title: "Reload" } []
  where
    className = "reload-btn fa fa-refresh"
    onClick _ = GUR.bump reloadS


mNgramsTypeFromTabType :: T.TabType -> Maybe T.CTabNgramType
mNgramsTypeFromTabType (T.TabCorpus (T.TabNgramType ngramType))   = Just ngramType
mNgramsTypeFromTabType (T.TabCorpus _)                            = Nothing
mNgramsTypeFromTabType (T.TabDocument (T.TabNgramType ngramType)) = Just ngramType
mNgramsTypeFromTabType (T.TabDocument _)                          = Nothing
mNgramsTypeFromTabType (T.TabPairing _)                           = Nothing


type ChartUpdateButtonProps = (
    chartType :: T.ChartType
  , path :: Record Path
  , reload :: GUR.ReloadS
  , session :: Session
  )

chartUpdateButton :: Record ChartUpdateButtonProps -> R.Element
chartUpdateButton p = R.createElement chartUpdateButtonCpt p []

chartUpdateButtonCpt :: R.Component ChartUpdateButtonProps
chartUpdateButtonCpt = here.component "chartUpdateButton" cpt
  where
    cpt { chartType
        , path: { corpusId, listId, tabType }
        , reload
        , session } _ = do

      pure $ H.a { className: "chart-update-button fa fa-database"
                 , on: { click: onClick }
                 , title: "Update chart data" } []
      where
        onClick :: forall a. a -> Effect Unit
        onClick _ = do
          launchAff_ $ do
            case mNgramsTypeFromTabType tabType of
              Just ngramsType -> do
                _ <- recomputeChart session chartType ngramsType corpusId listId
                liftEffect $ GUR.bump reload
              Nothing -> pure unit
