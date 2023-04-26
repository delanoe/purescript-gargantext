module Gargantext.Components.Nodes.Corpus.Chart.Utils where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude
import Gargantext.Components.Nodes.Corpus.Chart.API (recomputeChart)
import Gargantext.Components.Nodes.Corpus.Chart.Types (Path)
import Gargantext.Sessions (Session)
import Gargantext.Types as T
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Chart.Utils"

type ReloadButtonProps =
  ( reload :: T2.ReloadS )

reloadButton :: R2.Leaf ReloadButtonProps
reloadButton = R2.leaf reloadButtonCpt
reloadButtonCpt :: R.Component ReloadButtonProps
reloadButtonCpt = here.component "reloadButton" cpt where
  cpt { reload } _ = do
    pure $ H.a { className, on: { click }, title: "Reload" } []
    where
      className = "pr-1 fa fa-refresh"
      click _ = T2.reload reload


mNgramsTypeFromTabType :: T.TabType -> Maybe T.CTabNgramType
mNgramsTypeFromTabType (T.TabCorpus (T.TabNgramType ngramType))   = Just ngramType
mNgramsTypeFromTabType (T.TabDocument (T.TabNgramType ngramType)) = Just ngramType
mNgramsTypeFromTabType _                                          = Nothing


type ChartUpdateButtonProps =
  ( chartType :: T.ChartType
  , path      :: Record Path
  , reload    :: T2.ReloadS
  , session   :: Session
  )

chartUpdateButton :: R2.Leaf ChartUpdateButtonProps
chartUpdateButton = R2.leaf chartUpdateButtonCpt
chartUpdateButtonCpt :: R.Component ChartUpdateButtonProps
chartUpdateButtonCpt = here.component "chartUpdateButton" cpt where
  cpt {  path: { corpusId, listId, tabType }
      , reload, chartType, session } _ = do
    pure $ H.a { className, on: { click }, title: "Update chart data" } []
    where
      className = "chart-update-button fa fa-database"
      click :: forall a. a -> Effect Unit
      click _ = do
        launchAff_ $ do
          case mNgramsTypeFromTabType tabType of
            Just ngramsType -> do
              _ <- recomputeChart session chartType ngramsType corpusId listId
              liftEffect $ T2.reload reload
            Nothing -> pure unit
