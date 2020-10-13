module Gargantext.Components.Forest.Tree.Node.Tools.ProgressBar where

import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Timer (clearInterval, setInterval)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.Forest.Tree.Node.Tools.ProgressBar"


data BarType = Bar | Pie

type Props =
  ( asyncTask :: GT.AsyncTaskWithType
  , barType   :: BarType
  , corpusId  :: GT.ID
  , onFinish  :: Unit -> Effect Unit
  , session   :: Session
  )


asyncProgressBar :: Record Props -> R.Element
asyncProgressBar p = R.createElement asyncProgressBarCpt p []

asyncProgressBarCpt :: R.Component Props
asyncProgressBarCpt = R.hooksComponentWithModule thisModule "asyncProgressBar" cpt
  where
    cpt props@{ asyncTask: (GT.AsyncTaskWithType {task: GT.AsyncTask {id}})
              , barType
              , corpusId
              , onFinish
              } _ = do
      (progress /\ setProgress) <- R.useState' 0.0
      intervalIdRef <- R.useRef Nothing

      R.useEffectOnce' $ do
        intervalId <- setInterval 1000 $ do
          launchAff_ $ do
            asyncProgress@(GT.AsyncProgress {status}) <- queryProgress props
            liftEffect do
              setProgress \p -> min 100.0 $ GT.progressPercent asyncProgress
              if (status == GT.Finished) || (status == GT.Killed) || (status == GT.Failed) then do
                _ <- case R.readRef intervalIdRef of
                  Nothing -> pure unit
                  Just iid -> clearInterval iid
                onFinish unit
              else
                pure unit

        R.setRef intervalIdRef $ Just intervalId

        pure unit


      pure $ progressIndicator { barType, label: id, progress: toInt progress }

    toInt :: Number -> Int
    toInt n = case fromNumber n of
        Nothing -> 0
        Just x  -> x

type ProgressIndicatorProps =
  ( barType  :: BarType
  , label    :: String
  , progress :: Int
  )

progressIndicator :: Record ProgressIndicatorProps -> R.Element
progressIndicator p = R.createElement progressIndicatorCpt p []

progressIndicatorCpt :: R.Component ProgressIndicatorProps
progressIndicatorCpt = R.hooksComponentWithModule thisModule "progressIndicator" cpt
  where
    cpt { barType: Bar, label, progress } _ = do
      pure $
        H.div { className: "progress" } [
          H.div { className: "progress-bar"
                , role: "progressbar"
                , style: { width: (show $ progress) <> "%" }
                } [ H.text label ]
        ]

    cpt { barType: Pie, label, progress } _ = do
      pure $
        H.div { className: "progress-pie" } [
          H.div { className: "progress-pie-segment"
                , style: { "--over50": if progress < 50 then "0" else "1"
                         , "--value": show $ progress } } [
          ]
        ]

queryProgress :: Record Props -> Aff GT.AsyncProgress
queryProgress { asyncTask: GT.AsyncTaskWithType { task: GT.AsyncTask {id}
                                                , typ
                                                }
              , corpusId
              , session
              } = get session (p typ)
  where
    -- TODO refactor path
    p GT.UpdateNode = NodeAPI GT.Node   (Just corpusId) $ path <> id <> "/poll?limit=1"
    p _             = NodeAPI GT.Corpus (Just corpusId) $ path <> id <> "/poll?limit=1"
    path = GT.asyncTaskTypePath typ

    -- TODO wait route: take the result if failure then message
