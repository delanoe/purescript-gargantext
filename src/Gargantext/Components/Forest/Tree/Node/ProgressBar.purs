module Gargantext.Components.Forest.Tree.Node.ProgressBar where

import Gargantext.Prelude

import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Timer (clearInterval, setInterval)
import Gargantext.Components.Forest.Tree.Node.Action (ID)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get)
import Gargantext.Types as GT
import Partial.Unsafe (unsafePartial)

import Reactix as R
import Reactix.DOM.HTML as H


data BarType = Bar | Pie


type Props =
  (
    asyncTask :: GT.AsyncTaskWithType
  , barType   :: BarType
  , corpusId  :: ID
  , onFinish  :: Unit -> Effect Unit
  , session   :: Session
  )


asyncProgressBar :: Record Props -> R.Element
asyncProgressBar p = R.createElement asyncProgressBarCpt p []

asyncProgressBarCpt :: R.Component Props
asyncProgressBarCpt = R.hooksComponent "G.C.F.T.N.PB.asyncProgressBar" cpt
  where
    cpt props@{asyncTask: (GT.AsyncTaskWithType {task: GT.AsyncTask {id}}), barType, corpusId, onFinish} _ = do
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
    toInt n = unsafePartial $ fromJust $ fromNumber n

type ProgressIndicatorProps =
  (
    barType :: BarType
  , label :: String
  , progress :: Int
  )

progressIndicator :: Record ProgressIndicatorProps -> R.Element
progressIndicator p = R.createElement progressIndicatorCpt p []

progressIndicatorCpt :: R.Component ProgressIndicatorProps
progressIndicatorCpt = R.hooksComponent "G.C.F.T.N.PB.progressIndicator" cpt
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
queryProgress {asyncTask: GT.AsyncTaskWithType {task: GT.AsyncTask {id}, typ}, corpusId, session} = get session p
  where
    p = NodeAPI GT.Corpus (Just corpusId) $ path <> id <> "/poll?limit=1"
    path = GT.asyncTaskTypePath typ
