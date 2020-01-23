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


type Props =
  (
    asyncTask :: GT.AsyncTaskWithType
  , corpusId  :: ID
  , onFinish  :: Unit -> Effect Unit
  , session   :: Session
  )


asyncProgressBar :: Record Props -> R.Element
asyncProgressBar p = R.createElement asyncProgressBarCpt p []

asyncProgressBarCpt :: R.Component Props
asyncProgressBarCpt = R.hooksComponent "G.C.F.T.N.asyncProgressBar" cpt
  where
    cpt props@{asyncTask: (GT.AsyncTaskWithType {task: GT.AsyncTask {id}}), corpusId, onFinish} _ = do
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


      pure $
        H.div { className: "progress" } [
          H.div { className: "progress-bar"
                , role: "progressbar"
                , style: { width: (show $ toInt progress) <> "%" }
                } [ H.text id ]
        ]

    toInt :: Number -> Int
    toInt n = unsafePartial $ fromJust $ fromNumber n

queryProgress :: Record Props -> Aff GT.AsyncProgress
queryProgress {asyncTask: GT.AsyncTaskWithType {task: GT.AsyncTask {id}, typ}, corpusId, session} = get session p
  where
    p = NodeAPI GT.Corpus (Just corpusId) $ path <> id <> "/poll?limit=1"
    path = GT.asyncTaskTypePath typ
