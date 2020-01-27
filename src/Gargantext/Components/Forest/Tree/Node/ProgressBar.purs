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
import Gargantext.Types (AsyncProgress(..), AsyncTask(..), AsyncTaskStatus(..), NodeType(..), progressPercent)
import Partial.Unsafe (unsafePartial)
import Reactix as R
import Reactix.DOM.HTML as H


type Props =
  (
    asyncTask :: AsyncTask
  , corpusId  :: ID
  , onFinish  :: Unit -> Effect Unit
  , session   :: Session
  )


asyncProgressBar :: Record Props -> R.Element
asyncProgressBar p = R.createElement asyncProgressBarCpt p []

asyncProgressBarCpt :: R.Component Props
asyncProgressBarCpt = R.hooksComponent "G.C.F.T.N.asyncProgressBar" cpt
  where
    cpt props@{asyncTask: (AsyncTask {id}), corpusId, onFinish} _ = do
      (progress /\ setProgress) <- R.useState' 0.0
      intervalIdRef <- R.useRef Nothing

      R.useEffectOnce' $ do
        intervalId <- setInterval 1000 $ do
          launchAff_ $ do
            asyncProgress@(AsyncProgress {status}) <- queryProgress props
            liftEffect do
              setProgress \p -> min 100.0 $ progressPercent asyncProgress
              if (status == Finished) || (status == Killed) || (status == Failed) then do
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

queryProgress :: Record Props -> Aff AsyncProgress
queryProgress {asyncTask: AsyncTask {id}, corpusId, session} = get session p
  where
    p = NodeAPI Corpus (Just corpusId) $ "add/form/async/" <> id <> "/poll?limit=1"
