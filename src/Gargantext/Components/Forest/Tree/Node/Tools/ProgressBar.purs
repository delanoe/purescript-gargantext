module Gargantext.Components.Forest.Tree.Node.Tools.ProgressBar where

import Gargantext.Prelude

import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Timer (clearInterval, setInterval)
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Config.Utils (handleErrorInAsyncProgress, handleRESTError)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get)
import Gargantext.Types (FrontendError)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record.Extra as RX
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Tools.ProgressBar"


data BarType = Bar | Pie

type Props = (
    asyncTask :: GT.AsyncTaskWithType
  , barType   :: BarType
  , errors    :: T.Box (Array FrontendError)
  , nodeId    :: GT.ID
  , onFinish  :: Unit -> Effect Unit
  , session   :: Session
  )


asyncProgressBar :: R2.Component Props
asyncProgressBar = R.createElement asyncProgressBarCpt
asyncProgressBarCpt :: R.Component Props
asyncProgressBarCpt = here.component "asyncProgressBar" cpt
  where
    cpt props@{ asyncTask: (GT.AsyncTaskWithType {task: GT.AsyncTask {id}})
              , barType
              , errors
              , onFinish
              } _ = do
      progress <- T.useBox 0.0
      intervalIdRef <- R.useRef Nothing

      R.useEffectOnce' $ do
        intervalId <- setInterval 1000 $ do
          launchAff_ $ do
            let rdata = (RX.pick props :: Record QueryProgressData)
            eAsyncProgress <- queryProgress rdata
            handleRESTError here errors eAsyncProgress $ \asyncProgress -> liftEffect $ do
              let GT.AsyncProgress { status } = asyncProgress
              T.write_ (min 100.0 $ GT.progressPercent asyncProgress) progress
              if (status == GT.IsFinished) || (status == GT.IsKilled) || (status == GT.IsFailure) then do
                _ <- case R.readRef intervalIdRef of
                  Nothing -> pure unit
                  Just iid -> clearInterval iid
                handleErrorInAsyncProgress errors asyncProgress
                onFinish unit
              else
                pure unit

        R.setRef intervalIdRef $ Just intervalId

        pure unit


      pure $ progressIndicator { barType, label: id, progress }

--------------------------------------------------------------


type ProgressIndicatorProps =
  ( barType  :: BarType
  , label    :: String
  , progress :: T.Box Number
  )

progressIndicator :: Record ProgressIndicatorProps -> R.Element
progressIndicator p = R.createElement progressIndicatorCpt p []
progressIndicatorCpt :: R.Component ProgressIndicatorProps
progressIndicatorCpt = here.component "progressIndicator" cpt
  where
    cpt { barType, label, progress } _ = do
      progress' <- T.useLive T.unequal progress
      let progressInt = floor progress'

      case barType of
        Bar -> pure $
                H.div { className: "progress" }
                  [ H.div { className: "progress-bar"
                        , role: "progressbar"
                        , style: { width: (show $ progressInt) <> "%" }
                        } [ ]
                  ]
        Pie -> pure $
                H.div { className: "progress-pie" }
                  [ H.div { className: "progress-pie-segment"
                          , style: { "--over50": if progressInt < 50 then "0" else "1"
                                   , "--value": show $ progressInt } } [
                    ]
                  ]


--------------------------------------------------------------

type QueryProgressData =
  ( asyncTask :: GT.AsyncTaskWithType
  , nodeId    :: GT.ID
  , session   :: Session
  )

queryProgress :: Record QueryProgressData -> AffRESTError GT.AsyncProgress
queryProgress { asyncTask: GT.AsyncTaskWithType { task: GT.AsyncTask {id}
                                                , typ
                                                }
              , nodeId
              , session
              } = get session (p typ)
  where
    -- TODO refactor path
    p GT.ListCSVUpload      = NodeAPI GT.NodeList (Just nodeId) $ GT.asyncTaskTypePath GT.ListCSVUpload <> id <> "/poll?limit=1"
    p GT.UpdateNgramsCharts = NodeAPI GT.Node   (Just nodeId) $ path <> id <> "/poll?limit=1"
    p GT.UpdateNode         = NodeAPI GT.Node   (Just nodeId) $ path <> id <> "/poll?limit=1"
    p _                     = NodeAPI GT.Corpus (Just nodeId) $ path <> id <> "/poll?limit=1"
    path = GT.asyncTaskTypePath typ

    -- TODO wait route: take the result if failure then message
