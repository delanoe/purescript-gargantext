module Gargantext.Context.Progress
  ( AsyncProps
  , asyncProgress
  , asyncContext
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Timer (IntervalId, clearInterval, setInterval)
import Gargantext.Components.Forest.Tree.Node.Tools.ProgressBar (QueryProgressData, queryProgress)
import Gargantext.Config.Utils (handleErrorInAsyncProgress, handleRESTError)
import Gargantext.Hooks.FirstEffect (useFirstEffect')
import Gargantext.Sessions (Session)
import Gargantext.Types (AsyncProgress, FrontendError)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Record.Extra as RX
import Toestand as T

type AsyncProps =
  ( asyncTask :: GT.AsyncTaskWithType
  , errors    :: T.Box (Array FrontendError)
  , nodeId    :: GT.ID
  , onFinish  :: Unit -> Effect Unit
  , session   :: Session
  )

asyncProgress :: R2.Component AsyncProps
asyncProgress = R2.component component
component :: R.Component AsyncProps
component = R.hooksComponent "asyncProgressContext" cpt where
  cpt props@{ errors
            , onFinish
            } children = do
    -- States
    progress /\ progressBox <- R2.useBox' 0.0
    intervalIdRef <- R.useRef (Nothing :: Maybe IntervalId)

    -- Methods
    let
      exec :: Unit -> Effect Unit
      exec _ = launchAff_ do
        let rdata = (RX.pick props :: Record QueryProgressData)

        eAsyncProgress <- queryProgress rdata
        handleRESTError errors eAsyncProgress onProgress

      onProgress :: AsyncProgress -> Aff Unit
      onProgress value = liftEffect do
        let GT.AsyncProgress { status } = value

        T.write_ (min 100.0 $ GT.progressPercent value) progressBox

        if (status == GT.IsFinished) ||
           (status == GT.IsKilled) ||
           (status == GT.IsFailure)
        then do
          case R.readRef intervalIdRef of
            Nothing  -> R.nothing
            Just iid -> clearInterval iid
          handleErrorInAsyncProgress errors value
          onFinish unit
        else
          R.nothing

    -- Hooks
    useFirstEffect' do
      intervalId <- setInterval 1000 $ exec unit
      R.setRef intervalIdRef $ Just intervalId

    -- Render
    pure $

      R.provideContext asyncContext (progress)
      children

asyncContext :: R.Context (Number)
asyncContext = R.createContext 0.0
