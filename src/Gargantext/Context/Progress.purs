module Gargantext.Context.Progress
  ( AsyncProps
  , asyncProgress
  , asyncContext
  ) where

import Gargantext.Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Timer (IntervalId, clearInterval, setInterval)
import Gargantext.Components.App.Store as AppStore
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
  , nodeId    :: GT.ID
  , onFinish  :: Unit -> Effect Unit
  , session   :: Session
  )

here :: R2.Here
here = R2.here "Gargantext.Context.Progress"

asyncProgress :: R2.Component AsyncProps
asyncProgress = R2.component asyncProgressCpt
asyncProgressCpt :: R.Component AsyncProps
asyncProgressCpt = R.hooksComponent "asyncProgress" cpt where
  cpt props@{ onFinish
            } children = do
    { errors } <- AppStore.use
    
    -- States
    progress /\ progressBox <- R2.useBox' 0.0
    intervalIdRef <- R.useRef (Nothing :: Maybe IntervalId)
    -- exponential backoff is used when errors are reported
    interval <- T.useBox 1000

    -- Methods
    let
      exec :: Unit -> Effect Unit
      exec _ = launchAff_ do
        let rdata = (RX.pick props :: Record QueryProgressData)

        eAsyncProgress <- queryProgress rdata
        -- exponential backoff in case of errors
        liftEffect $ do
          case eAsyncProgress of
            Left _  -> T.modify_ (_ * 2) interval
            Right _ -> T.write_ 1000 interval
          interval' <- T.read interval
          resetInterval intervalIdRef (Just interval') exec
        handleRESTError (R2.herePrefix here "[asyncProgress]") errors eAsyncProgress onProgress

      onProgress :: AsyncProgress -> Aff Unit
      onProgress value = liftEffect do
        let GT.AsyncProgress { status } = value

        T.write_ (min 100.0 $ GT.progressPercent value) progressBox

        if (status == GT.IsFinished) ||
           (status == GT.IsKilled) ||
           (status == GT.IsFailure)
        then do
          resetInterval intervalIdRef Nothing exec
          -- case R.readRef intervalIdRef of
          --   Nothing  -> R.nothing
          --   Just iid -> clearInterval iid
          handleErrorInAsyncProgress errors value
          onFinish unit
        else
          R.nothing

    -- Hooks
    useFirstEffect' do
      resetInterval intervalIdRef (Just 1000) exec
      -- intervalId <- setInterval interval' $ exec unit
      -- R.setRef intervalIdRef $ Just intervalId

    -- Render
    pure $

      R.provideContext asyncContext (progress)
      children

resetInterval :: R.Ref (Maybe IntervalId) -> Maybe Int -> (Unit -> Effect Unit) -> Effect Unit
resetInterval ref mInt exec = do
  case R.readRef ref /\ mInt of
    Nothing /\ Nothing  ->
      pure unit
    Nothing /\ Just interval' -> do
      intervalId <- setInterval interval' $ exec unit
      R.setRef ref $ Just intervalId
    Just iid /\ Nothing -> do
      clearInterval iid
      R.setRef ref Nothing
    Just iid /\ Just interval' -> do
      clearInterval iid
      intervalId <- setInterval interval' $ exec unit
      R.setRef ref $ Just intervalId

asyncContext :: R.Context (Number)
asyncContext = R.createContext 0.0
