module Gargantext.Config.Utils where

import Gargantext.Prelude

import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Gargantext.Config.REST (RESTError, logRESTError)
import Gargantext.Types (AsyncEvent(..), AsyncProgress(..), AsyncTaskLog(..), AsyncTaskStatus(..), FrontendError(..))
import Gargantext.Utils.Reactix as R2
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Config.Utils"

handleRESTError :: forall a.
                   R2.Here
                -> T.Box (Array FrontendError)
                -> Either RESTError a
                -> (a -> Aff Unit)
                -> Aff Unit
handleRESTError here' errors (Left error) _ = liftEffect $ do
  T.modify_ (A.cons $ FRESTError { error }) errors
  logRESTError here' "[handleTaskError]" error
  -- here.warn2 "[handleTaskError] RESTError" error
handleRESTError _ _ (Right task) handler = handler task

handleErrorInAsyncProgress :: T.Box (Array FrontendError)
                           -> AsyncProgress
                           -> Effect Unit
handleErrorInAsyncProgress errors ap@(AsyncProgress { status: IsFailure }) = do
  T.modify_ (A.cons $ FStringError { error: concatErrors ap }) errors
handleErrorInAsyncProgress errors ap@(AsyncProgress { log, status: IsFinished }) = do
  if countFailed > 0 then
    T.modify_ (A.cons $ FStringError { error: concatErrors ap }) errors
  else
    pure unit
  where
    countFailed = foldl (+) 0 $ (\(AsyncTaskLog { failed }) -> failed) <$> log
handleErrorInAsyncProgress _ _ = pure unit

concatErrors :: AsyncProgress -> String
concatErrors (AsyncProgress { error, log }) = foldl eventsErrorMessage (fromMaybe "" error) log
  where
    eventsErrorMessage acc (AsyncTaskLog { events }) = (foldl eventErrorMessage "" events) <> "\n" <> acc
    eventErrorMessage acc (AsyncEvent { level: "ERROR", message }) = message <> "\n" <> acc
    eventErrorMessage acc _ = acc
