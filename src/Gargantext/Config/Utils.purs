module Gargantext.Config.Utils where

import Gargantext.Prelude

import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Gargantext.Config.REST (RESTError)
import Gargantext.Types (AsyncProgress(..), AsyncTaskEvent(..), AsyncTaskLog(..), FrontendError(..))
import Gargantext.Utils.Reactix as R2
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Config.Utils"

handleRESTError :: forall a.
                   T.Box (Array FrontendError)
                -> Either RESTError a
                -> (a -> Aff Unit)
                -> Aff Unit
handleRESTError errors (Left error) _ = liftEffect $ do
  T.modify_ (A.cons $ FRESTError { error }) errors
  here.log2 "[handleTaskError] RESTError" error
handleRESTError _ (Right task) handler = do
  handler task

handleTaskRESTError :: T.Box (Array FrontendError)
                    -> Either RESTError AsyncProgress
                    -> (AsyncProgress -> Aff Unit)
                    -> Aff Unit
handleTaskRESTError errors eTask handler = do
  handleRESTError errors eTask $ \task -> do
    

handleAsyncTaskError :: AsyncProgress
                     -> T.Box (Array FrontendError)
                     -> Aff Unit
handleAsyncTaskError (AsyncProgress { log, status }) errors = do
  case A.uncons log of
    Nothing -> do
      liftEffect $ here.log "[handleAsyncTaskError] log empty"
      pure unit
    Just { head: AsyncTaskLog { events, failed } } -> do
      if failed == 0
        then do
          liftEffect $ here.log "[handleAsyncTaskError] failed = 0"
          pure unit
        else case A.uncons events of
          Nothing -> do
            liftEffect $ here.log "[handleAsyncTaskError] events empty"
            pure unit
          Just { head: AsyncTaskEvent { message, level } } -> liftEffect $ do
            T.modify_ (A.cons $ FOtherError { error: "[" <> level <> "] " <> message }) errors
            here.log2 "[handleAsyncTaskError] message" message
            here.log2 "[handleAsyncTaskError] level" level
          
