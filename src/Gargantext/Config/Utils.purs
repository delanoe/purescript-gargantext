module Gargantext.Config.Utils where

import Gargantext.Prelude

import Data.Array as A
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Gargantext.Config.REST (RESTError)
import Gargantext.Types (FrontendError(..))
import Gargantext.Utils.Reactix as R2
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Config.Utils"

handleRESTError :: forall a. T.Box (Array FrontendError)
                -> Either RESTError a
                -> (a -> Aff Unit)
                -> Aff Unit
handleRESTError errors (Left error) _ = liftEffect $ do
  T.modify_ (A.cons $ FRESTError { error }) errors
  here.log2 "[handleTaskError] RESTError" error
handleRESTError _ (Right task) handler = handler task

