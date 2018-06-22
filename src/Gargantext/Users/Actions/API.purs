module Gargantext.Users.Actions.API
       where

import Gargantext.Users.Types.Types

import Control.Monad.Aff (Aff, attempt, launchAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (log)
import Data.Argonaut (decodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Identity (Identity(..))
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest, get)
import Prelude (pure, show, unit, bind, discard, (<<<), ($), (<>))


user userID = launchAff $ do
  res <- get "http://localhost:8008/node/452146"
  pure res
