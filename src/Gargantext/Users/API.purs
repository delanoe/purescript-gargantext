module Gargantext.Users.API
       where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Data.Either (Either)
import Gargantext.REST (get)
import Gargantext.Users (User)
import Network.HTTP.Affjax (AJAX)
import Prelude (show, ($), (<>))

user :: forall eff. Int -> Aff
        (console :: CONSOLE, ajax :: AJAX | eff) (Either String User)
user id = get $ "localhost:8008/node/" <> show id
