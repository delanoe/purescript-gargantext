module Gargantext.Components.ForgotPassword where

import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Gargantext.Config.REST (AffRESTError, logRESTError, get)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

here :: R2.Here
here = R2.here  "Gargantext.Components.ForgotPassword"

type ForgotPasswordProps = ( server :: String, uuid :: String )

forgotPasswordLayout :: R2.Component ForgotPasswordProps
forgotPasswordLayout = R.createElement forgotPasswordLayoutCpt

forgotPasswordLayoutCpt :: R.Component ForgotPasswordProps
forgotPasswordLayoutCpt = here.component "forgotPasswordLayout" cpt where
  cpt { server, uuid } _ = do
    useLoader { errorHandler
              , loader: loadPassword
              , path: { server, uuid }
              , render: \{ password } ->  
                H.p {} [ H.text ("Your new password is: " <> password) ] }
    where
      errorHandler = logRESTError here "[forgotPasswordLayout]"

------------------------------------

type PasswordData = ( password :: String )

loadPassword :: Record ForgotPasswordProps -> AffRESTError (Record PasswordData)
loadPassword { server, uuid } = get Nothing (server <> "/api/v1.0/forgot-password?uuid=" <> uuid )
