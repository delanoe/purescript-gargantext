module Gargantext.Version where


import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)

import Gargantext.Config.REST as REST
import Gargantext.Ends (toUrl)
import Gargantext.Sessions (Session(..))

type Version = String


foreign import version :: Version

getBackendVersion :: Session -> Aff Version
getBackendVersion (Session { backend }) = REST.get Nothing (toUrl backend "version")
