module Gargantext.Config where

import Data.String as S
import Web.HTML.Location (Location(..))
import Effect (Effect)
import Data.NonEmpty (NonEmpty, (:|), head)
import Gargantext.Ends
import Gargantext.Types (ApiVersion(..))
import Gargantext.Utils (location)
import Gargantext.Prelude (bind, pure, ($))

defaultBackends :: NonEmpty Array Backend
defaultBackends = local :| [prod, partner, demo, dev]
  where
    prod    = backend V10 "/api/" "https://v4.gargantext.org"   "iscpif.cnrs"
    partner = backend V10 "/api/" "https://imtv4.gargantext.org" "institut-mines-telecom.imt"
    demo    = backend V10 "/api/" "https://demo.gargantext.org" "demo.inshs.cnrs"
    dev     = backend V10 "/api/" "https://dev.gargantext.org"  "devel.inshs.cnrs"
    local   = backend V10 "/api/" "http://localhost:8008"       "local.cnrs"

defaultApps :: NonEmpty Array Frontend
defaultApps = relative :| [prod, dev, demo, haskell, caddy]
  where
    relative = frontend "/#/" "" "Relative"
    prod     = frontend "/#/" "https://v4.gargantext.org" "v4.gargantext.org"
    dev      = frontend "/#/" "https://dev.gargantext.org" "gargantext.org (dev)"
    demo     = frontend "/#/" "https://demo.gargantext.org" "gargantext.org (demo)"
    haskell  = frontend "/#/" "http://localhost:8008" "localhost.gargantext"
    python   = frontend "/#/" "http://localhost:8000" "localhost.python"
    caddy    = frontend "/#/" "http://localhost:2015" "localhost.caddy"

defaultStatics :: NonEmpty Array Frontend
defaultStatics = relative :| []
  where
    relative = frontend "" "/" "relative"

defaultApp :: Frontend
defaultApp = head defaultApps

defaultStatic :: Frontend
defaultStatic = head defaultStatics

defaultFrontends :: Frontends
defaultFrontends = Frontends { app: defaultApp, static: defaultStatic }

-- | public Backend
-- When user is not logged, use the location of the window
publicBackend :: Effect Backend
publicBackend = do
  url <- location
  pure $ Backend { name  : "Public Backend"
                 , baseUrl : url
                 , prePath : "api/"
                 , version : V10
                 }

changePort :: String -> String
changePort = S.replace (S.Pattern "http://localhost:8000/") (S.Replacement "http://localhost:8008/")


