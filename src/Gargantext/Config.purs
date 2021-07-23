module Gargantext.Config where

import Data.Array as A
import Data.Array.NonEmpty as AN
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty, (:|), head)
import Data.String (Pattern(..), Replacement(..), replace) as S
import Data.String.Utils (startsWith) as S
import Effect (Effect)
import Gargantext.Ends
import Gargantext.Prelude (bind, pure, ($))
import Gargantext.Types (ApiVersion(..))
import Gargantext.Utils (href)

defaultBackends :: NonEmpty Array Backend
defaultBackends =
 backend_local :| [ backend_prod, backend_partner, backend_demo, backend_dev ]

prodUrl :: String
prodUrl = "https://v4.gargantext.org"
backend_prod :: Backend
backend_prod    = backend V10 "/api/" prodUrl    "iscpif.cnrs"

partnerUrl :: String
partnerUrl = "https://imtv4.gargantext.org"
backend_partner :: Backend
backend_partner = backend V10 "/api/" partnerUrl "institut-mines-telecom.imt"

demoUrl :: String
demoUrl = "https://demo.gargantext.org"
backend_demo :: Backend
backend_demo    = backend V10 "/api/" demoUrl    "demo.inshs.cnrs"

devUrl :: String
devUrl = "https://dev.gargantext.org"
backend_dev :: Backend
backend_dev     = backend V10 "/api/" devUrl     "devel.inshs.cnrs"

localUrl :: String
localUrl = "http://localhost:8008"
backend_local :: Backend
backend_local   = backend V10 "/api/" localUrl   "local.cnrs"

matchCurrentLocation :: Effect (Maybe Backend)
matchCurrentLocation = do
  href <- href
  let starts = AN.filter (\(Backend { baseUrl }) -> S.startsWith baseUrl href) $ AN.fromNonEmpty defaultBackends
  pure $ A.head starts


-- | public Backend
-- When user is not logged, use the location of the window
publicBackend :: Backend
publicBackend = backend_local

publicBackend' :: Effect Backend
publicBackend' = do
  href <- href
  pure $ Backend { name  : "Public Backend"
                 , baseUrl : href
                 , prePath : "api/"
                 , version : V10
                 }

defaultApps :: NonEmpty Array Frontend
defaultApps = relative :| [prod, dev, demo, haskell, python, caddy]
  where
    relative = frontend "/#/" ""                            "Relative"
    prod     = frontend "/#/" "https://v4.gargantext.org"   "v4.gargantext.org"
    dev      = frontend "/#/" "https://dev.gargantext.org"  "gargantext.org (dev)"
    demo     = frontend "/#/" "https://demo.gargantext.org" "gargantext.org (demo)"
    haskell  = frontend "/#/" "http://localhost:8008"       "localhost.gargantext"
    python   = frontend "/#/" "http://localhost:8000"       "localhost.python"
    caddy    = frontend "/#/" "http://localhost:2015"       "localhost.caddy"

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

changePort :: String -> String
changePort = S.replace (S.Pattern "http://localhost:8000/") (S.Replacement "http://localhost:8008/")

