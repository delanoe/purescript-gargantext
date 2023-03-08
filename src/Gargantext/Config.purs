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
defaultBackends =   backend' "Demo"            "Public Show room"          "https://demo.gargantext.org"
               :| [ backend' "Education"       "Class Rooms"               "https://formation.gargantext.org"
                  , backend' "Organization"    "CNRS/ISCPIF Unit"          "https://cnrs.gargantext.org"
                  , backend' "Organization"    "Mines Telecom Institute"   "https://imt.sub.gargantext.org"
                  , backend' "Organization"    "Hello Word Company"        "https://helloword.gargantext.org"
                  , backend' "Networking"      "Complex Systems Community" "https://complexsystems.gargantext.org"
                  , backend' "Networking"      "European Projects"         "https://europa.gargantext.org"
                  , backend' "Development"     "Main SandBox"              "https://dev.sub.gargantext.org"
                  , backend' "Private"         "Offline Bunker"            "http://localhost:8008"
                  ]
 

  where
    backend' t n u = backend t n V10 "/api/" u


matchCurrentLocation :: Effect (Maybe Backend)
matchCurrentLocation = do
  href <- href
  let starts = AN.filter (\(Backend { baseUrl }) -> S.startsWith baseUrl href) $ AN.fromNonEmpty defaultBackends
  pure $ A.head starts


-- | public Backend
-- When user is not logged, use the location of the window
publicBackend :: Backend
publicBackend = backend "Private" "local" V10 "/api/" "http://localhost:8008"

publicBackend' :: Effect Backend
publicBackend' = do
  href <- href
  pure $ Backend { name  : "Public Backend"
                 , baseUrl : href
                 , prePath : "api/"
                 , version : V10
                 , backendType : "Public home"
                 }

defaultApps :: NonEmpty Array Frontend
defaultApps = relative :| [prod, dev, demo, haskell, python, caddy]
  where
    relative = frontend "/#/" ""                            "Relative"
    prod     = frontend "/#/" "https://v4.gargantext.org"   "v4.gargantext.org"
    dev      = frontend "/#/" "https://dev.sub.gargantext.org"  "gargantext.org (dev)"
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

