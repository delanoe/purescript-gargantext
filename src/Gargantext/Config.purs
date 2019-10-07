module Gargantext.Config where

import Data.NonEmpty (NonEmpty, (:|), head)
import Gargantext.Ends
import Gargantext.Types (ApiVersion(..))

defaultBackends :: NonEmpty Array Backend
defaultBackends = local :| [ dev, demo ]
  where
  -- prod  = backend V10 "/api/" "https://gargantext.org" "gargantext.org"
  dev = backend V10 "/api/" "https://dev.gargantext.org" "dev.gargantext.org"

  demo = backend V10 "/api/" "https://demo.gargantext.org" "demo.gargantext.org"

  local = backend V10 "/api/" "http://localhost:8008" "localhost"

defaultApps :: NonEmpty Array Frontend
defaultApps = relative :| [ dev, demo, haskell, caddy ]
  where
  relative = frontend "/#/" "" "Relative"

  -- prod     = frontend "/#/" "https://gargantext.org" "gargantext.org"
  dev = frontend "/#/" "https://dev.gargantext.org" "gargantext.org (dev)"

  demo = frontend "/#/" "https://demo.gargantext.org" "gargantext.org (demo)"

  haskell = frontend "/#/" "http://localhost:8008" "localhost.gargantext"

  python = frontend "/#/" "http://localhost:8000" "localhost.python"

  caddy = frontend "/#/" "http://localhost:2015" "localhost.caddy"

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
