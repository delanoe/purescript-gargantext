module Gargantext.Version where

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Prelude
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Config.REST as REST
import Gargantext.Ends (toUrl)
import Gargantext.Sessions (Session(..))
import Gargantext.Sessions as Sessions
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Version"

type Version = String

foreign import version :: Version

getBackendVersion :: Session -> Aff Version
getBackendVersion (Session { backend }) = REST.get Nothing (toUrl backend "version")


type VersionProps =
  (
    session :: Sessions.Session
  )

versionView :: Record VersionProps -> R.Element
versionView props = R.createElement versionCpt props []

versionCpt :: R.Component VersionProps
versionCpt = here.component "version" cpt
  where
    cpt { session } _ = do
      versionBack <- T.useBox "No Backend Version"
      versionBack' <- T.useLive T.unequal versionBack

      R.useEffect' $ do
        launchAff_ $ do
          v <- getBackendVersion session
          liftEffect $ T.write_ v versionBack

      pure $ case version == versionBack' of
        true  -> H.a { className: "fa fa-check-circle-o"
                     , textDecoration: "none"
                     , title: "Versions match: frontend ("
                            <> version
                            <> "), backend ("
                            <> versionBack'
                            <> ")"
                      } []
        false -> H.a { className: "fa fa-exclamation-triangle"
                     , textDecoration: "none"
                     , title: "Versions mismatch: frontend ("
                            <> version
                            <> "), backend ("
                            <> versionBack'
                            <> ")"
                     } []

