module Gargantext.Version where

import Prelude

import DOM.Simple.Console (log2)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
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

getBackendVersion :: Session -> REST.AffRESTError Version
getBackendVersion (Session { backend }) = REST.get Nothing (toUrl backend "version")


type VersionProps =
  (
    session :: Sessions.Session
  )

versionView :: R2.Component VersionProps
versionView = R.createElement versionCpt
versionCpt :: R.Component VersionProps
versionCpt = here.component "version" cpt
  where
    cpt { session } _ = do
      versionBack <- T.useBox "No Backend Version"
      versionBack' <- T.useLive T.unequal versionBack

      R.useEffect' $ do
        launchAff_ $ do
          v <- getBackendVersion session
          case v of
            Right v' -> liftEffect $ T.write_ v' versionBack
            Left err -> liftEffect $ log2 "[version] error" err

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

