module Gargantext.Components.NgramsTable.Loader where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, (.:), (:=), (~>), jsonEmptyObject)
import Data.Argonaut.Core (stringify)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Reactix as R
import Web.Storage.Storage as WSS
  
import Gargantext.Prelude

import Gargantext.Components.LoadingSpinner (loadingSpinner)
import Gargantext.Components.NgramsTable.Core (Version(..), Versioned(..))
import Gargantext.Hooks.Loader (useCachedLoaderEffect)


useLoaderWithVersionCache :: forall path st. Eq path => DecodeJson st => EncodeJson st =>
                                path
                             -> (path -> String)
                             -> (path -> Aff Version)
                             -> (path -> Aff (Versioned st))
                             -> (Versioned st -> R.Element)
                             -> R.Hooks R.Element
useLoaderWithVersionCache path keyFunc versionEndpoint loader render = do
  state <- R.useState' Nothing
  useCachedLoaderEffect { cacheEndpoint: strVersionEndpoint
                        , keyFunc
                        , loadRealData: loadRealData state
                        , path
                        , state }
  pure $ maybe (loadingSpinner {}) render (fst state)
  where
    strVersionEndpoint :: path -> Aff String
    strVersionEndpoint p = do
      v <- versionEndpoint p
      pure $ show v

    loadRealData :: R.State (Maybe (Versioned st)) -> String -> String -> WSS.Storage -> Aff Unit
    loadRealData (_ /\ setState) key keyCache localStorage = do
      --R2.affEffect "G.H.Loader.useCachedLoaderEffect" $ do
      v@(Versioned { version }) <- loader path
      liftEffect $ do
        let value = stringify $ encodeJson v
        WSS.setItem key value localStorage
        WSS.setItem keyCache (show version) localStorage
        setState $ const $ Just v
      pure unit