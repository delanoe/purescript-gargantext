module Gargantext.Components.NgramsTable.Loader where

import Data.Argonaut (class DecodeJson)
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log, log2)
import Effect.Aff (Aff, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Reactix as R

import Gargantext.Prelude

import Gargantext.Components.LoadingSpinner (loadingSpinner)
import Gargantext.Components.NgramsTable.Core (Version(..), Versioned(..))
import Gargantext.Utils.CacheAPI as GUC


cacheName = "ngrams-cache-api-loader"


clearCache :: Unit -> Aff Unit
clearCache _ = GUC.delete $ GUC.CacheName cacheName


type LoaderWithCacheAPIProps path res ret = (
    cacheEndpoint :: path -> Aff Version
  , handleResponse :: Versioned res -> ret
  , mkRequest :: path -> GUC.Request
  , path :: path
  , renderer :: ret -> R.Element
  )


useLoaderWithCacheAPI :: forall path res ret. Eq path => DecodeJson res =>
                         Record (LoaderWithCacheAPIProps path res ret)
                      -> R.Hooks R.Element
useLoaderWithCacheAPI { cacheEndpoint, handleResponse, mkRequest, path, renderer } = do
  state <- R.useState' Nothing
  useCachedAPILoaderEffect { cacheEndpoint
                           , handleResponse
                           , mkRequest
                           , path
                           , state }
  pure $ maybe (loadingSpinner {}) renderer (fst state)

type LoaderWithCacheAPIEffectProps path res ret = (
    cacheEndpoint :: path -> Aff Version
  , handleResponse :: Versioned res -> ret
  , mkRequest :: path -> GUC.Request
  , path :: path
  , state :: R.State (Maybe ret)
  )

useCachedAPILoaderEffect :: forall path res ret. Eq path => DecodeJson res =>
                            Record (LoaderWithCacheAPIEffectProps path res ret)
                         -> R.Hooks Unit
useCachedAPILoaderEffect { cacheEndpoint
                         , handleResponse
                         , mkRequest
                         , path
                         , state: state@(state' /\ setState) } = do
  oPath <- R.useRef path

  R.useEffect' $ do
    if (R.readRef oPath == path) && (isJust state') then
      pure unit
    else do
      R.setRef oPath path

      let req = mkRequest path
      -- log2 "[useCachedLoader] mState" mState
      launchAff_ $ do
        cache <- GUC.openCache $ GUC.CacheName cacheName
        -- TODO Parallelize?
        vr@(Versioned { version, "data": d }) <- GUC.cachedJson cache req
        cacheReal <- cacheEndpoint path
        val <- if version == cacheReal then
          pure vr
        else do
          -- liftEffect $ do
          --   log "[useCachedAPILoaderEffect] versions dont match"
          --   log2 "[useCachedAPILoaderEffect] cached version" version
          --   log2 "[useCachedAPILoaderEffect] real version" cacheReal
          _ <- GUC.deleteReq cache req
          vr@(Versioned { version, "data": d }) <- GUC.cachedJson cache req
          if version == cacheReal then
            pure vr
          else
            throwError $ error $ "Fetched clean cache but hashes don't match"
        liftEffect $ do
          setState $ const $ Just $ handleResponse val
