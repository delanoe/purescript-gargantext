module Gargantext.Hooks.Loader where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, (.:), (:=), (~>), jsonEmptyObject)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Reactix as R
import Toestand as T

import Gargantext.Components.LoadingSpinner (loadingSpinner)
import Gargantext.Prelude
import Gargantext.Utils.Crypto (Hash)
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.Reactix as R2

cacheName :: String
cacheName = "cache-api-loader"

clearCache :: Unit -> Aff Unit
clearCache _ = GUC.delete $ GUC.CacheName cacheName


useLoader :: forall path st. Eq path
          => path
          -> (path -> Aff st)
          -> (st -> R.Element)
          -> R.Hooks R.Element
useLoader path loader render = do
  state <- R.useState' Nothing
  useLoaderEffect path state loader
  pure $ maybe (loadingSpinner {}) render (fst state)

useLoaderEffect :: forall st path. Eq path =>
                      path
                   -> R.State (Maybe st)
                   -> (path -> Aff st)
                   -> R.Hooks Unit
useLoaderEffect path state@(state' /\ setState) loader = do
  oPath <- R.useRef path

  R.useEffect' $ do
    path' <- R.readRefM oPath
    if (path' == path) && (isJust state')
    then pure $ R.nothing
    else do
      R.setRef oPath path
      R2.affEffect "G.H.Loader.useLoaderEffect" $ do
        l <- loader path
        liftEffect $ setState $ const $ Just l


newtype HashedResponse a = HashedResponse { hash  :: Hash, value :: a }

instance decodeHashedResponse :: DecodeJson a => DecodeJson (HashedResponse a) where
  decodeJson json = do
    obj   <- decodeJson json
    hash  <- obj .: "hash"
    value <- obj .: "value"
    pure $ HashedResponse { hash, value }

instance encodeHashedResponse :: EncodeJson a => EncodeJson (HashedResponse a) where
  encodeJson (HashedResponse { hash, value }) = do
       "hash" := encodeJson hash
    ~> "value" := encodeJson value
    ~> jsonEmptyObject


type LoaderWithCacheAPIProps path res ret = (
    cacheEndpoint :: path -> Aff Hash
  , handleResponse :: HashedResponse res -> ret
  , mkRequest :: path -> GUC.Request
  , path :: path
  , renderer :: ret -> R.Element
  )


useLoaderWithCacheAPI :: forall path res ret.
                         Eq ret => Eq path => DecodeJson res =>
                         Record (LoaderWithCacheAPIProps path res ret)
                      -> R.Hooks R.Element
useLoaderWithCacheAPI { cacheEndpoint, handleResponse, mkRequest, path, renderer } = do
  state <- T.useBox Nothing
  state' <- T.useLive T.unequal state

  useCachedAPILoaderEffect { cacheEndpoint
                           , handleResponse
                           , mkRequest
                           , path
                           , state }
  pure $ maybe (loadingSpinner {}) renderer state'

type LoaderWithCacheAPIEffectProps path res ret = (
    cacheEndpoint  :: path -> Aff Hash
  , handleResponse :: HashedResponse res -> ret
  , mkRequest      :: path -> GUC.Request
  , path           :: path
  , state          :: T.Box (Maybe ret)
  )

useCachedAPILoaderEffect :: forall path res ret.
                            Eq ret => Eq path => DecodeJson res =>
                            Record (LoaderWithCacheAPIEffectProps path res ret)
                         -> R.Hooks Unit
useCachedAPILoaderEffect { cacheEndpoint
                         , handleResponse
                         , mkRequest
                         , path
                         , state } = do
  state' <- T.useLive T.unequal state
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
        hr@(HashedResponse { hash, value }) <- GUC.cachedJson cache req
        cacheReal <- cacheEndpoint path
        val <- if hash == cacheReal then
          pure hr
        else do
          _ <- GUC.deleteReq cache req
          hr'@(HashedResponse { hash: h }) <- GUC.cachedJson cache req
          if h == cacheReal then
            pure hr'
          else
            throwError $ error $ "Fetched clean cache but hashes don't match: " <> h <> " != " <> cacheReal
        liftEffect $ do
          T.write_ (Just $ handleResponse val) state
