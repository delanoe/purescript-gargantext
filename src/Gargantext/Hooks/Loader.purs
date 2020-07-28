module Gargantext.Hooks.Loader where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, (.:), (:=), (~>), jsonEmptyObject)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect.Aff (Aff, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Milkis as M
import Reactix as R
import Web.Storage.Storage as WSS

import Gargantext.Components.LoadingSpinner (loadingSpinner)
import Gargantext.Ends (class ToUrl, toUrl)
import Gargantext.Prelude
import Gargantext.Utils.Crypto (Hash)
import Gargantext.Utils as GU
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.Reactix as R2


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
    if (R.readRef oPath == path) && (isJust state') then
      pure $ pure unit
    else do
      R.setRef oPath path

      R2.affEffect "G.H.Loader.useLoaderEffect" $ do
        l <- loader path
        liftEffect $ setState $ const $ Just l


newtype HashedResponse a =
  HashedResponse { hash  :: Hash
                 , value :: a
                 }

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
                         Eq path => DecodeJson res =>
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
    cacheEndpoint :: path -> Aff Hash
  , handleResponse :: HashedResponse res -> ret
  , mkRequest :: path -> GUC.Request
  , path :: path
  , state :: R.State (Maybe ret)
  )

useCachedAPILoaderEffect :: forall path res ret.
                            Eq path => DecodeJson res =>
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

      let cacheName = "cache-api-loader"
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
          _ <- GUC.delete cache req
          hr@(HashedResponse { hash, value }) <- GUC.cachedJson cache req
          if hash == cacheReal then
            pure hr
          else
            throwError $ error $ "Fetched clean cache but hashes don't match"
        liftEffect $ do
          setState $ const $ Just $ handleResponse val
