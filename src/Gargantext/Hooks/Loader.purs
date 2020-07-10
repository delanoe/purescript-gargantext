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

import Gargantext.Prelude

import Gargantext.Components.LoadingSpinner (loadingSpinner)
import Gargantext.Ends (class ToUrl, toUrl)
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


type MD5 = String


newtype HashedResponse a = HashedResponse {
    md5 :: MD5
  , value :: a
  }

instance decodeHashedResponse :: DecodeJson a => DecodeJson (HashedResponse a) where
  decodeJson json = do
    obj <- decodeJson json
    md5 <- obj .: "md5"
    value <- obj .: "value"
    pure $ HashedResponse { md5, value }

instance encodeHashedResponse :: EncodeJson a => EncodeJson (HashedResponse a) where
  encodeJson (HashedResponse { md5, value }) = do
       "md5" := encodeJson md5
    ~> "value" := encodeJson value
    ~> jsonEmptyObject

useLoaderWithCache :: forall path st. Eq path => DecodeJson st => EncodeJson st =>
                      path
                      -> (path -> String)
                      -> (path -> Aff String)
                      -> (path -> Aff (HashedResponse st))
                      -> (st -> R.Element)
                      -> R.Hooks R.Element
useLoaderWithCache path keyFunc md5Endpoint loader render = do
  state <- R.useState' Nothing
  useCachedLoaderEffect { cacheEndpoint: md5Endpoint
                        , keyFunc
                        , loadRealData: loadRealData state
                        , path
                        , state }
  pure $ maybe (loadingSpinner {}) render (fst state)
  where
    loadRealData :: R.State (Maybe st) -> String -> String -> WSS.Storage -> Aff Unit
    loadRealData (_ /\ setState) key keyCache localStorage = do
      --R2.affEffect "G.H.Loader.useCachedLoaderEffect" $ do
      HashedResponse { md5, value: l } <- loader path
      liftEffect $ do
        let value = stringify $ encodeJson l
        WSS.setItem key value localStorage
        WSS.setItem keyCache md5 localStorage
        setState $ const $ Just l
      pure unit


type CachedLoaderEffectProps cacheKey path st = (
    cacheEndpoint :: path -> Aff cacheKey
  , keyFunc :: path -> String
  , loadRealData :: String -> String -> WSS.Storage -> Aff Unit
  , path :: path
  , state :: R.State (Maybe st)
)

useCachedLoaderEffect :: forall path st. Eq path => DecodeJson st => EncodeJson st =>
                         Record (CachedLoaderEffectProps String path st)
                         -> R.Hooks Unit
useCachedLoaderEffect { cacheEndpoint, keyFunc, loadRealData, path, state: state@(state' /\ setState) } = do
  oPath <- R.useRef path

  R.useEffect' $ do
    if (R.readRef oPath == path) && (isJust state') then
      pure unit
    else do
      R.setRef oPath path

      let key = "loader--" <> (keyFunc path)
      -- log2 "[useCachedLoader] key" key
      let keyCache = key <> "-cache"
      localStorage <- R2.getls
      mState <- WSS.getItem key localStorage
      mCache <- WSS.getItem keyCache localStorage
      -- log2 "[useCachedLoader] mState" mState
      launchAff_ $ do
        case mState of
          Nothing -> loadRealData key keyCache localStorage
          Just stStr -> do
            let parsed = parse stStr >>= decode
            case parsed of
              Left err -> do
                -- liftEffect $ log2 "[useCachedLoader] err" err
                loadRealData key keyCache localStorage
              Right (st :: st) -> do
                cacheReal <- cacheEndpoint path
                -- liftEffect $ log2 "[useCachedLoader] cacheReal" cacheReal
                case mCache of
                  Nothing -> do
                    -- liftEffect $ log2 "[useCachedLoader] no stored cache" Nothing
                    loadRealData key keyCache localStorage
                  Just cache -> do
                    -- liftEffect $ log2 "[useCachedLoader] stored cache" cache
                    if cache == cacheReal then
                      -- yay! cache hit!
                      liftEffect $ setState $ const $ Just st
                    else
                      loadRealData key keyCache localStorage

  where
    parse  s = GU.mapLeft (\err -> "Error parsing serialised sessions:" <> show err) (jsonParser s)
    decode j = GU.mapLeft (\err -> "Error decoding serialised sessions:" <> show err) (decodeJson j)


type LoaderWithCacheAPIProps path res ret = (
    cacheEndpoint :: path -> Aff MD5
  , handleResponse :: HashedResponse res -> ret
  , mkRequest :: path -> GUC.Request
  , path :: path
  , renderer :: ret -> R.Element
  )


useLoaderWithCacheAPI :: forall path res ret. Eq path => Show path => DecodeJson res =>
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
    cacheEndpoint :: path -> Aff MD5
  , handleResponse :: HashedResponse res -> ret
  , mkRequest :: path -> GUC.Request
  , path :: path
  , state :: R.State (Maybe ret)
  )

useCachedAPILoaderEffect :: forall path res ret. Eq path => Show path => DecodeJson res =>
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
      let keyCache = "cached-api-md5-" <> (show path)
      -- log2 "[useCachedLoader] mState" mState
      launchAff_ $ do
        cache <- GUC.openCache $ GUC.CacheName cacheName
        -- TODO Parallelize?
        hr@(HashedResponse { md5, value }) <- GUC.cachedJson cache req
        cacheReal <- cacheEndpoint path
        val <- if md5 == cacheReal then
          pure hr
        else do
          _ <- GUC.delete cache req
          hr@(HashedResponse { md5, value }) <- GUC.cachedJson cache req
          if md5 == cacheReal then
            pure hr
          else
            throwError $ error $ "Fetched clean cache but hashes don't match"
        liftEffect $ do
          setState $ const $ Just $ handleResponse hr
