module Gargantext.Hooks.Loader where

import Gargantext.Prelude

import Control.Bind ((=<<))
import Control.Monad.RWS (state)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Reactix as R
import Record as Record
import Simple.JSON as JSON
import Toestand as T

import Gargantext.Components.LoadingSpinner (loadingSpinner)
import Gargantext.Config.REST (RESTError)
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.Crypto (Hash)
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Hooks.Loader"

cacheName :: String
cacheName = "cache-api-loader"

clearCache :: Unit -> Aff Unit
clearCache _ = GUC.delete $ GUC.CacheName cacheName

type UseLoader path state =
  ( errorHandler :: RESTError -> Effect Unit
  , loader       :: path -> Aff (Either RESTError state)
  , path         :: path
  , render       :: state -> R.Element
  )

useLoader :: forall path st. Eq path => Eq st
          => Record (UseLoader path st)
          -> R.Hooks R.Element
useLoader { errorHandler, loader: loader', path, render } = do
  state <- T.useBox Nothing

  useLoaderEffect { errorHandler, loader: loader', path, state: state }

  pure $ loader { path, render, state } []


type LoaderProps path st =
  ( path   :: path
  , render :: st -> R.Element
  , state  :: T.Box (Maybe st) )

loader :: forall path st. Eq path => Eq st => R2.Component (LoaderProps path st)
loader = R.createElement loaderCpt
loaderCpt :: forall path st. Eq path => Eq st => R.Component (LoaderProps path st)
loaderCpt = here.component "loader" cpt
  where
    cpt { render, state } _ = do
      state' <- T.useLive T.unequal state

      pure $ maybe (loadingSpinner {}) render state'

type UseLoaderEffect path state =
  ( errorHandler :: RESTError -> Effect Unit
  , loader       :: path -> Aff (Either RESTError state)
  , path         :: path
  , state        :: T.Box (Maybe state)
  )

useLoaderEffect :: forall st path. Eq path => Eq st
                   => Record (UseLoaderEffect path st)
                   -> R.Hooks Unit
useLoaderEffect { errorHandler, loader: loader', path, state } = do
  state' <- T.useLive T.unequal state
  oPath <- R.useRef path

  R.useEffect' $ do
    path' <- R.readRefM oPath
    if (path' == path) && (isJust state')
    then pure $ R.nothing
    else do
      R.setRef oPath path
      R2.affEffect "G.H.Loader.useLoaderEffect" $ do
        l <- loader' path
        case l of
          Left err -> liftEffect $ errorHandler err
          Right l' -> liftEffect $ T.write_ (Just l') state


newtype HashedResponse a = HashedResponse { hash  :: Hash, value :: a }
derive instance Generic (HashedResponse a) _
derive instance Newtype (HashedResponse a) _
derive newtype instance JSON.ReadForeign a => JSON.ReadForeign (HashedResponse a)
derive newtype instance JSON.WriteForeign a => JSON.WriteForeign (HashedResponse a)

type LoaderWithCacheAPIProps path res ret = (
    cacheEndpoint :: path -> Aff (Either RESTError Hash)
  , handleResponse :: HashedResponse res -> ret
  , mkRequest :: path -> GUC.Request
  , path :: path
  , renderer :: ret -> R.Element
  )


useLoaderWithCacheAPI :: forall path res ret.
                         Eq ret => Eq path => JSON.ReadForeign res =>
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
    cacheEndpoint  :: path -> Aff (Either RESTError Hash)
  , handleResponse :: HashedResponse res -> ret
  , mkRequest      :: path -> GUC.Request
  , path           :: path
  , state          :: T.Box (Maybe ret)
  )

useCachedAPILoaderEffect :: forall path res ret.
                            Eq ret => Eq path => JSON.ReadForeign res =>
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
        hr@(HashedResponse { hash }) <- GUC.cachedJson cache req
        cacheReal <- cacheEndpoint path
        case cacheReal of
          Left _err -> throwError $ error $ "[useCachedAPILoaderEffect] RESTError"
          Right cacheReal' -> do
            val <- if hash == cacheReal' then
              pure hr
            else do
              _ <- GUC.deleteReq cache req
              hr'@(HashedResponse { hash: h }) <- GUC.cachedJson cache req
              if h == cacheReal' then
                pure hr'
              else
                throwError $ error $ "[Hooks.Loader] Fetched clean cache but hashes don't match: " <> h <> " != " <> cacheReal'
            liftEffect $ do
              T.write_ (Just $ handleResponse val) state
