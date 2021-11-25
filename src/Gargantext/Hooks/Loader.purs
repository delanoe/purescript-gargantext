module Gargantext.Hooks.Loader where

import Gargantext.Prelude

import Data.Array as A
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.LoadingSpinner (loadingSpinner)
import Gargantext.Config.REST (RESTError, AffRESTError)
import Gargantext.Config.Utils (handleRESTError)
import Gargantext.Types (FrontendError(..))
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.Crypto (Hash)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Simple.JSON as JSON
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Hooks.Loader"

cacheName :: String
cacheName = "cache-api-loader"

clearCache :: Unit -> Aff Unit
clearCache _ = GUC.delete $ GUC.CacheName cacheName

type UseLoader path state =
  ( errorHandler :: RESTError -> Effect Unit
  , loader       :: path -> AffRESTError state
  , path         :: path
  , render       :: state -> R.Element
  )

useLoader :: forall path st. Eq path => Eq st
          => Record (UseLoader path st)
          -> R.Hooks R.Element
useLoader { errorHandler, loader: loader', path, render } = do
  state <- T.useBox Nothing

  useLoaderEffect { errorHandler, loader: loader', path, state: state }

  pure $ loader { render, state } []


type LoaderProps st =
  ( render :: st -> R.Element
  , state  :: T.Box (Maybe st) )

loader :: forall st. Eq st => R2.Component (LoaderProps st)
loader = R.createElement loaderCpt
loaderCpt :: forall st. Eq st => R.Component (LoaderProps st)
loaderCpt = here.component "loader" cpt
  where
    cpt { render, state } _ = do
      state' <- T.useLive T.unequal state

      pure $ maybe (loadingSpinner {}) render state'

type UseLoaderEffect path state =
  ( errorHandler :: RESTError -> Effect Unit
  , loader       :: path -> AffRESTError state
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


type UseLoaderBox path state =
  ( errorHandler :: RESTError -> Effect Unit
  , loader       :: path -> AffRESTError state
  , path         :: T.Box path
  , render       :: state -> R.Element
  )

useLoaderBox :: forall path st. Eq path => Eq st
          => Record (UseLoaderBox path st)
          -> R.Hooks R.Element
useLoaderBox { errorHandler, loader: loader', path, render } = do
  state <- T.useBox Nothing

  useLoaderBoxEffect { errorHandler, loader: loader', path, state: state }

  pure $ loader { render, state } []

type UseLoaderBoxEffect path state =
  ( errorHandler :: RESTError -> Effect Unit
  , loader       :: path -> AffRESTError state
  , path         :: T.Box path
  , state        :: T.Box (Maybe state)
  )

useLoaderBoxEffect :: forall st path. Eq path => Eq st
                   => Record (UseLoaderBoxEffect path st)
                   -> R.Hooks Unit
useLoaderBoxEffect { errorHandler, loader: loader', path, state } = do
  path' <- T.useLive T.unequal path

  R.useEffect' $ do
    R2.affEffect "G.H.Loader.useLoaderBoxEffect" $ do
      l <- loader' path'
      case l of
        Left err -> liftEffect $ errorHandler err
        Right l' -> liftEffect $ T.write_ (Just l') state


newtype HashedResponse a = HashedResponse { hash  :: Hash, value :: a }
derive instance Generic (HashedResponse a) _
derive instance Newtype (HashedResponse a) _
derive newtype instance JSON.ReadForeign a => JSON.ReadForeign (HashedResponse a)
derive newtype instance JSON.WriteForeign a => JSON.WriteForeign (HashedResponse a)

type LoaderWithCacheAPIProps path res ret =
  ( boxes          :: Boxes
  , cacheEndpoint  :: path -> AffRESTError Hash
  , handleResponse :: HashedResponse res -> ret
  , mkRequest      :: path -> GUC.Request
  , path           :: path
  , renderer       :: ret -> R.Element
  )

useLoaderWithCacheAPI :: forall path res ret.
                         Eq ret => Eq path => JSON.ReadForeign res =>
                         Record (LoaderWithCacheAPIProps path res ret)
                      -> R.Hooks R.Element
useLoaderWithCacheAPI { boxes
                      , cacheEndpoint
                      , handleResponse
                      , mkRequest
                      , path
                      , renderer } = do
  state <- T.useBox Nothing
  state' <- T.useLive T.unequal state

  useCachedAPILoaderEffect { boxes
                           , cacheEndpoint
                           , handleResponse
                           , mkRequest
                           , path
                           , state }
  pure $ maybe (loadingSpinner {}) renderer state'

type LoaderWithCacheAPIEffectProps path res ret = (
    boxes          :: Boxes
  , cacheEndpoint  :: path -> AffRESTError Hash
  , handleResponse :: HashedResponse res -> ret
  , mkRequest      :: path -> GUC.Request
  , path           :: path
  , state          :: T.Box (Maybe ret)
  )

useCachedAPILoaderEffect :: forall path res ret.
                            Eq ret => Eq path => JSON.ReadForeign res =>
                            Record (LoaderWithCacheAPIEffectProps path res ret)
                         -> R.Hooks Unit
useCachedAPILoaderEffect { boxes: { errors }
                         , cacheEndpoint
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
        eCacheReal <- cacheEndpoint path
        handleRESTError errors eCacheReal $ \cacheReal -> do
          val <- if hash == cacheReal then
            pure hr
          else do
            _ <- GUC.deleteReq cache req
            hr'@(HashedResponse { hash: h }) <- GUC.cachedJson cache req
            if h == cacheReal then
              pure hr'
            else do
              let err = "[Hooks.Loader] Fetched clean cache but hashes don't match: " <> h <> " != " <> cacheReal
              liftEffect $ T.modify_ (A.cons $ FStringError { error: err }) errors
              throwError $ error err
          liftEffect $ do
            T.write_ (Just $ handleResponse val) state
