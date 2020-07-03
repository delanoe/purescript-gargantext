module Gargantext.Hooks.Loader where

import Affjax.RequestHeader as ARH
import Affjax.ResponseHeader as ARsH
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, (.:), (:=), (~>), jsonEmptyObject)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Tuple (fst, Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Reactix as R
import Web.Storage.Storage as WSS

import Gargantext.Prelude

import Gargantext.Components.LoadingSpinner (loadingSpinner)
import Gargantext.Sessions (getH, Session)
import Gargantext.Utils as GU
import Gargantext.Utils.Reactix as R2


useLoader :: forall path st. Eq path =>
             path -> (path -> Aff st)
             -> (st -> R.Element) -> R.Hooks R.Element
useLoader path loader render = do
  state <- R.useState' Nothing
  useLoaderEffect path state loader
  pure $ maybe (loadingSpinner {}) render (fst state)

useLoaderEffect :: forall st path. Eq path =>
                   path -> R.State (Maybe st)
                   -> (path -> Aff st) -> R.Hooks Unit
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


newtype HashedResponse a = HashedResponse {
    md5 :: String
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
                      -> (st -> R.Element) -> R.Hooks R.Element
useLoaderWithCache path keyFunc md5Endpoint loader render = do
  state <- R.useState' Nothing
  useCachedLoaderEffect path keyFunc md5Endpoint state loader
  pure $ maybe (loadingSpinner {}) render (fst state)

useCachedLoaderEffect :: forall path st. Eq path => DecodeJson st => EncodeJson st =>
                      path
                      -> (path -> String)
                      -> (path -> Aff String)
                      -> R.State (Maybe st)
                      -> (path -> Aff (HashedResponse st))
                      -> R.Hooks Unit
useCachedLoaderEffect path keyFunc md5Endpoint state@(state' /\ setState) loader = do
  oPath <- R.useRef path

  R.useEffect' $ do
    if (R.readRef oPath == path) && (isJust state') then
      pure unit
    else do
      R.setRef oPath path

      let key = "loader--" <> (keyFunc path)
      -- log2 "[useCachedLoader] key" key
      let keyMD5 = key <> "-md5"
      localStorage <- R2.getls
      mState <- WSS.getItem key localStorage
      mMD5 <- WSS.getItem keyMD5 localStorage
      -- log2 "[useCachedLoader] mState" mState
      launchAff_ $ do
        case mState of
          Nothing -> loadRealData key keyMD5 localStorage
          Just stStr -> do
            let parsed = parse stStr >>= decode
            case parsed of
              Left err -> do
                -- liftEffect $ log2 "[useCachedLoader] err" err
                loadRealData key keyMD5 localStorage
              Right (st :: st) -> do
                md5Real <- md5Endpoint path
                -- liftEffect $ log2 "[useCachedLoader] md5Real" md5Real
                case mMD5 of
                  Nothing -> do
                    -- liftEffect $ log2 "[useCachedLoader] no stored md5" Nothing
                    loadRealData key keyMD5 localStorage
                  Just md5 -> do
                    -- liftEffect $ log2 "[useCachedLoader] stored md5" md5
                    if md5 == md5Real then
                      -- yay! cache hit!
                      liftEffect $ setState $ const $ Just st
                    else
                      loadRealData key keyMD5 localStorage

  where
    loadRealData :: String -> String -> WSS.Storage -> Aff Unit
    loadRealData key keyMD5 localStorage = do
      --R2.affEffect "G.H.Loader.useCachedLoaderEffect" $ do
      HashedResponse { md5, value: l } <- loader path
      liftEffect $ do
        let value = stringify $ encodeJson l
        WSS.setItem key value localStorage
        WSS.setItem keyMD5 md5 localStorage
        setState $ const $ Just l
      pure unit

    parse  s = GU.mapLeft (\err -> "Error parsing serialised sessions:" <> show err) (jsonParser s)
    decode j = GU.mapLeft (\err -> "Error decoding serialised sessions:" <> show err) (decodeJson j)


newtype RequestWithLastUpdated a = RequestWithLastUpdated {
    mLastUpdated :: Maybe String
  , path :: a
  }


newtype ResponseWithLastUpdated a = ResponseWithLastUpdated {
    lastUpdated :: String
  , value :: a
  }


getLU :: forall a p. DecodeJson a => Session -> RequestWithLastUpdated p -> Aff (ResponseWithLastUpdated a)


useLoaderWithLastUpdated :: forall path st. Eq path => DecodeJson st => EncodeJson st =>
                            path
                            -> (path -> String)
                            -> ((RequestWithLastUpdated path) -> Aff (ResponseWithLastUpdated st))
                            -> (st -> R.Element) -> R.Hooks R.Element
useLoaderWithLastUpdated path keyFunc loader render = do
  state <- R.useState' Nothing
  useLoaderWithLastUpdatedEffect path keyFunc state loader
  pure $ maybe (loadingSpinner {}) render (fst state)

useLoaderWithLastUpdatedEffect :: forall path st. Eq path => DecodeJson st => EncodeJson st =>
                                  path
                                  -> (path -> String)
                                  -> R.State (Maybe st)
                                  -> ((RequestWithLastUpdated path) -> Aff (ResponseWithLastUpdated st))
                                  -> R.Hooks Unit
useLoaderWithLastUpdatedEffect path keyFunc state@(state' /\ setState) loader = do
  oPath <- R.useRef path

  R.useEffect' $ do
    if (R.readRef oPath == path) && (isJust state') then
      pure unit
    else do
      R.setRef oPath path

      let key = "loader--" <> (keyFunc path)
      -- log2 "[useCachedLoader] key" key
      let keyLastUpdated = key <> "-last-updated"
      localStorage <- R2.getls
      mState <- WSS.getItem key localStorage
      mLastUpdated <- WSS.getItem keyLastUpdated localStorage
      -- log2 "[useCachedLoader] mState" mState
      launchAff_ $ do
        case mState of
          -- local data missing
          Nothing -> loadFreshData key keyLastUpdated localStorage
          Just stStr -> do
            let parsed = parse stStr >>= decode
            case parsed of
              -- local data corrupt
              Left err -> do
                -- liftEffect $ log2 "[useCachedLoader] err" err
                loadFreshData key keyLastUpdated localStorage
              -- local data ok
              Right (st :: st) -> do
                case mLastUpdated of
                  Nothing -> loadFreshData key keyLastUpdated localStorage
                  Just lastUpdated' -> do
                    let req = RequestWithLastUpdated { mLastUpdated: Just lastUpdated', path }
                    ResponseWithLastUpdated { lastUpdated, value: l } <- loader req
                    liftEffect $ do
                      let value = stringify $ encodeJson l
                      WSS.setItem key value localStorage
                      WSS.setItem keyLastUpdated lastUpdated localStorage
                      setState $ const $ Just l
                    pure unit


  where
    loadFreshData :: String -> String -> WSS.Storage -> Aff Unit
    loadFreshData key keyLastUpdated localStorage = do
      --R2.affEffect "G.H.Loader.useCachedLoaderEffect" $ do
      let req = RequestWithLastUpdated { mLastUpdated: Nothing, path }
      ResponseWithLastUpdated { lastUpdated, value: l } <- loader req
      liftEffect $ do
        let value = stringify $ encodeJson l
        WSS.setItem key value localStorage
        WSS.setItem keyLastUpdated lastUpdated localStorage
        setState $ const $ Just l
      pure unit

    parse  s = GU.mapLeft (\err -> "Error parsing serialised sessions:" <> show err) (jsonParser s)
    decode j = GU.mapLeft (\err -> "Error decoding serialised sessions:" <> show err) (decodeJson j)
