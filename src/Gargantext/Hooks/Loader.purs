module Gargantext.Hooks.Loader where

import Gargantext.Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Reactix as R
import Web.Storage.Storage as WSS

import Gargantext.Components.LoadingSpinner (loadingSpinner)
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


useLoaderWithCache :: forall path st. Eq path => DecodeJson st => EncodeJson st =>
                      path
                      -> (path -> String)
                      -> (path -> Aff st)
                      -> (st -> R.Element) -> R.Hooks R.Element
useLoaderWithCache path keyFunc loader render = do
  state <- R.useState' Nothing
  useCachedLoaderEffect path keyFunc state loader
  pure $ maybe (loadingSpinner {}) render (fst state)

useCachedLoaderEffect :: forall path st. Eq path => DecodeJson st => EncodeJson st =>
                      path
                      -> (path -> String)
                      -> R.State (Maybe st)
                      -> (path -> Aff st)
                      -> R.Hooks Unit
useCachedLoaderEffect path keyFunc state@(state' /\ setState) loader = do
  oPath <- R.useRef path

  R.useEffect' $ do
    if (R.readRef oPath == path) && (isJust state') then
      pure $ pure unit
    else do
      R.setRef oPath path

      let key = keyFunc path
      localStorage <- R2.getls
      mState <- WSS.getItem key localStorage
      case mState of
        Nothing -> pure unit
        Just stStr ->
          case (parse stStr >>= decode) of
            Left err -> pure unit
            Right st -> setState $ const $ Just st

      R2.affEffect "G.H.Loader.useCachedLoaderEffect" $ do
        l <- loader path
        liftEffect $ do
          let value = stringify $ encodeJson l
          WSS.setItem key value localStorage
          setState $ const $ Just l

  where
    parse  s = GU.mapLeft (log2 "Error parsing serialised sessions:") (jsonParser s)
    decode j = GU.mapLeft (log2 "Error decoding serialised sessions:") (decodeJson j)
