module Gargantext.Hooks.Loader where

import Gargantext.Prelude
import Data.Maybe (Maybe(..), isNothing, maybe, maybe')
import Data.Tuple (fst, Tuple(..))
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Reactix as R
import Gargantext.Utils.Reactix as R2
import Gargantext.Components.LoadingSpinner (loadingSpinner)

useAff :: forall st.
          Aff st -> R.Hooks (Maybe st)
useAff loader = do
  (loaded /\ setLoaded) <- R.useState' Nothing
  R.useEffect' $ do
    if isNothing loaded then
      R2.affEffect "G.H.Loader.useAff" $
        loader >>= (liftEffect <<< setLoaded <<< const <<< Just)
    else pure R.nothing
  pure loaded

useLoader :: forall path st. Eq path =>
             path -> (path -> Aff st) -> (st -> R.Element) -> R.Hooks R.Element
useLoader path loader render
  =   maybe' (\_ -> loadingSpinner {}) render
  -- <$> (useAff =<< R.useMemo2 path loader (\_ -> loader path))
  -- <$> (useAff =<< R2.useCache path (\p -> pure $ loader p))
  <$> (useAff =<< (loader path))

useLoader2 :: forall path st.
              R.State path -> (path -> Aff st) 
              -> (st -> R.Element) -> R.Hooks R.Element
useLoader2 path loader render = do
  state <- R.useState' Nothing
  useLoaderEffect2 path state loader
  pure $ maybe (loadingSpinner {}) render (fst state)
  
useLoaderEffect :: forall state.
                   Aff state -> R.State (Maybe state) -> R.Hooks Unit
useLoaderEffect loader (state /\ setState) = do
  R.useEffect2 state loader $ do
    if isNothing state then
      R2.affEffect "G.H.Loader.useLoader" $ loader >>= (liftEffect <<< setState <<< const <<< Just)
    else pure R.nothing

useLoaderEffect' :: forall state.
                    Aff state -> R.Hooks (R.State (Maybe state))
useLoaderEffect' aff = do
  state <- R.useState' Nothing
  useLoaderEffect aff state
  pure state

useLoaderEffect2 :: forall st path.
                    R.State path -> R.State (Maybe st)
                    -> (path -> Aff st) -> R.Hooks Unit
useLoaderEffect2 path state loader = do
  aff <- useRepointer path loader
  useLoaderEffect aff state

useRepointer :: forall path st.
                R.State path -> (path -> Aff st) -> R.Hooks (Aff st)
useRepointer path@(path' /\ _) loader = R.useMemo2 loader path' (\_ -> loader path')


