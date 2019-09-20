module Gargantext.Hooks.Loader where

import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Gargantext.Prelude
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Reactix as R
import Gargantext.Utils.Reactix as R2
import Gargantext.Components.LoadingSpinner (loadingSpinner)

useLoader :: forall path state. path -> (path -> Aff state) -> (state -> R.Element) -> R.Hooks R.Element
useLoader path loader render = do
  state <- R.useState' Nothing
  loader' <- R.useMemo2 path loader (\_ -> loader path)
  useLoaderEffect state loader'
  pure $ maybe (loadingSpinner {}) render (fst state)

useLoader2 :: forall path state. R.State path -> (path -> Aff state) -> (state -> R.Element) -> R.Hooks R.Element
useLoader2 path loader render = do
  state <- R.useState' Nothing
  useLoaderEffect2 path state loader
  pure $ maybe (loadingSpinner {}) render (fst state)
  
useLoaderEffect :: forall state. R.State (Maybe state) -> Aff state -> R.Hooks Unit
useLoaderEffect (state /\ setState) loader = do
  R.useEffect2 state loader $ do
    if isNothing state then
      R2.affEffect "useLoader" $ loader >>= (liftEffect <<< setState <<< const <<< Just)
    else pure R.nothing

useLoaderEffect' :: forall state. Aff state -> R.Hooks (R.State (Maybe state))
useLoaderEffect' aff = do
  state <- R.useState' Nothing
  useLoaderEffect state aff
  pure state

useLoaderEffect2 :: forall st path. R.State path -> R.State (Maybe st) -> (path -> Aff st) -> R.Hooks Unit
useLoaderEffect2 path state loader = useRepointer path loader >>= useLoaderEffect state

useRepointer :: forall path st. R.State path -> (path -> Aff st) -> R.Hooks (Aff st)
useRepointer path@(path' /\ _) loader = R.useMemo1 path' (\_ -> loader path')


