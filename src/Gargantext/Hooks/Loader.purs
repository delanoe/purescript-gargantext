module Gargantext.Hooks.Loader where

import Gargantext.Prelude
import Data.Maybe (Maybe(..), isJust, isNothing, maybe, maybe')
import Data.Tuple (fst, Tuple(..))
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log, log2)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import FFI.Simple (delay)
import Reactix as R
import Gargantext.Utils.Reactix as R2
import Gargantext.Components.LoadingSpinner (loadingSpinner)

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

      R2.affEffect "G.H.Loader.useLoaderEffect2" $ do
        l <- loader path
        liftEffect $ setState $ const $ Just l
