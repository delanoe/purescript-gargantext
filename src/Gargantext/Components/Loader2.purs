module Gargantext.Components.Loader2 where

import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple.Nested ((/\))
import Gargantext.Prelude
import Effect.Aff (Aff, launchAff, launchAff_, killFiber)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Reactix as R

type State path loaded = { currentPath :: path, loaded :: Maybe loaded }

useLoader
  :: forall path loaded
  .  Eq path
  => Show path
  => path
  -> (path -> Aff loaded)
  -> (path -> loaded -> R.Element)
  -> R.Hooks R.Element
useLoader newPath loader render = do
  {currentPath, loaded} /\ setState <- R.useState' { currentPath: newPath, loaded: Nothing }

  R.useEffect $ \_ ->
    if (isNothing loaded || newPath /= currentPath) then do
      logs $ "useLoader " <> show {newPath, currentPath, loadedIsNothing: isNothing loaded}

      fiber <- launchAff do
        freshlyLoaded <- loader newPath
        liftEffect $ setState { currentPath: newPath, loaded: Just freshlyLoaded }
      pure $ \_ -> launchAff_ $ killFiber (error "useLoader") fiber
    else do
      pure $ \_ -> pure unit

  pure case loaded of
    Nothing ->
      -- TODO load spinner
      R.fragment []
    Just loadedData ->
      render currentPath loadedData
