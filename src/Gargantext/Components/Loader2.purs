module Gargantext.Components.Loader2 where

--import Control.Monad.Cont.Trans (lift)
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple.Nested ((/\))
import Gargantext.Prelude
import Effect (Effect)
--import Effect.Aff (Aff)
import Reactix as R

type State path loaded = { currentPath :: path, loaded :: Maybe loaded }

useLoader
  :: forall path loaded
  .  Eq path
  => Show path
  => path
  -> (path -> Effect loaded)
  -> (path -> loaded -> Array R.Element)
  -> R.Hooks (Array R.Element)
useLoader newPath loader render = do
  {currentPath, loaded} /\ setState <- R.useState' { currentPath: newPath, loaded: Nothing }

  -- What about cleanup handlers?
  R.useEffect' $ \_ ->
    when (isNothing loaded || newPath /= currentPath) do
      logs $ "useLoader " <> show {newPath, currentPath, loadedIsNothing: isNothing loaded}
      freshlyLoaded <- loader newPath
      setState { currentPath: newPath, loaded: Just freshlyLoaded }

  pure case loaded of
    Nothing ->
      -- TODO load spinner
      []
    Just loadedData ->
      render currentPath loadedData
