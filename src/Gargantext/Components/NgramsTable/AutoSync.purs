module Gargantext.Components.NgramsTable.AutoSync where

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Gargantext.Core.NgramsTable.Types (CoreAction(..), CoreDispatch, CoreState)
import Gargantext.Prelude
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Toestand as T

------------------------------------------------------------------

here :: R2.Here
here = R2.here "Gargantext.Components.NgramsTable.AutoSync"


type AutoSyncInput s =
  ( state  :: T.Box (CoreState s)
  , action :: CoreDispatch
  )

type AutoSyncOutput =
  -- @XXX: cannot use an Either here due to the mecanism of `syncPatches` only
  --       returning an `Aff Unit`
  -- ( result :: T.Box (Maybe (Either RESTError Unit))
  ( result    :: T.Box (Maybe Unit)
  , onPending :: T.Box Boolean
  )

useAutoSync :: forall s.
     Record (AutoSyncInput s)
  -> R.Hooks (Record AutoSyncOutput)
useAutoSync { state, action } = do
  -- States
  onPending <- T.useBox false
  result    <- T.useBox Nothing

  ngramsLocalPatch <-
    T.useFocused
      (_.ngramsLocalPatch)
      (\a b -> b { ngramsLocalPatch = a }) state

  -- Computed
  let
    exec { new } =
      let hasChanges = new /= mempty
      in when hasChanges do
        T.write_ true onPending
        T.write_ Nothing result
        action $ Synchronize
          { afterSync: onSuccess
          }

    onSuccess _ = liftEffect do
      T.write_ false onPending
      T.write_ (Just unit) result

  -- Hooks
  R.useEffectOnce' $ T.listen exec ngramsLocalPatch

  -- Output
  pure
    { onPending
    , result
    }
