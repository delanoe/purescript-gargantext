module Gargantext.Components.NgramsTable.SyncResetButton where

import Gargantext.Prelude

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import FFI.Simple.Functions (delay)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), ComponentStatus(..), Variant(..))
import Gargantext.Core.NgramsTable.Types (CoreAction(..), CoreDispatch, NgramsTablePatch)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.NgramsTable.SyncResetButton"


-- | Reset Button
type SyncResetButtonsProps =
  ( afterSync        :: Unit -> Aff Unit
  , ngramsLocalPatch :: NgramsTablePatch
  , performAction    :: CoreDispatch
  )

syncResetButtons :: Record SyncResetButtonsProps -> R.Element
syncResetButtons p = R.createElement syncResetButtonsCpt p []
syncResetButtonsCpt :: R.Component SyncResetButtonsProps
syncResetButtonsCpt = here.component "syncResetButtons" cpt
  where
    cpt { afterSync, ngramsLocalPatch, performAction } _ = do
      synchronizing <- T.useBox false
      synchronizing' <- T.useLive T.unequal synchronizing

      let
        hasChanges = ngramsLocalPatch /= mempty

        statusReset _     true = Disabled
        statusReset false _ = Disabled
        statusReset _     _ = Enabled

        statusSync _     true = Deferred
        statusSync false _    = Disabled
        statusSync _     _    = Enabled

        resetClick _ = do
          performAction ResetPatches

        synchronizeClick _ = delay unit $ \_ -> do
          T.write_ true synchronizing
          performAction $ Synchronize { afterSync: newAfterSync }

        newAfterSync x = do
          afterSync x
          liftEffect $ T.write_ false synchronizing

      pure $

        B.wad
        [ "d-flex" ]
        [
          B.button
          { variant: ButtonVariant Light
          , callback: resetClick
          , status: statusReset hasChanges synchronizing'
          }
          [ H.text "Reset" ]
        ,
          B.wad_ [ "mr-1", "d-inline-block" ]
        ,
          B.button
          { variant: ButtonVariant Primary
          , callback: synchronizeClick
          , status: statusSync hasChanges synchronizing'
          }
          [ H.text "Save (Sync)" ]
        ]
