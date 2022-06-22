module Gargantext.Components.NgramsTable.SyncResetButton where

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import FFI.Simple.Functions (delay)
import Gargantext.Core.NgramsTable.Types (CoreAction(..), CoreDispatch, NgramsTablePatch)
import Gargantext.Prelude
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
        hasChangesClass = if hasChanges then "" else " disabled"

        synchronizingClass = if synchronizing' then " disabled" else ""

        resetClick _ = do
          performAction ResetPatches

        synchronizeClick _ = delay unit $ \_ -> do
          T.write_ true synchronizing
          performAction $ Synchronize { afterSync: newAfterSync }

        newAfterSync x = do
          afterSync x
          liftEffect $ T.write_ false synchronizing

      pure $ H.div { className: "btn-toolbar" }
        [ H.div { className: "btn-group mr-2" }
          [ H.button { className: "btn btn-danger " <> hasChangesClass <> synchronizingClass
                     , on: { click: resetClick }
                     } [ H.text "Reset" ]
          ]
        , H.div { className: "btn-group mr-2" }
          [ H.button { className: "btn btn-primary " <> hasChangesClass <> synchronizingClass
                     , on: { click: synchronizeClick }
                     } [ H.text "Sync" ]
          ]
        ]

