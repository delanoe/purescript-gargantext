module Gargantext.Components.NgramsTable.SelectionCheckbox where

import Data.Maybe (Maybe(..))
import Data.Nullable (null, toMaybe)
import Data.Set (Set)
import Data.Set as Set
import FFI.Simple (delay)
import Gargantext.Core.NgramsTable.Types (Action(..), Dispatch, NgramsTerm)
import Gargantext.Prelude
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

here :: R2.Here
here = R2.here "Gargantext.Components.NgramsTable.SelectionCheckbox"


type SelectionCheckboxProps =
  ( allNgramsSelected :: Boolean
  , dispatch          :: Dispatch
  , ngramsSelection   :: Set NgramsTerm
  )

selectionCheckbox :: Record SelectionCheckboxProps -> R.Element
selectionCheckbox props = R.createElement selectionCheckboxCpt props []
selectionCheckboxCpt :: R.Component SelectionCheckboxProps
selectionCheckboxCpt = here.component "selectionCheckbox" cpt
  where
    cpt { allNgramsSelected, dispatch, ngramsSelection } _ = do
      ref <- R.useRef null

      R.useEffect' $ delay unit $ \_ -> do
        let mCb = toMaybe $ R.readRef ref
        case mCb of
          Nothing -> pure unit
          Just cb -> do
            _ <- if allNgramsSelected || (Set.isEmpty ngramsSelection) then
              R2.setIndeterminateCheckbox cb false
            else
              R2.setIndeterminateCheckbox cb true
            pure unit

      pure $ H.input { checked: allNgramsSelected
                     , className: "checkbox"
                     , on: { change: const $ dispatch $ ToggleSelectAll }
                     , ref
                     , type: "checkbox" }
