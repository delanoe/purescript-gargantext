module Gargantext.Folder where

import Prelude
import Reactix as R
import Reactix.DOM.HTML as H
import Thermite (Spec)

import Gargantext.Utils.Reactix as R2

-- TODO : get REST informations

layoutFolder :: Spec {} {} Void
layoutFolder = R2.elSpec $ R.hooksComponent "LayoutFolder" cpt
  where
    cpt {} _ = do
      pure $ H.span {} [
          H.h1 {} [ H.text "Folder" ]
        , H.text "Some description of the folder here"
        ]
