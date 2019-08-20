module Gargantext.Pages.Corpus where

import Reactix as R
import Reactix.DOM.HTML as H
import Thermite (Spec)
--------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Utils.Reactix as R2

------------------------------------------------------------------------
layout :: Spec {} {nodeId :: Int} Void
layout = R2.elSpec $ R.hooksComponent "CorpusLoader" cpt
  where
    cpt {nodeId} _children = do
      pure $ H.div {} [H.text "Empty page"]
