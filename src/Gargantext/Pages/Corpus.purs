module Gargantext.Pages.Corpus where

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Reactix as R
import Reactix.DOM.HTML as H
import Thermite (Spec)
--------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Components.Node (NodePoly(..), HyperdataList)
import Gargantext.Components.Loader2 (useLoader)
import Gargantext.Components.Table as Table
import Gargantext.Config      (toUrl, Path(..), NodeType(..), End(..))
import Gargantext.Config.REST (get)
import Gargantext.Pages.Texts.Tabs.Types (CorpusData, CorpusInfo(..))
import Gargantext.Pages.Texts.Tabs.Specs (elt) as Tabs
import Gargantext.Utils.Reactix as R2

------------------------------------------------------------------------
layout :: Spec {} {nodeId :: Int} Void
layout = R2.elSpec $ R.hooksComponent "CorpusLoader" cpt
  where
    cpt {nodeId} _children = do
      pure $ H.div {} [H.text "Soon: corpus synthesis here (when all others charts/featurs will be stabilized)."]
