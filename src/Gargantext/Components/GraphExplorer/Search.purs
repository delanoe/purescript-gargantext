module Gargantext.Components.GraphExplorer.Search
  ( Props
  , nodeSearchControl
  ) where

import Global (readFloat)
import Prelude
import Data.Set as Set
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect (Effect)
import FFI.Simple ((..))
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Hooks.Sigmax.Types as SigmaxTypes
import Gargantext.Utils.Reactix as R2

type Props = (
    selectedNodeIds :: R.State SigmaxTypes.SelectedNodeIds
  )

nodeSearchControl :: Record Props -> R.Element
nodeSearchControl props = R.createElement sizeButtonCpt props []

sizeButtonCpt :: R.Component Props
sizeButtonCpt = R.hooksComponent "NodeSearchControl" cpt
  where
    cpt {selectedNodeIds} _ = do
      (search /\ setSearch) <- R.useState' ""

      pure $
        H.span {}
          [ H.input { type: "text"
                    , className: "form-control"
                    , defaultValue: search
                    , on: { input: \e -> setSearch $ const $ e .. "target" .. "value" }
                    }
          , H.button { className: "btn btn-primary"
                     , on: { click: \_ -> log2 "[sizeButtonCpt] search" search }} [ H.text "Search" ]
          ]

-- TODO Wherefrom do I get graph nodes?
-- How to implement filtering here? I want to set selectedNodeIds based on graph data.
