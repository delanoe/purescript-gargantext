module Gargantext.Components.PhyloExplorer.Sidebar.UpdateTerms where

import Gargantext.Prelude

import Effect (Effect)
import Gargantext.Components.App.Store as AppStore
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), ComponentStatus(..), Variant(..))
import Gargantext.Components.PhyloExplorer.Store as PhyloStore
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

here :: R2.Here
here = R2.here "Gargantext.Components.PhyloExplorer.Sidebar.UpdateTerms"

-- | @NOTE #408: only dealing with single Term selection
-- |             (hence not dealing with multiple selection, nor Branch,
-- |             nor Source → if so, please change the source code accordingly)
updateTerms :: R2.Leaf ()
updateTerms = R2.leaf updateTermsCpt

updateTermsCpt :: R.Component ()
updateTermsCpt = here.component "main" cpt where
  cpt {} _ = do
    -- | States
    -- |
    { errors
    , reloadForest
    } <- AppStore.use

    { selectedTerm
    , phyloId
    } <- PhyloStore.use

    selectedTerm' <- R2.useLive' selectedTerm
    phyloId'      <- R2.useLive' phyloId

    -- | Behaviors
    -- |
    let
      callback :: Unit -> Effect Unit
      callback _ = pure unit

    -- | Render
    -- |
    pure $

      B.buttonGroup
      { collapse: false }
      [
        B.button
        { variant: ButtonVariant Light
        , status: Disabled
        , callback
        }
        [
          B.icon
          { name: "circle"
          , className: "mr-1 candidate-term"
          }
        ,
          H.text "Move as candidate"
        ]
      ,
        B.button
        { variant: ButtonVariant Light
        , status: Disabled
        , callback
        }
        [
          B.icon
          { name: "circle"
          , className: "mr-1 stop-term"
          }
        ,
          H.text "Move as stop"
        ]
      ]
