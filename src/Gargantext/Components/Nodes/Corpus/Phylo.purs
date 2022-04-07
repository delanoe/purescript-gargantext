module Gargantext.Components.Nodes.Corpus.Phylo
  ( phyloLayout
  ) where

import Gargantext.Prelude

import DOM.Simple (document, querySelector)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.PhyloExplorer.API (get)
import Gargantext.Components.PhyloExplorer.Layout (layout)
import Gargantext.Components.PhyloExplorer.Types (PhyloDataSet)
import Gargantext.Config.REST (logRESTError)
import Gargantext.Hooks.Loader (useLoaderEffect)
import Gargantext.Sessions (Session)
import Gargantext.Types (NodeID)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

type MainProps =
  ( nodeId      :: NodeID
  , session     :: Session
  , boxes       :: Boxes
  )

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Phylo"

phyloLayout :: R2.Leaf MainProps
phyloLayout = R2.leaf phyloLayoutCpt

phyloLayoutCpt :: R.Component MainProps
phyloLayoutCpt = here.component "main" cpt where
  cpt { nodeId, session } _ = do

    -- | States
    -- |

    state' /\ state <- R2.useBox' Nothing


    -- | Computed
    -- |
    let

      errorHandler = logRESTError here "[phylo]"

      handler (phyloDataSet :: PhyloDataSet) =
        layout
        { nodeId
        , phyloDataSet
        }


    -- | Hooks
    -- |

    useLoaderEffect
      { errorHandler
      , loader: get session
      , path: nodeId
      , state
      }

    -- @XXX: Runtime odd behavior
    --       cannot use the `useEffect` + its cleanup function within the
    --       same `Effect`, otherwise the below cleanup example will be
    --       execute at mount

    -- @XXX: inopinent <div> (see Gargantext.Components.Router) (@TODO?)
    R.useEffectOnce' do
      mEl <- querySelector document ".main-page__main-route .container"

      case mEl of
        Nothing -> R.nothing
        Just el -> R2.addClass el [ "d-none" ]

    R.useEffectOnce do
      pure do
        mEl <- querySelector document ".main-page__main-route .container"

        case mEl of
          Nothing -> R.nothing
          Just el -> R2.removeClass el [ "d-none" ]

    -- @XXX: reset "main-page__main-route" wrapper margin
    --       see Gargantext.Components.Router) (@TODO?)
    R.useEffectOnce' do
      mEl <- querySelector document ".main-page__main-route"

      case mEl of
        Nothing -> R.nothing
        Just el -> R2.addClass el [ "p-0--i" ]

    R.useEffectOnce do
      pure do
        mEl <- querySelector document ".main-page__main-route"

        case mEl of
          Nothing -> R.nothing
          Just el -> R2.removeClass el [ "p-0--i" ]


    -- | Render
    -- |
    pure $

      B.cloak
      { isDisplayed: isJust state'
      , idlingPhaseDuration: Just 150
      , cloakSlot:
          -- mimicking `PhyloExplorer.layout` preloading template
          H.div
          { className: "phylo" }
          [
            H.div
            { className: "phylo__spinner-wrapper" }
            [
              B.spinner
              { className: "phylo__spinner" }
            ]
          ]
      , defaultSlot:
          R2.fromMaybe_ state' handler
      }
