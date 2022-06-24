module Gargantext.Components.Nodes.Corpus.Phylo
  ( node
  ) where

import Gargantext.Prelude

import DOM.Simple (document, querySelector)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Tuple.Nested ((/\))
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.PhyloExplorer.API (get)
import Gargantext.Components.PhyloExplorer.Layout (layout)
import Gargantext.Components.PhyloExplorer.Store as PhyloStore
import Gargantext.Components.PhyloExplorer.Types (CacheParams, PhyloSet(..), defaultCacheParams)
import Gargantext.Config.REST (logRESTError)
import Gargantext.Hooks.FirstEffect (useFirstEffect')
import Gargantext.Hooks.Loader (useLoaderEffect)
import Gargantext.Hooks.Session (useSession)
import Gargantext.Types (NodeID)
import Gargantext.Utils (getter)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record

type MainProps =
  ( nodeId      :: NodeID
  )

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Phylo"

node :: R2.Leaf MainProps
node = R2.leaf nodeCpt

nodeCpt :: R.Component MainProps
nodeCpt = here.component "node" cpt where
  cpt { nodeId } _ = do
    -- | States
    -- |
    session <- useSession

    state' /\ state <- R2.useBox' (Nothing :: Maybe PhyloSet)
    cache' /\ cache <- R2.useBox' (defaultCacheParams :: CacheParams)

    -- | Computed
    -- |
    let errorHandler = logRESTError here "[phylo]"

    -- | Hooks
    -- |

    -- load Local Storage cache (if exists)
    useFirstEffect' $
      R2.loadLocalStorageState R2.phyloParamsKey cache

    useLoaderEffect
      { errorHandler
      , loader: get session
      , path: nodeId
      , state
      }

    -- @XXX: reset "main-page__main-route" wrapper margin
    --       see Gargantext.Components.Router) (@TODO?)
    R.useLayoutEffect1 [] do
      let mEl = querySelector document ".main-page__main-route"
      -- Mount
      mEl >>= maybe R.nothing (flip R2.addClass ["p-0"])
      -- Unmount
      pure $
        mEl >>= maybe R.nothing (flip R2.removeClass ["p-0"])


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
          R2.fromMaybe state' \(PhyloSet { corpusId, listId, phyloData }) ->

            let
              state_ :: Record PhyloStore.State
              state_ =
                -- Data
                { phyloData
                , corpusId
                , listId
                , phyloId: nodeId
                -- (cache params)
                , expandSelection: getter _.expandSelection cache'
                , expandNeighborhood: getter _.expandNeighborhood cache'
                -- (default options)
                } `Record.merge` PhyloStore.options

            in
              PhyloStore.provide
              state_
              [
                layout
                {}
              ]
      }
