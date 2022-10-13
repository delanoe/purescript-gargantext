module Gargantext.Components.Nodes.Corpus.Graph
  ( node
  ) where

import Gargantext.Prelude

import DOM.Simple (document, querySelector)
import Data.Int as I
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Sequence as Seq
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Gargantext.Components.App.Store as AppStore
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.GraphExplorer.API as GraphAPI
import Gargantext.Components.GraphExplorer.GraphTypes as GEGT
import Gargantext.Components.GraphExplorer.Layout (convert, layout)
import Gargantext.Components.GraphExplorer.Store as GraphStore
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Config.REST (logRESTError)
import Gargantext.Hooks.FirstEffect (useFirstEffect')
import Gargantext.Hooks.Loader (useLoaderEffect)
import Gargantext.Hooks.Session (useSession)
import Gargantext.Hooks.Sigmax.ForceAtlas2 as ForceAtlas
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Utils (getter)
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Record as Record

type Props =
  ( graphId :: GET.GraphId
  )

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Graph"

node :: R2.Leaf ( key :: String | Props )
node = R2.leaf nodeCpt
nodeCpt :: R.Component ( key :: String | Props )
nodeCpt = here.component "node" cpt where
  cpt { graphId } _ = do
    -- | States
    -- |
    { graphVersion
    } <- AppStore.use

    session <- useSession

    graphVersion'   <- R2.useLive' graphVersion
    state' /\ state <- R2.useBox' Nothing
    cache' /\ cache <- R2.useBox' (GET.defaultCacheParams :: GET.CacheParams)

    -- | Computed
    -- |
    let errorHandler = logRESTError here "[explorerLayout]"

    -- | Hooks
    -- |

    -- load Local Storage cache (if exists)
    useFirstEffect' $
      R2.loadLocalStorageState R2.graphParamsKey cache

    useLoaderEffect
      { errorHandler
      , loader: GraphAPI.getNodes session graphVersion'
      , path: graphId
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
          B.preloader
          {}

      , defaultSlot:
          R2.fromMaybe state' \loaded ->
            let
              GET.HyperdataGraph { graph: hyperdataGraph } = loaded
              Tuple mMetaData graph = convert hyperdataGraph
            in
              hydrateStore
              { graph
              , hyperdataGraph: loaded
              , mMetaData
              , graphId
              , cacheParams: cache'
              }
      }

--------------------------------------------------------

type HydrateStoreProps =
  ( mMetaData       :: Maybe GET.MetaData
  , graph           :: SigmaxT.SGraph
  , hyperdataGraph  :: GET.HyperdataGraph
  , graphId         :: GET.GraphId
  , cacheParams     :: GET.CacheParams
  )

hydrateStore :: R2.Leaf HydrateStoreProps
hydrateStore = R2.leaf hydrateStoreCpt
hydrateStoreCpt :: R.Component HydrateStoreProps
hydrateStoreCpt = here.component "hydrateStore" cpt where
  cpt { mMetaData
      , graph
      , graphId
      , hyperdataGraph
      , cacheParams
      } _ = do
    -- | Computed
    -- |
    let
      startForceAtlas = maybe true
        (\(GET.MetaData { startForceAtlas: sfa }) -> sfa) mMetaData

      forceAtlasState
        = if startForceAtlas
          then SigmaxT.InitialRunning
          else SigmaxT.InitialStopped

    -- | Hooks
    -- |

    sigmaRef <- Sigmax.initSigma >>= R.useRef
    fa2Ref <- R.useRef (Nothing :: Maybe ForceAtlas.FA2Layout)

    -- Hydrate GraphStore
    (state :: Record GraphStore.State) <- pure $
      -- Data
      { graph
      , graphId
      , mMetaData
      , hyperdataGraph
      -- Controls
      , startForceAtlas
      , forceAtlasState
      , edgeWeight:  Range.Closed
          { min: 0.0
          , max: I.toNumber $ Seq.length $ SigmaxT.graphEdges graph
          }
      -- (cache options)
      , expandSelection: getter _.expandSelection cacheParams
      , expandNeighborhood: getter _.expandNeighborhood cacheParams
      -- (default options)
      } `Record.merge` GraphStore.options

    -- | Render
    -- |
    pure $

      GraphStore.provide
      state
      [
        layout
        { fa2Ref
        , sigmaRef }
      ]
