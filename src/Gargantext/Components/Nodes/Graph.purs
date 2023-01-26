module Gargantext.Components.Nodes.Graph
  ( node
  ) where

import Gargantext.Prelude

import Data.Array as A
import Data.Int as I
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Sequence as Seq
import Data.Tuple (Tuple(..))
import DOM.Simple (document, querySelector)
import Gargantext.Components.App.Store as AppStore
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.GraphExplorer.API as GraphAPI
import Gargantext.Components.GraphExplorer.Layout (convert, layout)
import Gargantext.Components.GraphExplorer.Store as GraphStore
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Config.REST (logRESTError)
import Gargantext.Hooks.FirstEffect (useFirstEffect')
import Gargantext.Hooks.Loader (useLoaderEffect)
import Gargantext.Hooks.Session (useSession)
import Gargantext.Hooks.Sigmax.ForceAtlas2 as ForceAtlas
import Gargantext.Hooks.Sigmax.Noverlap as Noverlap
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Utils (getter)
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Record as Record
import Toestand as T

type Props =
  ( graphId :: GET.GraphId
  )

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Graph"

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
    state <- T.useBox Nothing
    cache <- T.useBox (GET.defaultCacheParams :: GET.CacheParams)

    -- | Computed
    -- |
    let errorHandler = logRESTError here "[node]"

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
    pure $ renderNode { cache, graphId, state }

type RenderNodeProps = (
  cache   :: T.Box GET.CacheParams,
  graphId :: GET.GraphId,
  state   :: T.Box (Maybe GET.HyperdataGraph)
)

renderNode :: R2.Leaf RenderNodeProps
renderNode = R2.leaf renderNodeCpt
renderNodeCpt :: R.Component RenderNodeProps
renderNodeCpt = here.component "renderNode" cpt where
  cpt { cache, graphId, state } _ = do
    cache' <- T.useLive T.unequal cache
    state' <- T.useLive T.unequal state

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
              { cacheParams: cache'
              , graph
              , graphId
              , hyperdataGraph: loaded
              , mMetaData
              }
      }

--------------------------------------------------------

type HydrateStoreProps =
  ( cacheParams     :: GET.CacheParams
  , graph           :: SigmaxT.SGraph
  , graphId         :: GET.GraphId
  , hyperdataGraph  :: GET.HyperdataGraph
  , mMetaData       :: Maybe GET.MetaData
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
          --then SigmaxT.InitialLoading
          then SigmaxT.InitialRunning
          else SigmaxT.InitialStopped

    -- | Hooks
    -- |

    sigmaRef <- Sigmax.initSigma >>= R.useRef
    fa2Ref <- R.useRef (Nothing :: Maybe ForceAtlas.FA2Layout)
    noverlapRef <- R.useRef (Nothing :: Maybe Noverlap.NoverlapLayout)

    -- | Precompute some values
    -- |

    let edgesConfluenceSorted = A.sortWith (_.confluence) $ Seq.toUnfoldable $ SigmaxT.graphEdges graph
    let edgeConfluenceMin = maybe 0.0 _.confluence $ A.head edgesConfluenceSorted
    let edgeConfluenceMax = maybe 100.0 _.confluence $ A.last edgesConfluenceSorted
    let edgeConfluenceRange = Range.Closed { min: edgeConfluenceMin, max: edgeConfluenceMax }

    --let edgesWeightSorted = A.sortWith (_.weight) $ Seq.toUnfoldable $ SigmaxT.graphEdges graph
    --let edgeWeightMin = maybe 0.0 _.weight $ A.head edgesWeightSorted
    --let edgeWeightMax = maybe 100.0 _.weight $ A.last edgesWeightSorted
    --let edgeWeightRange = Range.Closed { min: edgeWeightMin, max: edgeWeightMax }
    -- let edgeWeightRange = Range.Closed {
    --       min: 0.0
    --     , max: I.toNumber $ Seq.length $ SigmaxT.graphEdges graph
    --     }

    let nodesSorted = A.sortWith (_.size) $ Seq.toUnfoldable $ SigmaxT.graphNodes graph
    let nodeSizeMin = maybe 0.0 _.size $ A.head nodesSorted
    let nodeSizeMax = maybe 100.0 _.size $ A.last nodesSorted
    let nodeSizeRange = Range.Closed { min: nodeSizeMin, max: nodeSizeMax }


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
      , noverlapState: SigmaxT.NoverlapPaused
      , edgeWeight:  Range.Closed
          { min: 0.0
          , max: I.toNumber $ Seq.length $ SigmaxT.graphEdges graph
          }
      , edgeConfluenceRange
      , nodeSizeRange
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
        , noverlapRef
        , sigmaRef }
      ]
