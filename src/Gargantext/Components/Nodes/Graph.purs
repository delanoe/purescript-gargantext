module Gargantext.Components.Nodes.Corpus.Graph
  ( graphLayout
  ) where

import Gargantext.Prelude

import DOM.Simple (document, querySelector)
import Data.Int as I
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Sequence as Seq
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.GraphExplorer.API as GraphAPI
import Gargantext.Components.GraphExplorer.Layout (convert, layout)
import Gargantext.Components.GraphExplorer.Store as GraphStore
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Config.REST (logRESTError)
import Gargantext.Hooks.Loader (useLoaderEffect)
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Sessions (Session)
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record


type Props =
  ( key       :: String
  , session   :: Session
  , boxes     :: Boxes
  , graphId   :: GET.GraphId
  )

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Graph"

graphLayout :: R2.Leaf Props
graphLayout = R2.leaf graphLayoutCpt

graphLayoutCpt :: R.Component Props
graphLayoutCpt = here.component "explorerLayout" cpt where
  cpt props@{ boxes: { graphVersion }, graphId, session } _ = do
    -- | States
    -- |
    graphVersion'   <- R2.useLive' graphVersion
    state' /\ state <- R2.useBox' Nothing


    -- | Hooks
    -- |
    useLoaderEffect
      { errorHandler
      , loader: GraphAPI.getNodes session graphVersion'
      , path: graphId
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
        Just el -> R2.addClass el [ "p-0" ]

    R.useEffectOnce do
      pure do
        mEl <- querySelector document ".main-page__main-route"

        case mEl of
          Nothing -> R.nothing
          Just el -> R2.removeClass el [ "p-0" ]

    -- | Render
    -- |
    pure $

      B.cloak
      { isDisplayed: isJust state'
      , idlingPhaseDuration: Just 150
      , cloakSlot:
          H.div
          { className: "graph-loader" }
          [
            B.spinner
            { className: "graph-loader__spinner" }
          ]
      , defaultSlot:
          R2.fromMaybe_ state' handler
      }

    where
      errorHandler = logRESTError here "[explorerLayout]"
      handler loaded@(GET.HyperdataGraph { graph: hyperdataGraph }) =
        initGraph { graph
                , hyperdataGraph: loaded
                , mMetaData
                , session
                , boxes: props.boxes
                , graphId
                }
        where
          Tuple mMetaData graph = convert hyperdataGraph

--------------------------------------------------------

type InitGraphProps =
  ( mMetaData       :: Maybe GET.MetaData
  , graph           :: SigmaxT.SGraph
  , hyperdataGraph  :: GET.HyperdataGraph
  , session         :: Session
  , boxes           :: Boxes
  , graphId         :: GET.GraphId
  )

initGraph :: R2.Leaf InitGraphProps
initGraph = R2.leaf initGraphCpt

initGraphCpt :: R.Component InitGraphProps
initGraphCpt = here.component "initGraph" cpt where
  cpt { boxes
      , mMetaData
      , graph
      , graphId
      , session
      , hyperdataGraph
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
      -- (default options)
      } `Record.merge` GraphStore.options

    -- | Render
    -- |

    pure $

      GraphStore.provide
      state
      [
        layout
        { session
        , boxes
        , sigmaRef
        }
      ]
