module Gargantext.Components.Nodes.Corpus where

import Gargantext.Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Corpus.Layout (layout)
import Gargantext.Components.GraphQL.Endpoints (getNode)
import Gargantext.Config.REST (logRESTError)
import Gargantext.Hooks.Loader (useLoaderEffect)
import Gargantext.Hooks.Session (useSession)
import Gargantext.Types (ID)
import Gargantext.Utils.Reactix as R2
import Reactix as R

type Props =
  ( nodeId  :: ID
  )

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus"

node :: R2.Leaf ( key :: String | Props )
node = R2.leaf nodeCpt
nodeCpt :: R.Component ( key :: String | Props )
nodeCpt = here.component "node" cpt where
  cpt { nodeId } _ = do
    -- | States
    -- |
    session <- useSession

    state' /\ state <- R2.useBox' Nothing

    -- | Computed
    -- |
    let
      errorHandler = logRESTError here "[corpusLayout]"

      loader { nodeId: nodeId_, session: session_ } = getNode session_ nodeId_

    -- | Hooks
    -- |
    useLoaderEffect
      { errorHandler
      , loader
      , path: { nodeId, session }
      , state
      }

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
          R2.fromMaybe state' \nodeData ->

            layout
            { nodeId
            , nodeData
            }
      }
