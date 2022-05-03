module Gargantext.Components.Nodes.Frame
  ( frameLayout
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Frame.Layout (layout)
import Gargantext.Components.Frame.Types (Hyperdata)
import Gargantext.Components.Node (NodePoly)
import Gargantext.Config.REST (AffRESTError, logRESTError)
import Gargantext.Hooks.Loader (useLoaderEffect)
import Gargantext.Hooks.Session (useSession)
import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, get)
import Gargantext.Types (NodeType(..))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R

type Props =
  ( nodeId   :: Int
  , nodeType :: NodeType
  )

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Frame"

frameLayout :: R2.Leaf ( key :: String | Props )
frameLayout = R2.leaf frameLayoutCpt

frameLayoutCpt :: R.Component ( key :: String | Props )
frameLayoutCpt = here.component "main" cpt where
  cpt { nodeId
      , nodeType
      } _ = do
    -- | States
    -- |
    session <- useSession

    state'  /\ state  <- R2.useBox' Nothing
    reload' /\ reload <- R2.useBox' T2.newReload

    -- | Hooks
    -- |
    useLoaderEffect
      { errorHandler: logRESTError here "[frameLayoutWithKey]"
      , loader: loadframeWithReload
      , path:
          { nodeId
          , reload: reload'
          , session
          }
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
          R2.fromMaybe_ state' \frame ->
            layout
            { frame
            , nodeId
            , reload
            , nodeType
            }
      }



-----------------------------------------------------------

-- @WIP: test reload

type LoadProps =
  ( nodeId  :: Int
  , session :: Session
  )

type ReloadProps =
  ( reload :: T2.Reload
  | LoadProps
  )

-- Just to make reloading effective
loadframeWithReload :: Record ReloadProps -> AffRESTError (NodePoly Hyperdata)
loadframeWithReload { nodeId, session } = loadframe { nodeId, session }

loadframe :: Record LoadProps -> AffRESTError (NodePoly Hyperdata)
loadframe { nodeId, session } = get session $ NodeAPI Node (Just nodeId) ""
