module Gargantext.Components.Nodes.Frame where

import Gargantext.Prelude

import DOM.Simple as DOM
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, null, toMaybe)
import Data.Show.Generic (genericShow)
import Gargantext.Components.FolderView as FV
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Config.REST (AffRESTError, logRESTError)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, get, sessionId)
import Gargantext.Types (NodeType(..))
import Gargantext.Utils.JitsiMeet as JM
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON
import Toestand as T
import Web.URL as WURL

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Frame"

newtype Hyperdata = Hyperdata { base :: String, frame_id :: String }
derive instance Generic Hyperdata _
derive instance Newtype Hyperdata _
instance Eq Hyperdata where eq = genericEq
instance Show Hyperdata where show = genericShow
derive newtype instance JSON.ReadForeign Hyperdata
derive newtype instance JSON.WriteForeign Hyperdata

type Props =
  ( nodeId   :: Int
  , nodeType :: NodeType
  , session  :: Session
  )

type KeyProps =
  ( key      :: String
  | Props
  )

frameLayout :: R2.Leaf Props
frameLayout = R2.leafComponent frameLayoutCpt
frameLayoutCpt :: R.Component Props
frameLayoutCpt = here.component "frameLayout" cpt where
  cpt { nodeId, nodeType, session } _ = do
    pure $ frameLayoutWithKey { key, nodeId, nodeType, session }
      where
        key = show (sessionId session) <> "-" <> show nodeId

frameLayoutWithKey :: R2.Leaf KeyProps
frameLayoutWithKey = R2.leafComponent frameLayoutWithKeyCpt
frameLayoutWithKeyCpt :: R.Component KeyProps
frameLayoutWithKeyCpt = here.component "frameLayoutWithKey" cpt where
  cpt { nodeId, session, nodeType} _ = do
    reload <- T.useBox T2.newReload
    reload' <- T.useLive T.unequal reload
    useLoader { errorHandler
              , loader: loadframeWithReload
              , path: {nodeId, reload: reload', session}
              , render: \frame -> frameLayoutView {frame, nodeId, reload, session, nodeType} }
    where
      errorHandler = logRESTError here "[frameLayoutWithKey]"

type ViewProps =
  ( frame    :: NodePoly Hyperdata
  , reload   :: T2.ReloadS
  , nodeId   :: Int
  , nodeType :: NodeType
  , session  :: Session
  )

type Base = String

type FrameId = String

hframeUrl :: NodeType -> Base -> FrameId -> String
hframeUrl NodeFrameNotebook _ frame_id = frame_id  -- Temp fix : frame_id is currently the whole url created
hframeUrl NodeFrameCalc  base frame_id = base <> "/" <> frame_id
hframeUrl NodeFrameVisio base frame_id = base <> "/" <> frame_id
hframeUrl _ base frame_id = base <> "/" <> frame_id <> "?view" -- "?both"

frameLayoutView :: R2.Leaf ViewProps
frameLayoutView = R2.leafComponent frameLayoutViewCpt
frameLayoutViewCpt :: R.Component ViewProps
frameLayoutViewCpt = here.component "frameLayoutView" cpt
  where
    cpt { frame: NodePoly { hyperdata: h@(Hyperdata { base, frame_id }) }
        , nodeId
        , nodeType
        , reload
        , session } _ = do
      case nodeType of
        NodeFrameVisio ->
          case WURL.fromAbsolute base of
            Nothing  -> pure $ H.div {} [ H.text $ "Wrong base url: " <> base ]
            Just url -> pure $ nodeFrameVisio { frame_id, reload, url }
        _              ->
          pure $ H.div{}
            [ FV.backButton {} []
            , H.div { className : "frame"
                    , rows: "100%,*" }
              [ -- H.script { src: "https://visio.gargantext.org/external_api.js"} [],
                H.iframe { src: hframeUrl nodeType base frame_id
                         , width: "100%"
                         , height: "100%"
                         } []
              ]
            ]

type NodeFrameVisioProps =
  ( frame_id  :: String
  , reload    :: T2.ReloadS
  , url       :: WURL.URL
  )

nodeFrameVisio :: R2.Leaf NodeFrameVisioProps
nodeFrameVisio = R2.leafComponent nodeFrameVisioCpt
nodeFrameVisioCpt :: R.Component NodeFrameVisioProps
nodeFrameVisioCpt = here.component "nodeFrameVisio" cpt
  where
    cpt { frame_id
        , url } _  = do
      ref <- R.useRef (null :: Nullable DOM.Element)

      R.useEffect' $ do
        here.log2 "[nodeFrameVisio] ref" $ R.readRef ref
        here.log2 "[nodeFrameVisio] JM.api" JM._api
        case toMaybe (R.readRef ref) of
          Nothing -> pure unit
          Just r  -> do
            api <- JM.jitsiMeetAPI (WURL.host url) { parentNode: r, roomName: frame_id }
            here.log2 "[nodeFrameVisio] api" api

      pure $ H.div { ref } [ H.text $ WURL.host url ]

type LoadProps   = ( nodeId  :: Int
                   , session :: Session )

type ReloadProps = ( nodeId  :: Int
                   , reload :: T2.Reload
                   , session :: Session )

loadframe' :: Record LoadProps -> AffRESTError (NodePoly Hyperdata)
loadframe' { nodeId, session } = get session $ NodeAPI Node (Just nodeId) ""

-- Just to make reloading effective
loadframeWithReload :: Record ReloadProps -> AffRESTError (NodePoly Hyperdata)
loadframeWithReload { nodeId, session } = loadframe' { nodeId, session }
