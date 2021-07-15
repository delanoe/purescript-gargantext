module Gargantext.Components.Nodes.Frame where

import Gargantext.Prelude

import Data.Argonaut (decodeJson, (.:))
import Data.Argonaut as Argonaut
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, null)
import Data.Show.Generic (genericShow)
import DOM.Simple as DOM
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON
import Toestand as T
import Web.URL as WURL

import Gargantext.Components.FolderView as FV
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, get, sessionId)
import Gargantext.Types (NodeType(..))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

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
frameLayout props = R.createElement frameLayoutCpt props []
frameLayoutCpt :: R.Component Props
frameLayoutCpt = here.component "frameLayout" cpt where
  cpt { nodeId, nodeType, session } _ = do
    pure $ frameLayoutWithKey { key, nodeId, nodeType, session }
      where
        key = show (sessionId session) <> "-" <> show nodeId

frameLayoutWithKey :: R2.Leaf KeyProps
frameLayoutWithKey props = R.createElement frameLayoutWithKeyCpt props []
frameLayoutWithKeyCpt :: R.Component KeyProps
frameLayoutWithKeyCpt = here.component "frameLayoutWithKey" cpt where
  cpt { nodeId, session, nodeType} _ = do
    reload <- T.useBox T2.newReload
    reload' <- T.useLive T.unequal reload
    useLoader {nodeId, reload: reload', session} loadframeWithReload $
      \frame -> frameLayoutView {frame, nodeId, reload, session, nodeType}

type ViewProps =
  ( frame    :: NodePoly Hyperdata
  , reload   :: T2.ReloadS
  , nodeId   :: Int
  , session  :: Session
  , nodeType :: NodeType
  )

type Base = String

type FrameId = String

hframeUrl :: NodeType -> Base -> FrameId -> String
hframeUrl NodeFrameNotebook _ frame_id = frame_id  -- Temp fix : frame_id is currently the whole url created
hframeUrl NodeFrameCalc  base frame_id = base <> "/" <> frame_id
hframeUrl NodeFrameVisio base frame_id = base <> "/" <> frame_id
hframeUrl _ base frame_id = base <> "/" <> frame_id <> "?view" -- "?both"

frameLayoutView :: R2.Leaf ViewProps
frameLayoutView props  = R.createElement frameLayoutViewCpt props []
frameLayoutViewCpt :: R.Component ViewProps
frameLayoutViewCpt = here.component "frameLayoutView" cpt
  where
    cpt { frame: NodePoly { hyperdata: Hyperdata { base, frame_id }}
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
          pure $ H.div{} [
            FV.backButton
            , FV.homeButton
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
nodeFrameVisio props = R.createElement nodeFrameVisioCpt props []
nodeFrameVisioCpt :: R.Component NodeFrameVisioProps
nodeFrameVisioCpt = here.component "nodeFrameVisio" cpt
  where
    cpt { frame_id
        , reload
        , url } _  = do
      -- api = new JitsiMeetExternalAPI("visio.gargantext.org", {roomName: frame_id})
      ref <- R.useRef (null :: Nullable DOM.Element)

      R.useEffect' $ do
        here.log2 "[nodeFrameVisio] ref" $ R.readRef ref
      
      pure $ H.div { ref } [ nodeFrameVisioPlaceholder { frame_id, ref, reload, url } ]

type NodeFrameVisioPlaceholderProps =
  ( ref :: R.Ref (Nullable DOM.Element)
  | NodeFrameVisioProps
  )

nodeFrameVisioPlaceholder :: R2.Leaf NodeFrameVisioPlaceholderProps
nodeFrameVisioPlaceholder props = R.createElement nodeFrameVisioPlaceholderCpt props []
nodeFrameVisioPlaceholder :: R.Component NodeFrameVisioPlaceholderProps
nodeFrameVisioPlaceholder = here.component "nodeFrameVisioPlaceholder" cpt
  where
    cpt { frame_id
        , ref
        , url } _ = do
      R.useEffect' $ do
        here.log2 "[nodeFrameVisioPlaceholder] ref" $ R.readRef ref

      pure $ H.text $ WURL.host url

type LoadProps   = ( nodeId  :: Int
                   , session :: Session )

type ReloadProps = ( nodeId  :: Int
                   , reload :: T2.Reload
                   , session :: Session )

loadframe' :: Record LoadProps -> Aff (NodePoly Hyperdata)
loadframe' { nodeId, session } = get session $ NodeAPI Node (Just nodeId) ""

-- Just to make reloading effective
loadframeWithReload :: Record ReloadProps -> Aff (NodePoly Hyperdata)
loadframeWithReload { nodeId, session } = loadframe' { nodeId, session }
