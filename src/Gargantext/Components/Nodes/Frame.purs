module Gargantext.Components.Nodes.Frame where

import Gargantext.Prelude

import Data.Argonaut (decodeJson, (.:))
import Data.Argonaut as Argonaut
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Gargantext.Components.FolderView as FV
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, get, sessionId)
import Gargantext.Types (NodeType(..))
import Gargantext.Utils.Argonaut (genericSumEncodeJson)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Frame"

data Hyperdata = Hyperdata { base :: String, frame_id :: String }

derive instance genericHyperdata :: Generic Hyperdata _
instance eqHyperdata :: Eq Hyperdata where
  eq = genericEq
instance showHyperdata :: Show Hyperdata where
  show = genericShow
instance decodeJsonHyperdata :: Argonaut.DecodeJson Hyperdata where
-- TODO
--  decodeJson = genericSumDecodeJson
  decodeJson json = do
    obj      <- decodeJson json
    base     <- obj .: "base"
    frame_id <- obj .: "frame_id"
    pure $ Hyperdata {base, frame_id}
instance encodeJsonHyperdata :: Argonaut.EncodeJson Hyperdata where
  encodeJson = genericSumEncodeJson

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
hframeUrl NodeFrameCalc base frame_id = base <> "/" <> frame_id
hframeUrl _ base frame_id = base <> "/" <> frame_id <> "?view" -- "?both"

frameLayoutView :: Record ViewProps -> R.Element
frameLayoutView props = R.createElement frameLayoutViewCpt props []

frameLayoutViewCpt :: R.Component ViewProps
frameLayoutViewCpt = here.component "frameLayoutView" cpt
  where
    cpt { frame: (NodePoly { hyperdata: Hyperdata { base, frame_id }})
        , nodeId
        , nodeType
        , reload
        , session } _ =
      pure $ H.div{} [
        FV.backButton
      , FV.homeButton
      , H.div { className : "frame"
              , rows: "100%,*" }
          [ H.iframe { src: hframeUrl nodeType base frame_id
                     , width: "100%"
                     , height: "100%"
                     } []
          ]
      ]

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
