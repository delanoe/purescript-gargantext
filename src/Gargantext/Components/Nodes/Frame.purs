module Gargantext.Components.Nodes.Frame where

import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H
import Data.Argonaut as Argonaut
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
--import Gargantext.Utils.Argonaut (genericSumDecodeJson, genericSumEncodeJson, genericEnumDecodeJson, genericEnumEncodeJson)
import Data.Argonaut (decodeJson, (.:))

import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude
import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, get, sessionId)
import Gargantext.Types (NodeType(..))
import Gargantext.Utils.Argonaut (genericSumEncodeJson)
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.Nodes.Frame"

data Hyperdata =
  Hyperdata { base     :: String
            , frame_id :: String
            }

derive instance eqHyperdata :: Eq Hyperdata

derive instance genericHyperdata :: Generic Hyperdata _

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
  , session  :: Session
  , nodeType :: NodeType
  )

type Reload = R.State Int

type KeyProps =
  ( key :: String
  | Props
  )

frameLayout :: Record Props -> R.Element
frameLayout props = R.createElement frameLayoutCpt props []

frameLayoutCpt :: R.Component Props
frameLayoutCpt = R.hooksComponentWithModule thisModule "frameLayout" cpt
  where
    cpt {nodeId, session, nodeType} _ = do
      let sid = sessionId session

      pure $ frameLayoutWithKey { key: show sid <> "-" <> show nodeId, nodeId, session, nodeType}

frameLayoutWithKey :: Record KeyProps -> R.Element
frameLayoutWithKey props = R.createElement frameLayoutWithKeyCpt props []

frameLayoutWithKeyCpt :: R.Component KeyProps
frameLayoutWithKeyCpt = R.hooksComponentWithModule thisModule "frameLayoutWithKey" cpt
  where
    cpt { nodeId, session, nodeType} _ = do
      reload <- R.useState' 0

      useLoader {nodeId, reload: fst reload, session} loadframeWithReload $
        \frame -> frameLayoutView {frame, nodeId, reload, session, nodeType}

type ViewProps =
  ( frame  :: NodePoly Hyperdata
  , reload  :: Reload
  | Props
  )


type Base = String
type FrameId = String

hframeUrl :: NodeType -> Base -> FrameId -> String
hframeUrl NodeFrameNotebook _    frame_id = frame_id  -- Temp fix : frame_id is currently the whole url created
hframeUrl _             base frame_id = base <> "/" <> frame_id <> "?both"

frameLayoutView :: Record ViewProps -> R.Element
frameLayoutView props = R.createElement frameLayoutViewCpt props []

frameLayoutViewCpt :: R.Component ViewProps
frameLayoutViewCpt = R.hooksComponentWithModule thisModule "frameLayoutView" cpt
  where
    cpt {frame: (NodePoly {hyperdata: Hyperdata {base, frame_id}}), nodeId, reload, session, nodeType} _ = do
      pure $ H.div { className : "frame" }
                   [ H.iframe { src: hframeUrl nodeType base frame_id
                              , width: "100%"
                              , height: "100%"
                              } []
                   ]


type LoadProps = 
  ( nodeId  :: Int
  , session :: Session
  )

loadframe' :: Record LoadProps -> Aff (NodePoly Hyperdata)
loadframe' {nodeId, session} = get session $ NodeAPI Node (Just nodeId) ""

-- Just to make reloading effective
loadframeWithReload :: {reload :: Int  | LoadProps} -> Aff (NodePoly Hyperdata)
loadframeWithReload {nodeId, session} = loadframe' {nodeId, session}

