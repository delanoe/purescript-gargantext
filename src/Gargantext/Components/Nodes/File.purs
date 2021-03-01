module Gargantext.Components.Nodes.File where

import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Prelude
import Gargantext.Ends (toUrl)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get, sessionId)
import Gargantext.Types (NodeType(..), NodeID)
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.File"

newtype HyperdataFile =
  HyperdataFile
  { mime :: String
  , name :: String
  , path :: String
  }

instance decodeHyperdataFile :: DecodeJson HyperdataFile where
  decodeJson json = do
    obj  <- decodeJson json
    mime <- obj .: "mime"
    name <- obj .: "name"
    path <- obj .: "path"
    pure $ HyperdataFile { mime, name, path }

newtype File =
  File
  { id        :: Int
  , date      :: String
  , hyperdata :: HyperdataFile
  , name      :: String
  }

instance decodeFile :: DecodeJson File where
  decodeJson json = do
    obj       <- decodeJson json
    id        <- obj .: "id"
    date      <- obj .: "date"
    name      <- obj .: "name"
    hyperdata <- (obj .: "hyperdata") >>= decodeJson
    pure $ File { id, date, hyperdata, name }

type FileLayoutProps = ( nodeId :: NodeID, session :: R.Context Session )

fileLayout :: R2.Leaf FileLayoutProps
fileLayout props = R.createElement fileLayoutCpt props []

fileLayoutCpt :: R.Component FileLayoutProps
fileLayoutCpt = here.component "fileLayout" cpt where
  cpt { nodeId, session } _ = R.useContext session >>= cp where
    cp s = useLoader nodeId (loadFile s) onLoad where
      onLoad loaded = fileLayoutLoaded { loaded, nodeId, session: s } where
        key = show (sessionId s) <> "-" <> show nodeId

loadFile :: Session -> NodeID -> Aff File
loadFile session nodeId = get session $ NodeAPI Node (Just nodeId) ""

type FileLayoutLoadedProps =
  ( loaded  :: File
  , nodeId  :: Int
  , session :: Session
  )

fileLayoutLoaded :: Record FileLayoutLoadedProps -> R.Element
fileLayoutLoaded props = R.createElement fileLayoutLoadedCpt props []

fileLayoutLoadedCpt :: R.Component FileLayoutLoadedProps
fileLayoutLoadedCpt = here.component "fileLayoutLoaded" cpt where
  cpt { loaded: File { hyperdata: HyperdataFile hyperdata }, nodeId, session } _ = do
    R.useEffect' $ here.log hyperdata
    pure $
      H.div { className: "col-md-12" }
      [ H.div { className: "row" } [ H.h2 {} [ H.text hyperdata.name ] ]
      , H.div { className: "row" }
        [ H.div { className: "btn btn-primary" }
          [ H.a { href, target: "_blank" } [ H.text "Download" ]]]] where
      href = toUrl session ("node/" <> show nodeId <> "/file/download")
