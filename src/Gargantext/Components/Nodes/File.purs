module Gargantext.Components.Nodes.File where

import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H

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
derive instance genericHyperdataFile :: Generic HyperdataFile _
instance eqHyperdataFile :: Eq HyperdataFile where
  eq = genericEq
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
derive instance genericFile :: Generic File _
instance eqFile :: Eq File where
  eq = genericEq
instance decodeFile :: DecodeJson File where
  decodeJson json = do
    obj       <- decodeJson json
    id        <- obj .: "id"
    date      <- obj .: "date"
    name      <- obj .: "name"
    hyperdata <- (obj .: "hyperdata") >>= decodeJson
    pure $ File { id, date, hyperdata, name }

type FileLayoutProps = ( nodeId :: NodeID, session :: Session )

fileLayout :: R2.Leaf FileLayoutProps
fileLayout props = R.createElement fileLayoutCpt props []

fileLayoutCpt :: R.Component FileLayoutProps
fileLayoutCpt = here.component "fileLayout" cpt where
  cpt { nodeId, session } _ = do
    useLoader nodeId (loadFile session) onLoad
      where
        onLoad loaded = fileLayoutLoaded { loaded, nodeId, session }
        key = show (sessionId session) <> "-" <> show nodeId

loadFile :: Session -> NodeID -> Aff File
loadFile session nodeId = get session $ NodeAPI Node (Just nodeId) ""

type FileLayoutLoadedProps =
  ( loaded  :: File
  | FileLayoutProps
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
