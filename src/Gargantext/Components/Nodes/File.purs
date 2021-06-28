module Gargantext.Components.Nodes.File where

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON

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
derive instance Generic HyperdataFile _
derive instance Newtype HyperdataFile _
derive newtype instance JSON.ReadForeign HyperdataFile
instance Eq HyperdataFile where
  eq = genericEq

newtype File =
  File
  { id        :: Int
  , date      :: String
  , hyperdata :: HyperdataFile
  , name      :: String
  }
derive instance Generic File _
derive instance Newtype File _
derive newtype instance JSON.ReadForeign File
instance Eq File where
  eq = genericEq

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
