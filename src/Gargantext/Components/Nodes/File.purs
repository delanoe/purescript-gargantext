module Gargantext.Components.Nodes.File where

import Gargantext.Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Gargantext.Config.REST (AffRESTError, logRESTError)
import Gargantext.Ends (toUrl)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get)
import Gargantext.Types (NodeType(..), NodeID)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON

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
fileLayout = R2.leaf fileLayoutCpt
fileLayoutCpt :: R.Component FileLayoutProps
fileLayoutCpt = here.component "fileLayout" cpt where
  cpt { nodeId, session } _ = do
    useLoader { errorHandler
              , loader: loadFile session
              , path: nodeId
              , render: onLoad }
      where
        errorHandler = logRESTError here "[fileLayout]"
        onLoad loaded = fileLayoutLoaded { loaded, nodeId, session }

loadFile :: Session -> NodeID -> AffRESTError File
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
