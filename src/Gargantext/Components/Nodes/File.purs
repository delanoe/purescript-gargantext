module Gargantext.Components.Nodes.File where

import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Maybe (Maybe(..))
import DOM.Simple.Console (log2)
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude
import Gargantext.Ends (toUrl)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get)
import Gargantext.Types as T
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.Nodes.File"


newtype HyperdataFile = HyperdataFile {
    mime :: String
  , name :: String
  , path :: String
  }

instance decodeHyperdataFile :: DecodeJson HyperdataFile where
  decodeJson json = do
    obj <- decodeJson json
    mime <- obj .: "mime"
    name <- obj .: "name"
    path <- obj .: "path"
    pure $ HyperdataFile {
        mime
      , name
      , path
      }


newtype File = File {
    id :: Int
  , date :: String
  , hyperdata :: HyperdataFile
  , name :: String
  }

instance decodeFile :: DecodeJson File where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    date <- obj .: "date"
    hyperdata' <- obj .: "hyperdata"
    hyperdata <- decodeJson hyperdata'
    name <- obj .: "name"

    pure $ File {
        id
      , date
      , hyperdata
      , name
      }


type FileLayoutProps = (
    nodeId :: Int
  , session :: Session
)

fileLayout :: Record FileLayoutProps -> R.Element
fileLayout props = R.createElement fileLayoutCpt props []

fileLayoutCpt :: R.Component FileLayoutProps
fileLayoutCpt = R.hooksComponentWithModule thisModule "fileLayout" cpt
  where
    cpt { nodeId, session } _ = do
      useLoader { nodeId } (loadFile session) $ \loaded ->
        fileLayoutLoaded { loaded, nodeId, session }

type LoadFileProps = (
  nodeId :: Int
  )

loadFile :: Session -> Record LoadFileProps -> Aff File
loadFile session { nodeId } = get session $ NodeAPI T.Node (Just nodeId) ""

type FileLayoutLoadedProps = (
  loaded :: File
  | FileLayoutProps
  )

fileLayoutLoaded :: Record FileLayoutLoadedProps -> R.Element
fileLayoutLoaded props = R.createElement fileLayoutLoadedCpt props []

fileLayoutLoadedCpt :: R.Component FileLayoutLoadedProps
fileLayoutLoadedCpt = R.hooksComponentWithModule thisModule "fileLayoutLoaded" cpt
  where
    cpt { loaded: File { hyperdata: HyperdataFile hyperdata }, nodeId, session } _ = do
      R.useEffect' $ do
        log2 "[fileLayoutLoaded] hyperdata" hyperdata

      pure $ H.div { className: "col-md-12" } [
          H.div { className: "row" } [
            H.h2 {} [ H.text hyperdata.name ]
          ]
        , H.div { className: "row" } [
            H.div { className: "btn btn-primary" } [
               H.a { href: toUrl session ("node/" <> show nodeId <> "/file/download")
                   , target: "_blank"
                   } [ H.text "Download" ]
               ]
          ]
      ]
