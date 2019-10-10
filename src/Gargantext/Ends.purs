-- | Those things at the end of urls
module Gargantext.Ends
  -- ( )
  where

import Prelude (class Eq, class Show, identity, show, ($), (<>), bind, pure)
import Data.Argonaut ( class DecodeJson, decodeJson, class EncodeJson, encodeJson, (:=), (~>), jsonEmptyObject, (.:))
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Maybe (Maybe, maybe)
import Gargantext.Routes as R
import Gargantext.Types
   ( ApiVersion, Limit, NodePath, NodeType(..), Offset, TabType(..)
   , TermSize(..), nodePath, nodeTypePath, showTabType')

-- | A means of generating a url to visit, a destination
class ToUrl conf p where
  toUrl :: conf -> p -> String

url :: forall conf p. ToUrl conf p => conf -> p -> String
url = toUrl

-- | Encapsulates the data we need to talk to a backend server
newtype Backend = Backend
  { name    :: String
  , baseUrl :: String
  , prePath :: String
  , version :: ApiVersion }

backend :: ApiVersion -> String -> String -> String -> Backend
backend version prePath baseUrl name = Backend { name, version, prePath, baseUrl }

-- | Creates a backend url from a backend and the path as a string
backendUrl :: Backend -> String -> String
backendUrl (Backend b) path = b.baseUrl <> b.prePath <> show b.version <> "/" <> path

derive instance genericBackend :: Generic Backend _

instance eqBackend :: Eq Backend where
  eq = genericEq

instance showBackend :: Show Backend where
  show (Backend {name}) = name

instance toUrlBackendString :: ToUrl Backend String where
  toUrl = backendUrl

-- JSON instances
instance encodeJsonBackend :: EncodeJson Backend where
  encodeJson (Backend {name, baseUrl, prePath, version})
    =  "name"    := name
    ~> "baseUrl" := baseUrl
    ~> "prePath" := prePath
    ~> "version" := show version
    ~> jsonEmptyObject

instance decodeJsonBackend :: DecodeJson Backend where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .: "objet"
    baseUrl <- obj .: "baseUrl"
    prePath <- obj .: "prePath"
    version <- obj .: "version"
    pure $ Backend {name, baseUrl, prePath, version}

-- | Encapsulates the data needed to construct a url to a frontend
-- | server (either for the app or static content)
newtype Frontend = Frontend
  { name    :: String
  , baseUrl :: String
  , prePath :: String }

derive instance genericFrontend :: Generic Frontend _

instance eqFrontend :: Eq Frontend where
  eq = genericEq

instance toUrlFrontendNodePath :: ToUrl Frontend NodePath where
  toUrl front np = frontendUrl front (nodePath np)

-- | Creates a frontend
frontend :: String -> String -> String -> Frontend
frontend baseUrl prePath name = Frontend { name, baseUrl, prePath }

-- | Creates a url from a frontend and the path as a string
frontendUrl :: Frontend -> String -> String
frontendUrl (Frontend f) path = f.baseUrl <> f.prePath <> path

instance showFrontend :: Show Frontend where
  show (Frontend {name}) = name

instance toUrlFrontendString :: ToUrl Frontend String where
  toUrl = frontendUrl

instance toUrlFrontendAppRoute :: ToUrl Frontend R.AppRoute where
  toUrl f r = frontendUrl f (R.appPath r)

-- | The currently selected App and Static configurations
newtype Frontends = Frontends { app :: Frontend, static :: Frontend }

instance toUrlFrontendsRoutes :: ToUrl Frontends R.AppRoute where
  toUrl f r = appUrl f (R.appPath r)

instance toUrlFrontendsNodePath :: ToUrl Frontends NodePath where
  toUrl (Frontends {app}) np = frontendUrl app (nodePath np)

-- | Creates an app url from a Frontends and the path as a string
appUrl :: Frontends -> String -> String
appUrl (Frontends {app}) = frontendUrl app

-- | Creates a static url from a Frontends and the path as a string
staticUrl :: Frontends -> String -> String
staticUrl (Frontends {static}) = frontendUrl static

sessionPath :: R.SessionRoute -> String
sessionPath (R.Tab t i)            = sessionPath (R.NodeAPI Node i) <> "/" <> showTabType' t
sessionPath (R.Children n o l s i) = root <> "children?type=" <> show n <> offsetUrl o <> limitUrl l <> orderUrl s
  where root = sessionPath (R.NodeAPI Node i) <> "/"
sessionPath (R.NodeAPI Phylo pId)  = "phyloscape?nodeId=" <> (show $ maybe 0 identity pId)
sessionPath (R.GetNgrams opts i)   =
  base opts.tabType
  <> "/ngrams?ngramsType="
  <> showTabType' opts.tabType
  <> offsetUrl opts.offset
  <> limitUrl opts.limit
  <> orderByUrl opts.orderBy
  <> foldMap (\x -> "&list=" <> show x) opts.listIds
  <> foldMap (\x -> "&listType=" <> show x) opts.termListFilter
  <> foldMap termSizeFilter opts.termSizeFilter
  <> search opts.searchQuery
  where
    base (TabCorpus _) = sessionPath (R.NodeAPI Node i)
    base _ = sessionPath (R.NodeAPI Url_Document i)
    termSizeFilter MonoTerm = "&minTermSize=0&maxTermSize=1"
    termSizeFilter MultiTerm = "&minTermSize=2"
    search "" = ""
    search s = "&search=" <> s
sessionPath (R.ListDocument lId dId) =
  sessionPath (R.NodeAPI NodeList lId) <> "/document/" <> (show $ maybe 0 identity dId)
sessionPath (R.PutNgrams t listId termList i) =
    sessionPath (R.NodeAPI Node i)
    <> "/ngrams?ngramsType="
    <> showTabType' t
    <> maybe "" (\x -> "&list=" <> show x) listId
    <> foldMap (\x -> "&listType=" <> show x) termList
sessionPath (R.NodeAPI nt i) = nodeTypePath nt <> (maybe "" (\i' -> "/" <> show i') i)
sessionPath (R.Search {listId,limit,offset,orderBy} i) =
    sessionPath (R.NodeAPI Corpus i)
    <> "/search?list_id=" <> show listId
    <> offsetUrl offset
    <> limitUrl limit
    <> orderUrl orderBy
sessionPath (R.CorpusMetrics {tabType, listId, limit} i) =
    sessionPath (R.NodeAPI Corpus i) <> "/metrics"
      <> "?ngrams=" <> show listId
      <> "&ngramsType=" <> showTabType' tabType
      <> maybe "" (\x -> "&limit=" <> show x) limit
-- TODO fix this url path
sessionPath (R.Chart {chartType, tabType} i) =
    sessionPath (R.NodeAPI Corpus i) <> "/" <> show chartType
      <> "?ngramsType=" <> showTabType' tabType
      <> "&listType=GraphTerm" -- <> show listId
      -- <> maybe "" (\x -> "&limit=" <> show x) limit

------- misc routing stuff

limitUrl :: Limit -> String
limitUrl l = "&limit=" <> show l

offsetUrl :: Offset -> String
offsetUrl o = "&offset=" <> show o

orderUrl :: forall a. Show a => Maybe a -> String
orderUrl = maybe "" (\x -> "&order=" <> show x)

orderByUrl :: forall a. Show a => Maybe a -> String
orderByUrl = maybe "" (\x -> "&orderBy=" <> show x)

-- nodeTypePath :: NodeType -> Path
-- nodeTypePath = NodeAPI

-- instance toUrlNodeType :: ToUrl NodeType where
--   toUrl ec e nt i = toUrl ec e (NodeAPI nt) i

-- instance toUrlPath :: ToUrl Path where
--   toUrl ec e p i = doUrl base path params
--     where
--       base   = endBaseUrl e ec
--       path   = endPathUrl e ec p i
--       params = ""
------------------------------------------------------------
