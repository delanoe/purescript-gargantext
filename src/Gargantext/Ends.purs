-- | Those things at the end of urls
module Gargantext.Ends
  -- ( )
  where

import Data.Array (filter)
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Prelude (class Eq, class Show,  show, ($), (/=), (<<<), (<>), (==), (<$>))
import Simple.JSON as JSON

import Gargantext.Routes as R
import Gargantext.Types (ApiVersion, ChartType(..), Limit, NodePath, NodeType(..), Offset, TabType(..), TermSize(..), nodePath, nodeTypePath, showTabType', TermList(MapTerm))
import Gargantext.Utils.QueryString (joinQueryStrings, mQueryParam, queryParam, queryParamS)

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
  , version :: ApiVersion
  , backendType :: String
  }
derive instance Generic Backend _
derive instance Newtype Backend _
derive newtype instance JSON.ReadForeign Backend
derive newtype instance JSON.WriteForeign Backend
instance Eq Backend where eq = genericEq
instance Show Backend where show (Backend {name}) = name
instance ToUrl Backend String where toUrl = backendUrl

type BaseUrl = String
type PrePath = String
type Name    = String
type BackendType = String

backend :: BackendType -> Name -> ApiVersion -> PrePath -> BaseUrl -> Backend
backend backendType name version prePath baseUrl = Backend { name, version, prePath, baseUrl, backendType}

-- | Creates a backend url from a backend and the path as a string
backendUrl :: Backend -> String -> String
backendUrl (Backend b) path = b.baseUrl <> b.prePath <> show b.version <> "/" <> path

-- | Encapsulates the data needed to construct a url to a frontend
-- | server (either for the app or static content)
newtype Frontend = Frontend
  { name    :: String
  , baseUrl :: String
  , prePath :: String
  }

derive instance Generic Frontend _
instance Eq Frontend where eq = genericEq
instance ToUrl Frontend NodePath where toUrl front np = frontendUrl front (nodePath np)
instance Show Frontend where show (Frontend {name}) = name
instance ToUrl Frontend String where toUrl = frontendUrl
instance ToUrl Frontend R.AppRoute where toUrl f r = frontendUrl f (R.appPath r)

-- | Creates a frontend
frontend :: String -> String -> String -> Frontend
frontend baseUrl prePath name = Frontend { name, baseUrl, prePath }

-- | Creates a url from a frontend and the path as a string
frontendUrl :: Frontend -> String -> String
frontendUrl (Frontend f) path = f.baseUrl <> f.prePath <> path

-- | The currently selected App and Static configurations
newtype Frontends = Frontends { app :: Frontend, static :: Frontend }

derive instance Eq Frontends
instance ToUrl Frontends R.AppRoute where toUrl f r = appUrl f (R.appPath r)
instance ToUrl Frontends NodePath where toUrl (Frontends {app}) np = frontendUrl app (nodePath np)

-- | Creates an app url from a Frontends and the path as a string
appUrl :: Frontends -> String -> String
appUrl (Frontends {app}) = frontendUrl app

-- | Creates a static url from a Frontends and the path as a string
staticUrl :: Frontends -> String -> String
staticUrl (Frontends {static}) = frontendUrl static

sessionPath :: R.SessionRoute -> String
sessionPath (R.Tab t i)             = sessionPath (R.NodeAPI Node i (showTabType' t))
sessionPath (R.Children n o l s i)  = sessionPath (R.NodeAPI Node i ("children" <> qs))
  where
    qs = joinQueryStrings [ queryParam "type" n
                          , queryParam "offset" o
                          , queryParam "limit" l
                          , queryParam "order" s ]

sessionPath (R.RecomputeNgrams nt nId lId)      = "node/" <> (show nId) <> "/ngrams/recompute" <> qs
  where
    qs = joinQueryStrings [ queryParam "list" lId
                          , queryParam "ngramsType" nt ]
sessionPath (R.RecomputeListChart ChartBar nt nId lId)   = "node/" <> (show nId) <> "/pie" <> qs
  where
    qs = joinQueryStrings [ queryParam "list" lId
                          , queryParam "ngramsType" nt ]
sessionPath (R.RecomputeListChart ChartPie nt nId lId)   = "node/" <> (show nId) <> "/pie" <> qs
  where
    qs = joinQueryStrings [ queryParam "list" lId
                          , queryParam "ngramsType" nt ]
sessionPath (R.RecomputeListChart ChartTree nt nId lId)  = "node/" <> (show nId) <> "/tree" <> qs
  where
    qs = joinQueryStrings [ queryParam "list" lId
                          , queryParam "ngramsType" nt
                          , queryParam "listType" MapTerm ]
sessionPath (R.RecomputeListChart Histo nt nId lId)      = "node/" <> (show nId) <> "/chart" <> qs
  where
    qs = joinQueryStrings [ queryParam "list" lId
                          , queryParam "ngramsType" nt ]
sessionPath (R.RecomputeListChart Scatter nt nId lId)    = "node/" <> (show nId) <> "/metrics" <> qs
  where
    qs = joinQueryStrings [ queryParam "list" lId
                          , queryParam "ngramsType" nt ]
sessionPath (R.RecomputeListChart _ nt nId lId)          = "node/" <> (show nId) <> "/recompute-chart" <> qs
  where
    qs = joinQueryStrings [ queryParam "list" lId
                          , queryParam "ngramsType" nt ]
sessionPath (R.GraphAPI gId p)      = "graph/" <> (show gId) <> "/" <> p
sessionPath (R.GetNgrams opts i)    =
  base opts.tabType $ "ngrams" <> qs
  where
    base (TabCorpus _) = sessionPath <<< R.NodeAPI Node i
    base _             = sessionPath <<< R.NodeAPI Url_Document i

    qs = joinQueryStrings ( [ queryParamS "ngramsType" $ showTabType' opts.tabType
                            , queryParam "limit" opts.limit
                            , mQueryParam "orderBy" opts.orderBy
                            , mQueryParam "offset" opts.offset
                            , mQueryParam "listType" opts.termListFilter ]
                            <> listIds
                            <> termSizeFilter opts.termSizeFilter
                            <> search opts.searchQuery )

    listIds = (queryParam "list") <$> filter (_ /= 0) opts.listIds
    termSizeFilter Nothing = []
    termSizeFilter (Just MonoTerm) = [ queryParam "minTermSize" 0, queryParam "maxTermSize" 1 ]
    termSizeFilter (Just MultiTerm) = [ queryParam "minTermSize" 2 ]
    search "" = []
    search s = [ queryParamS "search" s ]
sessionPath (R.GetNgramsTableAll opts i) =
  sessionPath $ R.NodeAPI Node i $ "ngrams" <> qs
  where
    qs = joinQueryStrings ([ queryParamS "ngramsType" $ showTabType' opts.tabType
                           , queryParam "limit" 100000 ] <> list)
    list = (queryParam "list") <$> opts.listIds
sessionPath (R.GetNgramsTableVersion opts i) =
  sessionPath $ R.NodeAPI Node i $ "ngrams/version" <> qs
    --  $ "ngrams/version?"
    -- <> "list=" <> show opts.listId
  where
    qs = joinQueryStrings [ queryParamS "ngramsType" $ showTabType' opts.tabType
                          , queryParam "list" opts.listId ]
sessionPath (R.ListDocument lId dId) =
  sessionPath $ R.NodeAPI NodeList lId ("document/" <> (show $ fromMaybe 0 dId))
sessionPath (R.ListsRoute lId) = "lists/" <> show lId
sessionPath (R.PutNgrams t listId termList i) =
  sessionPath $ R.NodeAPI Node i $ "ngrams" <> qs
  where
    qs = joinQueryStrings [ queryParamS "ngramsType" $ showTabType' t
                          , mQueryParam "list" listId
                          , mQueryParam "listType" termList ]
sessionPath (R.PostNgramsChartsAsync i) =
  sessionPath $ R.NodeAPI Node i $ "ngrams/async/charts/update"
sessionPath (R.NodeAPI nt i p) = nodeTypePath nt
                              <> (maybe "" (\i' -> "/" <> show i') i)
                              <> (if p == "" then "" else "/" <> p)
sessionPath (R.TreeFirstLevel nId p) = nodeTypePath Tree
                                    <> (maybe "" (\nId' -> "/" <> show nId') nId) <> "/first-level" <> p
sessionPath (R.Search {listId, limit, offset, orderBy} mCorpusId) =
  sessionPath $ R.NodeAPI Corpus mCorpusId $ "search" <> qs
  where
    qs = joinQueryStrings [ queryParam "list_id" listId
                          , queryParam "offset" offset
                          , queryParam "limit" limit
                          , mQueryParam "orderBy" orderBy ]
-- sessionPath (R.Search {listId, limit, offset, orderBy} (Just corpusId)) =
--     "search/" <> (show corpusId) <> "/list/" <> (show listId) <> "?"
--     <> offsetUrl offset
--     <> limitUrl limit
--     <> orderUrl orderBy
sessionPath (R.CorpusMetrics { listId, limit, tabType} i) =
  sessionPath $ R.NodeAPI Corpus i $ "metrics" <> qs
  where
    qs = joinQueryStrings [ queryParam "ngrams" listId
                          , queryParamS "ngramsType" $ showTabType' tabType
                          , mQueryParam "limit" limit ]
sessionPath (R.CorpusMetricsHash { listId, tabType} i) =
  sessionPath $ R.NodeAPI Corpus i $ "metrics/hash" <> qs
  where
    qs = joinQueryStrings [ queryParam "ngrams" listId
                          , queryParamS "ngramsType" $ showTabType' tabType ]
-- TODO fix this url path
sessionPath (R.Chart {chartType, limit, listId, tabType} i) =
  sessionPath $ R.NodeAPI Corpus i $ show chartType <> qs
    where
      qs = joinQueryStrings [ queryParamS "ngramsType" $ showTabType' tabType
                            , queryParam "listType" MapTerm
                            , mQueryParam "list" listId ]
    -- <> maybe "" limitUrl limit
sessionPath (R.ChartHash { chartType, listId, tabType } i) =
  sessionPath $ R.NodeAPI Corpus i $ show chartType <> "/hash" <> qs
  where
    qs = joinQueryStrings [ queryParamS "ngramsType" $ showTabType' tabType
                          , queryParam "listType" MapTerm
                          , mQueryParam "list" listId ]
-- sessionPath (R.NodeAPI (NodeContact s a i) i) = sessionPath $ "annuaire/" <> show a <> "/contact/" <> show i
sessionPath (R.PhyloAPI nId) = "node/" <> show nId <> "/phylo"
sessionPath R.Members = "members"

------- misc routing stuff

defaultList :: Int -> String
defaultList n = if n == 0 then "" else ("list=" <> show n)

defaultListAdd :: Int -> String
defaultListAdd n = "&" <> defaultList n

defaultListAddMaybe :: Maybe Int -> String
defaultListAddMaybe Nothing = ""
defaultListAddMaybe (Just l) = "&list=" <> show l



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

-- instance ToUrl NodeType where
--   toUrl ec e nt i = toUrl ec e (NodeAPI nt) i

-- instance ToUrl Path where
--   toUrl ec e p i = doUrl base path params
--     where
--       base   = endBaseUrl e ec
--       path   = endPathUrl e ec p i
--       params = ""
------------------------------------------------------------
