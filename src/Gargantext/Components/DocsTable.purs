-- TODO: this module should be replaced by FacetsTable
module Gargantext.Components.DocsTable where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>), encodeJson)
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((^.))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Ord.Down (Down(..))
import Data.Sequence as Seq
import Data.Set (Set)
import Data.Set as Set
import Data.String as Str
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Event as DE
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H
------------------------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Components.Category
import Gargantext.Components.Table as T
import Gargantext.Ends (Frontends, url)
import Gargantext.Hooks.Loader (useLoaderWithCacheAPI, HashedResponse(..))
import Gargantext.Utils.Seq (sortWith) as Seq
import Gargantext.Utils.Reactix as R2
import Gargantext.Routes as Routes
import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, sessionId, get, delete, put)
import Gargantext.Types (NodeType(..), OrderBy(..), TableResult, TabType, showTabType')
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.DocsTable"
------------------------------------------------------------------------

type TotalRecords = Int

type LayoutProps =
  ( nodeId       :: Int
  , totalRecords :: Int
  , chart        :: R.Element
  , tabType      :: TabType
  , listId       :: Int
  , corpusId     :: Maybe Int
  , showSearch   :: Boolean
  , frontends    :: Frontends
  , session      :: Session )
  -- ^ tabType is not ideal here since it is too much entangled with tabs and
  -- ngramtable. Let's see how this evolves.  )

type PageLayoutProps =
  ( nodeId       :: Int
  , totalRecords :: Int
  , tabType      :: TabType
  , listId       :: Int
  , corpusId     :: Maybe Int
  , query        :: Query
  , session      :: Session
  , frontends    :: Frontends
  , params       :: T.Params )

type LocalCategories = Map Int Category
type Query = String

_documentIdsDeleted  = prop (SProxy :: SProxy "documentIdsDeleted")
_localCategories     = prop (SProxy :: SProxy "localCategories")

data Action
  = MarkCategory Int Category

newtype DocumentsView
  = DocumentsView
    { _id    :: Int
    , category :: Category
    , date   :: Int
    , ngramCount :: Int
    , source :: String
    , title  :: String
    , url    :: String
    }

{-
derive instance genericDocumentsView :: Generic DocumentsView _
instance showDocumentsView :: Show DocumentsView where
  show = genericShow
instance decodeJsonSearchType :: Argonaut.DecodeJson SearchType where
  decodeJson = genericSumDecodeJson
instance encodeJsonSearchType :: Argonaut.EncodeJson SearchType where
  encodeJson = genericSumEncodeJson
  -}

instance decodeDocumentsView :: DecodeJson DocumentsView where
  decodeJson json = do
    obj <- decodeJson json
    _id <- obj .: "id"
    category <- obj .: "category"
    date <- obj .: "date"
    ngramCount <- obj .: "ngramCount"
    source <- obj .: "source"
    title <- obj .: "title"
    url <- obj .: "url"
    pure $ DocumentsView { _id, category, date, ngramCount, source, title, url }
instance encodeDocumentsView :: EncodeJson DocumentsView where
  encodeJson (DocumentsView dv) = 
       "id" := dv._id
    ~> "category" := dv.category
    ~> "date" := dv.date
    ~> "ngramCount" := dv.ngramCount
    ~> "source" := dv.source
    ~> "title" := dv.title
    ~> "url" := dv.url
    ~> jsonEmptyObject


newtype Response = Response
  { cid        :: Int
  , hyperdata  :: Hyperdata
  , category   :: Category
  , ngramCount :: Int
  , title      :: String
  }


newtype Hyperdata = Hyperdata
  { title  :: String
  , source :: String
  , pub_year   :: Int
  }


instance decodeHyperdata :: DecodeJson Hyperdata where
  decodeJson json = do
    obj    <- decodeJson json
    title  <- obj .: "title"
    source <- obj .: "source"
    pub_year <- obj .: "publication_year"
    pure $ Hyperdata { title,source, pub_year}

instance decodeResponse :: DecodeJson Response where
  decodeJson json = do
    obj        <- decodeJson json
    cid        <- obj .: "id"
    category   <- obj .: "category"
    ngramCount <- obj .: "id"
    title  <- obj .: "title"
    hyperdata  <- obj .: "hyperdata"
    pure $ Response { cid, title, category: decodeCategory category, ngramCount, hyperdata }


docViewLayout :: Record LayoutProps -> R.Element
docViewLayout props = R.createElement docViewLayoutCpt props []

docViewLayoutCpt :: R.Component LayoutProps
docViewLayoutCpt = R2.hooksComponent thisModule "docViewLayout" cpt
  where
    cpt layout _children = do
      query <- R.useState' ""
      let params = T.initialParams
      pure $ docView {query, params, layout}

type Props = (
    layout :: Record LayoutProps
  , params :: T.Params
  , query :: R.State Query
  )

docView :: Record Props -> R.Element
docView props = R.createElement docViewCpt props []

docViewCpt :: R.Component Props
docViewCpt = R2.hooksComponent thisModule "docView" cpt where
  cpt { query, params
      , layout: { frontends, session, nodeId, tabType, listId
                , corpusId, totalRecords, chart, showSearch } } _ = do
    pure $ H.div {className: "container1"}
      [ R2.row
        [ chart
        , if showSearch then searchBar query else H.div {} []
        , H.div {className: "col-md-12"}
          [ pageLayout { corpusId
                       , frontends
                       , listId
                       , nodeId
                       , params
                       , query: fst query
                       , session
                       , tabType
                       , totalRecords
                       } ] ] ]
    -- onClickTrashAll nodeId _ = do
    --   launchAff $ deleteAllDocuments p.session nodeId
          
          {-, H.div {className: "col-md-1 col-md-offset-11"}
            [ pageLayout p.session params {nodeId, totalRecords, tabType, listId, corpusId, query: fst query} ]
          , H.div {className: "col-md-1 col-md-offset-11"}
            [ H.button { className: "btn"
                       , style: {backgroundColor: "peru", color : "white", border : "white"}
                       , on: { click: onClickTrashAll nodeId } }
              [  H.i {className: "glyphitem glyphicon glyphicon-trash"} []
              ,  H.text "Trash all"
              ]
            ]
           -}

searchBar :: R.State Query -> R.Element
searchBar (query /\ setQuery) = R.createElement el {} []
  where
    el = R2.hooksComponent thisModule "SearchBar" cpt
    cpt {} _children = do
      queryText <- R.useState' query

      pure $ H.div {className: "row"}
        [ H.div {className: "col col-md-3"} []
        , H.div {className: "col col-md-1"} [if query /= "" then clearButton else H.div {} []]
        , H.div {className: "col col-md-3 form-group"}
          [ H.input { type: "text"
                    , className: "form-control"
                    , on: {change: onSearchChange queryText, keyUp: onSearchKeyup queryText}
                    , placeholder: query
                    , defaultValue: query}
          ]
        , H.div {className: "col col-md-1"} [searchButton queryText]
        ]

    onSearchChange :: forall e. R.State Query -> e -> Effect Unit
    onSearchChange (_ /\ setQueryText) = \e ->
      setQueryText $ const $ R2.unsafeEventValue e

    onSearchKeyup :: R.State Query -> DE.KeyboardEvent -> Effect Unit
    onSearchKeyup (queryText /\ _) = \e ->
      if DE.key e == "Enter" then
        setQuery $ const queryText
      else
        pure $ unit

    searchButton (queryText /\ _) =
      H.button { type: "submit"
               , className: "btn btn-default"
               , on: {click: \e -> setQuery $ const queryText}}
      [ H.span {className: "glyphicon glyphicon-search"} [] ]

    clearButton =
      H.button { className: "btn btn-danger"
               , on: {click: \e -> setQuery $ const ""}}
      [ H.span {className: "glyphicon glyphicon-remove"} [] ]

mock :: Boolean
mock = false

type PageParams =
  { nodeId :: Int
  , listId :: Int
  , corpusId :: Maybe Int
  , tabType :: TabType
  , query   :: Query
  , params :: T.Params}

getPageHash :: Session -> PageParams -> Aff String
getPageHash session { corpusId, listId, nodeId, query, tabType } = do
  (get session $ tableHashRoute nodeId tabType) :: Aff String


convOrderBy (Just (T.ASC  (T.ColumnName "Date")))  = Just DateAsc
convOrderBy (Just (T.DESC (T.ColumnName "Date")))  = Just DateDesc
convOrderBy (Just (T.ASC  (T.ColumnName "Title"))) = Just TitleAsc
convOrderBy (Just (T.DESC (T.ColumnName "Title"))) = Just TitleDesc
convOrderBy (Just (T.ASC  (T.ColumnName "Source"))) = Just SourceAsc
convOrderBy (Just (T.DESC (T.ColumnName "Source"))) = Just SourceDesc
convOrderBy _ = Nothing

res2corpus :: Response -> DocumentsView
res2corpus (Response r) =
  DocumentsView { _id : r.cid
  , url    : ""
  , date   : (\(Hyperdata hr) -> hr.pub_year) r.hyperdata
  , title  : (\(Hyperdata hr) -> hr.title) r.hyperdata
  , source : (\(Hyperdata hr) -> hr.source) r.hyperdata
  , category : r.category
  , ngramCount : r.ngramCount
}

filterDocs :: Query -> Array Response -> Array Response
filterDocs query docs = A.filter filterFunc docs
  where
    filterFunc :: Response -> Boolean
    filterFunc (Response { hyperdata: Hyperdata { title } }) =
      isJust $ Str.indexOf (Str.Pattern $ Str.toLower query) $ Str.toLower title

pageLayout :: Record PageLayoutProps -> R.Element
pageLayout props = R.createElement pageLayoutCpt props []

pageLayoutCpt :: R.Component PageLayoutProps
pageLayoutCpt = R2.hooksComponent thisModule "pageLayout" cpt where
  cpt props@{ corpusId, frontends, listId, nodeId, params, query, session, tabType } _ =
    useLoaderWithCacheAPI {
        cacheEndpoint: getPageHash session
      , handleResponse
      , mkRequest
      , path
      , renderer: paint
      }
    where
      path = { corpusId, listId, nodeId, params, query, tabType }
      paint (Tuple count docs) = page params (props { totalRecords = count }) docs

      mkRequest :: PageParams -> GUC.Request
      mkRequest p@{ listId, nodeId, tabType } =
        GUC.makeGetRequest session $ tableRoute nodeId tabType listId
      handleResponse :: HashedResponse (TableResult Response) -> Tuple Int (Array DocumentsView)
      handleResponse (HashedResponse { hash, value: res }) = ret
        where
          docs = res2corpus <$> filterDocs query res.docs
          ret = if mock then
                    --Tuple 0 (take limit $ drop offset sampleData)
                    Tuple 0 sampleData
                  else
                    Tuple (A.length docs) docs

type PageProps = (
    documents :: Array DocumentsView
  , layout :: Record PageLayoutProps
  , params :: T.Params
  )

page :: T.Params -> Record PageLayoutProps -> Array DocumentsView -> R.Element
page params layout documents = R.createElement pageCpt { documents, layout, params } []

pageCpt :: R.Component PageProps
pageCpt = R2.hooksComponent thisModule "pageCpt" cpt where
  cpt { documents, layout, params } _ = do
    paramsS <- R.useState' params
    pure $ pagePaint { documents, layout, params: paramsS }

type PagePaintProps = (
    documents :: Array DocumentsView
  , layout :: Record PageLayoutProps
  , params :: R.State T.Params
)

pagePaint :: Record PagePaintProps -> R.Element
pagePaint props = R.createElement pagePaintCpt props []

pagePaintCpt :: R.Component PagePaintProps
pagePaintCpt = R2.hooksComponent thisModule "pagePaintCpt" cpt where
  cpt { layout: { corpusId, frontends, listId, nodeId, session, totalRecords }, documents, params } _ = do
    localCategories <- R.useState' (mempty :: LocalCategories)
    pure $ T.table
      { colNames
      , container: T.defaultContainer { title: "Documents" }
      , params
      , rows: Seq.fromFoldable $ rows localCategories
      , totalRecords
      , wrapColElts
      }
      where
        sid = sessionId session
        gi Favorite  = "glyphicon glyphicon-star"
        gi _ = "glyphicon glyphicon-star-empty"
        trashStyle Trash = {textDecoration: "line-through"}
        trashStyle _ = {textDecoration: "none"}
        corpusDocument
          | Just cid <- corpusId = Routes.CorpusDocument sid cid listId
          | otherwise = Routes.Document sid listId
        colNames = T.ColumnName <$> [ "Tag", "Date", "Title", "Source"]
        wrapColElts = const identity
        getCategory (localCategories /\ _) {_id, category} = fromMaybe category (localCategories ^. at _id)
        orderWith =
          case convOrderBy (fst params).orderBy of
            Just DateAsc    -> Seq.sortWith \(DocumentsView { date })   -> date
            Just DateDesc   -> Seq.sortWith \(DocumentsView { date })   -> Down date
            Just SourceAsc  -> Seq.sortWith \(DocumentsView { source }) -> Str.toLower source
            Just SourceDesc -> Seq.sortWith \(DocumentsView { source }) -> Down $ Str.toLower source
            Just TitleAsc   -> Seq.sortWith \(DocumentsView { title })  -> Str.toLower title
            Just TitleDesc  -> Seq.sortWith \(DocumentsView { title })  -> Down $ Str.toLower title
            _               -> identity -- the server ordering is enough here
        filteredRows = T.filterRows { params: fst params } $ orderWith $ A.toUnfoldable documents
        rows localCategories = row <$> filteredRows
          where
            row (DocumentsView r) =
              { row:
                T.makeRow [ -- H.div {} [ H.a { className, style, on: {click: click Favorite} } [] ]
                 caroussel session nodeId setLocalCategories r cat
                --, H.input { type: "checkbox", defaultValue: checked, on: {click: click Trash} }
                -- TODO show date: Year-Month-Day only
                , H.div { style } [ R2.showText r.date ]
                , H.div { style }
                  [ H.a { href: url frontends $ corpusDocument r._id, target: "_blank"} [ H.text r.title ] ]
                , H.div { style } [ H.text $ if r.source == "" then "Source" else r.source ]
                ]
              , delete: true }
              where
                cat         = getCategory localCategories r
                (_ /\ setLocalCategories) = localCategories
                checked    = Trash == cat
                style      = trashStyle cat
                className  = gi cat

---------------------------------------------------------
sampleData' :: DocumentsView
sampleData' = DocumentsView { _id : 1
                            , url : ""
                            , date : 2010
                            , title : "title"
                            , source : "source"
                            , category : UnRead
                            , ngramCount : 1}

sampleData :: Array DocumentsView
--sampleData = replicate 10 sampleData'
sampleData = map (\(Tuple t s) -> DocumentsView { _id : 1
                                                , url : ""
                                                , date : 2017
                                                , title: t
                                                , source: s
                                                , category : UnRead
                                                , ngramCount : 10}) sampleDocuments

sampleDocuments :: Array (Tuple String String)
sampleDocuments = [Tuple "Macroscopic dynamics of the fusion process" "Journal de Physique Lettres",Tuple "Effects of static and cyclic fatigue at high temperature upon reaction bonded silicon nitride" "Journal de Physique Colloques",Tuple "Reliability of metal/glass-ceramic junctions made by solid state bonding" "Journal de Physique Colloques",Tuple "High temperature mechanical properties and intergranular structure of sialons" "Journal de Physique Colloques",Tuple "SOLUTIONS OF THE LANDAU-VLASOV EQUATION IN NUCLEAR PHYSICS" "Journal de Physique Colloques",Tuple "A STUDY ON THE FUSION REACTION 139La + 12C AT 50 MeV/u WITH THE VUU EQUATION" "Journal de Physique Colloques",Tuple "Atomic structure of \"vitreous\" interfacial films in sialon" "Journal de Physique Colloques",Tuple "MICROSTRUCTURAL AND ANALYTICAL CHARACTERIZATION OF Al2O3/Al-Mg COMPOSITE INTERFACES" "Journal de Physique Colloques",Tuple "Development of oxidation resistant high temperature NbTiAl alloys and intermetallics" "Journal de Physique IV Colloque",Tuple "Determination of brazed joint constitutive law by inverse method" "Journal de Physique IV Colloque",Tuple "Two dimensional estimates from ocean SAR images" "Nonlinear Processes in Geophysics",Tuple "Comparison Between New Carbon Nanostructures Produced by Plasma with Industrial Carbon Black Grades" "Journal de Physique III",Tuple "<i>Letter to the Editor:</i> SCIPION, a new flexible ionospheric sounder in Senegal" "Annales Geophysicae",Tuple "Is reducibility in nuclear multifragmentation related to thermal scaling?" "Physics Letters B",Tuple "Independence of fragment charge distributions of the size of heavy multifragmenting sources" "Physics Letters B",Tuple "Hard photons and neutral pions as probes of hot and dense nuclear matter" "Nuclear Physics A",Tuple "Surveying the nuclear caloric curve" "Physics Letters B",Tuple "A hot expanding source in 50 A MeV Xe+Sn central reactions" "Physics Letters B"]

newtype SearchQuery = SearchQuery
  { query :: Array String
  , parent_id :: Int
  }


instance encodeJsonSQuery :: EncodeJson SearchQuery where
  encodeJson (SearchQuery {query, parent_id})
     = "query" := query
    ~> "parent_id" := parent_id
    ~> jsonEmptyObject


documentsRoute :: Int -> SessionRoute
documentsRoute nodeId = NodeAPI Node (Just nodeId) "documents"

tableRoute :: Int -> TabType -> Int -> SessionRoute
tableRoute nodeId tabType listId = NodeAPI Node (Just nodeId) $ "table" <> "?tabType=" <> (showTabType' tabType) <> "&list=" <> (show listId)

tableHashRoute :: Int -> TabType -> SessionRoute
tableHashRoute nodeId tabType = NodeAPI Node (Just nodeId) $ "table/hash" <> "?tabType=" <> (showTabType' tabType)

deleteAllDocuments :: Session -> Int -> Aff (Array Int)
deleteAllDocuments session = delete session <<< documentsRoute

-- TODO: not optimal but Data.Set lacks some function (Set.alter)
toggleSet :: forall a. Ord a => a -> Set a -> Set a
toggleSet a s
  | Set.member a s = Set.delete a s
  | otherwise      = Set.insert a s
