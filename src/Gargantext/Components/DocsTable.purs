-- TODO: this module should be replaced by FacetsTable
module Gargantext.Components.DocsTable where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Array as A
import Data.Lens ((^.))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Ord.Down (Down(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as Str
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log, log2)
import DOM.Simple.Event as DE
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H
------------------------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Components.Category (caroussel)
import Gargantext.Components.Category.Types (Category(..), decodeCategory)
import Gargantext.Components.DocsTable.Types
import Gargantext.Components.Nodes.Lists.Types as NT
import Gargantext.Components.Nodes.Texts.Types (SidePanelTriggers)
import Gargantext.Components.Table as T
import Gargantext.Ends (Frontends, url)
import Gargantext.Hooks.Loader (useLoader, useLoaderWithCacheAPI, HashedResponse(..))
import Gargantext.Routes as Routes
import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, sessionId, get, delete)
import Gargantext.Types (ListId, NodeID, NodeType(..), OrderBy(..), ReloadS, TableResult, TabSubType, TabType, showTabType')
import Gargantext.Utils (sortWith)
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.QueryString (joinQueryStrings, mQueryParamS, queryParam, queryParamS)
import Gargantext.Utils.Reactix as R2

thisModule :: String
thisModule = "Gargantext.Components.DocsTable"
------------------------------------------------------------------------

type TotalRecords = Int

type Path a = (
    corpusId :: Int
  , listId :: Int
  , frontends :: Frontends
  , session :: Session
  , tabType :: TabSubType a
  )

type LayoutProps = (
    cacheState      :: R.State NT.CacheState
  , frontends       :: Frontends
  , chart           :: R.Element
  , listId          :: Int
  , mCorpusId       :: Maybe Int
  , nodeId          :: Int
  -- , path         :: Record (Path a)
  , session         :: Session
  , showSearch      :: Boolean
  , sidePanelTriggers :: Record SidePanelTriggers
  , tabType         :: TabType
  -- ^ tabType is not ideal here since it is too much entangled with tabs and
  -- ngramtable. Let's see how this evolves.  )
  , totalRecords    :: Int
  )

type PageLayoutProps = (
    cacheState        :: R.State NT.CacheState
  , frontends         :: Frontends
  , key               :: String  -- NOTE Necessary to clear the component when cache state changes
  , listId            :: Int
  , mCorpusId         :: Maybe Int
  , nodeId            :: Int
  , params            :: T.Params
  , query             :: Query
  , session           :: Session
  , sidePanelTriggers :: Record SidePanelTriggers
  , tabType           :: TabType
  , totalRecords      :: Int
  )

_documentIdsDeleted  = prop (SProxy :: SProxy "documentIdsDeleted")
_localCategories     = prop (SProxy :: SProxy "localCategories")

docViewLayout :: Record LayoutProps -> R.Element
docViewLayout props = R.createElement docViewLayoutCpt props []

docViewLayoutCpt :: R.Component LayoutProps
docViewLayoutCpt = R.hooksComponentWithModule thisModule "docViewLayout" cpt
  where
    cpt layout _children = do
      query <- R.useState' ""
      let params = T.initialParams
      pure $ docView { layout, params, query } []

type Props = (
    layout :: Record LayoutProps
  , params :: T.Params
  , query :: R.State Query
  )

docView :: R2.Component Props
docView = R.createElement docViewCpt

docViewCpt :: R.Component Props
docViewCpt = R.hooksComponentWithModule thisModule "docView" cpt where
  cpt { layout: { cacheState
                , chart
                , frontends
                , listId
                , mCorpusId
                , nodeId
                , session
                , showSearch
                , sidePanelTriggers
                , tabType
                , totalRecords
                }
      , params
      , query
      } _ = do
    pure $ H.div {className: "container1"}
      [ R2.row
        [ chart
        , if showSearch then searchBar query else H.div {} []
        , H.div {className: "col-md-12"}
          [ pageLayout { cacheState
                       , frontends
                       , key: "docView-" <> (show $ fst cacheState)
                       , listId
                       , mCorpusId
                       , nodeId
                       , params
                       , query: fst query
                       , session
                       , sidePanelTriggers
                       , tabType
                       , totalRecords
                       } ] ] ]

searchBar :: R.State Query -> R.Element
searchBar (query /\ setQuery) = R.createElement el {} []
  where
    el = R.hooksComponentWithModule thisModule "SearchBar" cpt
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
      setQueryText $ const $ R.unsafeEventValue e

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

type PageParams = {
    listId    :: Int
  , mCorpusId :: Maybe Int
  , nodeId    :: Int
  , tabType   :: TabType
  , query     :: Query
  , params    :: T.Params
  }

getPageHash :: Session -> PageParams -> Aff String
getPageHash session { nodeId, tabType } = do
  (get session $ tableHashRoute nodeId tabType) :: Aff String

convOrderBy :: Maybe (T.OrderByDirection T.ColumnName) -> Maybe OrderBy
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
  , category   : r.category
  , date       : (\(Hyperdata hr) -> hr.pub_year) r.hyperdata
  , ngramCount : r.ngramCount
  , score      : r.score
  , source     : (\(Hyperdata hr) -> hr.source) r.hyperdata
  , title      : (\(Hyperdata hr) -> hr.title) r.hyperdata
  , url        : ""
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
pageLayoutCpt = R.hooksComponentWithModule thisModule "pageLayout" cpt where
  cpt props@{ cacheState
            , frontends
            , listId
            , mCorpusId
            , nodeId
            , params
            , query
            , session
            , sidePanelTriggers
            , tabType } _ = do
    let path = { listId, mCorpusId, nodeId, params, query, tabType }
        handleResponse :: HashedResponse (TableResult Response) -> Tuple Int (Array DocumentsView)
        handleResponse (HashedResponse { hash, value: res }) = ret
          where
            docs = res2corpus <$> filterDocs query res.docs
            ret = if mock then
                      --Tuple 0 (take limit $ drop offset sampleData)
                      Tuple 0 sampleData
                    else
                      Tuple res.count docs
    case cacheState of
      (NT.CacheOn  /\ _) -> do
        let paint (Tuple count docs) = page { documents: docs
                                            , layout: props { totalRecords = count }
                                            , params } []
            mkRequest :: PageParams -> GUC.Request
            mkRequest p = GUC.makeGetRequest session $ tableRoute p

        useLoaderWithCacheAPI {
            cacheEndpoint: getPageHash session
          , handleResponse
          , mkRequest
          , path
          , renderer: paint
          }
      (NT.CacheOff /\ _) -> do
        localCategories <- R.useState' (mempty :: LocalCategories)
        paramsS <- R.useState' params
        let loader p = do
              let route = tableRouteWithPage (p { params = fst paramsS, query = query })
              res <- get session $ route
              liftEffect $ do
                log2 "[pageLayout] table route" route
                log2 "[pageLayout] table res" res
              pure $ handleResponse res
            render (Tuple count documents) = pagePaintRaw { documents
                                                          , layout: props { params = fst paramsS
                                                                          , totalRecords = count }
                                                          , localCategories
                                                          , params: paramsS } []
        useLoader (path { params = fst paramsS }) loader render

type PageProps = (
    documents :: Array DocumentsView
  , layout :: Record PageLayoutProps
  , params :: T.Params
  )

page :: R2.Component PageProps
page = R.createElement pageCpt

pageCpt :: R.Component PageProps
pageCpt = R.hooksComponentWithModule thisModule "pageCpt" cpt where
  cpt { documents, layout, params } _ = do
    paramsS <- R.useState' params
    pure $ pagePaint { documents, layout, params: paramsS } []

type PagePaintProps = (
    documents :: Array DocumentsView
  , layout :: Record PageLayoutProps
  , params :: R.State T.Params
)

pagePaint :: R2.Component PagePaintProps
pagePaint = R.createElement pagePaintCpt

pagePaintCpt :: R.Component PagePaintProps
pagePaintCpt = R.hooksComponentWithModule thisModule "pagePaintCpt" cpt
  where
    cpt { documents, layout, params } _ = do
      localCategories <- R.useState' (mempty :: LocalCategories)
      pure $ pagePaintRaw { documents: A.fromFoldable filteredRows
                          , layout
                          , localCategories
                          , params } []
        where
          orderWith =
            case convOrderBy (fst params).orderBy of
              Just DateAsc    -> sortWith \(DocumentsView { date })   -> date
              Just DateDesc   -> sortWith \(DocumentsView { date })   -> Down date
              Just SourceAsc  -> sortWith \(DocumentsView { source }) -> Str.toLower source
              Just SourceDesc -> sortWith \(DocumentsView { source }) -> Down $ Str.toLower source
              Just TitleAsc   -> sortWith \(DocumentsView { title })  -> Str.toLower title
              Just TitleDesc  -> sortWith \(DocumentsView { title })  -> Down $ Str.toLower title
              _               -> identity -- the server ordering is enough here
          filteredRows = T.filterRows { params: fst params } $ orderWith $ A.toUnfoldable documents


type PagePaintRawProps = (
    documents :: Array DocumentsView
  , layout :: Record PageLayoutProps
  , localCategories :: R.State LocalCategories
  , params :: R.State T.Params
  )

pagePaintRaw :: R2.Component PagePaintRawProps
pagePaintRaw = R.createElement pagePaintRawCpt

pagePaintRawCpt :: R.Component PagePaintRawProps
pagePaintRawCpt = R.hooksComponentWithModule thisModule "pagePaintRawCpt" cpt where
  cpt { documents
      , layout: { frontends
                , listId
                , mCorpusId
                , nodeId
                , session
                , sidePanelTriggers: sidePanelTriggers@{ currentDocIdRef }
                , totalRecords }
      , localCategories
      , params } _ = do

    reload <- R.useState' 0

    pure $ T.table
      { colNames
      , container: T.defaultContainer { title: "Documents" }
      , params
      , rows: rows reload localCategories
      , totalRecords
      , wrapColElts
      }
      where
        sid = sessionId session
        gi Favorite  = "glyphicon glyphicon-star"
        gi _ = "glyphicon glyphicon-star-empty"
        trashClassName Trash _ = "trasn"
        trashClassName _ true = "active"
        trashClassName _ false = ""
        corpusDocument
          | Just cid <- mCorpusId = Routes.CorpusDocument sid cid listId
          | otherwise = Routes.Document sid listId
        colNames = T.ColumnName <$> [ "Tag", "Date", "Title", "Source", "Score"]
        wrapColElts = const identity
        getCategory (lc /\ _) {_id, category} = fromMaybe category (lc ^. at _id)
        rows reload lc@(_ /\ setLocalCategories) = row <$> A.toUnfoldable documents
          where
            row dv@(DocumentsView r) =
              { row:
                T.makeRow [ -- H.div {} [ H.a { className, style, on: {click: click Favorite} } [] ]
                  H.div { className: "column-tag flex" } [
                    caroussel { category: cat, nodeId, row: dv, session, setLocalCategories } []
                  , docChooser { listId, mCorpusId, nodeId: r._id, selected, sidePanelTriggers, tableReload: reload } []
                  ]
                --, H.input { type: "checkbox", defaultValue: checked, on: {click: click Trash} }
                -- TODO show date: Year-Month-Day only
                , H.div { className: tClassName } [ R2.showText r.date ]
                , H.div { className: tClassName } [
                   H.a { href: url frontends $ corpusDocument r._id, target: "_blank"} [ H.text r.title ]
                 ]
                , H.div { className: tClassName } [ H.text $ if r.source == "" then "Source" else r.source ]
                , H.div {} [ H.text $ maybe "-" show r.ngramCount ]
                ]
              , delete: true }
              where
                cat         = getCategory lc r
                checked    = Trash == cat
                tClassName = trashClassName cat selected
                className  = gi cat
                selected   = R.readRef currentDocIdRef == Just r._id

type DocChooser = (
    listId            :: ListId
  , mCorpusId         :: Maybe NodeID
  , nodeId            :: NodeID
  , selected          :: Boolean
  , sidePanelTriggers :: Record SidePanelTriggers
  , tableReload       :: ReloadS
  )

docChooser :: R2.Component DocChooser
docChooser = R.createElement docChooserCpt

docChooserCpt :: R.Component DocChooser
docChooserCpt = R.hooksComponentWithModule thisModule "docChooser" cpt
  where
    cpt { mCorpusId: Nothing } _ = do
      pure $ H.div {} []

    cpt { listId
        , mCorpusId: Just corpusId
        , nodeId
        , selected
        , sidePanelTriggers: { triggerAnnotatedDocIdChange }
        , tableReload: (_ /\ setReload) } _ = do

      let eyeClass = if selected then "fa-eye" else "fa-eye-slash"

      pure $ H.div { className: "doc-chooser" } [
        H.span { className: "fa " <> eyeClass
               , on: { click: onClick } } []
      ]
      where
        onClick _ = do
          -- log2 "[docChooser] onClick, listId" listId
          -- log2 "[docChooser] onClick, corpusId" corpusId
          -- log2 "[docChooser] onClick, nodeId" nodeId
          R2.callTrigger triggerAnnotatedDocIdChange { corpusId, listId, nodeId }
          setReload $ (_ + 1)


newtype SearchQuery = SearchQuery {
    parent_id :: Int
  , query :: Array String
  }


instance encodeJsonSQuery :: EncodeJson SearchQuery where
  encodeJson (SearchQuery {query, parent_id})
    = "query" := query
    ~> "parent_id" := parent_id
    ~> jsonEmptyObject


documentsRoute :: Int -> SessionRoute
documentsRoute nodeId = NodeAPI Node (Just nodeId) "documents"

tableRoute :: forall row. { listId :: Int, nodeId :: Int, tabType :: TabType | row} -> SessionRoute
tableRoute { listId, nodeId, tabType } = NodeAPI Node (Just nodeId) $ "table" <> "?tabType=" <> (showTabType' tabType) <> "&list=" <> (show listId)

tableHashRoute :: Int -> TabType -> SessionRoute
tableHashRoute nodeId tabType = NodeAPI Node (Just nodeId) $ "table/hash" <> "?tabType=" <> (showTabType' tabType)

tableRouteWithPage :: forall row.
                      { listId :: Int
                      , nodeId :: Int
                      , params :: T.Params
                      , query ::  Query
                      , tabType :: TabType
                      | row } -> SessionRoute
tableRouteWithPage { listId, nodeId, params: { limit, offset, orderBy, searchType }, query, tabType } =
  NodeAPI Node (Just nodeId) $ "table" <> joinQueryStrings [tt, lst, lmt, odb, ofs, st, q]
  where
    lmt = queryParam "limit" limit
    lst = queryParam "list" listId
    ofs = queryParam "offset" offset
    odb = mQueryParamS "orderBy" T.orderByToForm orderBy
    st  = queryParam "searchType" searchType
    tt  = queryParamS "tabType" (showTabType' tabType)
    q   = queryParamS "query" query

deleteAllDocuments :: Session -> Int -> Aff (Array Int)
deleteAllDocuments session = delete session <<< documentsRoute

-- TODO: not optimal but Data.Set lacks some function (Set.alter)
toggleSet :: forall a. Ord a => a -> Set a -> Set a
toggleSet a s
  | Set.member a s = Set.delete a s
  | otherwise      = Set.insert a s
