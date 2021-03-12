-- TODO: this module should be replaced by FacetsTable
module Gargantext.Components.DocsTable where

import Gargantext.Prelude
  ( class Ord, Unit, bind, const, discard, identity, mempty
  , otherwise, pure, show, unit, ($), (/=), (<$>), (<<<), (<>), (==) )
import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))
import Data.Array as A
import Data.Lens ((^.))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Ord.Down (Down(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as Str
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import DOM.Simple.Event as DE
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Components.Category (rating)
import Gargantext.Components.Category.Types (Star(..))
import Gargantext.Components.DocsTable.Types
  ( DocumentsView(..), Hyperdata(..), LocalUserScore, Query, Response(..), sampleData )
import Gargantext.Components.Table.Types as TT
import Gargantext.Components.Nodes.Lists.Types as NT
import Gargantext.Components.Nodes.Texts.Types (SidePanelTriggers)
import Gargantext.Components.Table as TT
import Gargantext.Ends (Frontends, url)
import Gargantext.Hooks.Loader (useLoader, useLoaderWithCacheAPI, HashedResponse(..))
import Gargantext.Routes as Routes
import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, sessionId, get, delete)
import Gargantext.Types (ListId, NodeID, NodeType(..), OrderBy(..), TableResult, TabSubType, TabType, showTabType')
import Gargantext.Utils (sortWith)
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.QueryString (joinQueryStrings, mQueryParamS, queryParam, queryParamS)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.DocsTable"

type TotalRecords = Int

type Path a =
  ( corpusId  :: Int
  , listId    :: Int
  , frontends :: Frontends
  , session   :: Session
  , tabType   :: TabSubType a
  )

type LayoutProps =
  ( cacheState      :: T.Box NT.CacheState
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

type PageLayoutProps =
  ( cacheState        :: T.Box NT.CacheState
  , frontends         :: Frontends
  , key               :: String  -- NOTE Necessary to clear the component when cache state changes
  , listId            :: Int
  , mCorpusId         :: Maybe Int
  , nodeId            :: Int
  , params            :: TT.Params
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
docViewLayoutCpt = here.component "docViewLayout" cpt
  where
    cpt layout _children = do
      query <- R.useState' ""
      let params = TT.initialParams
      pure $ docView { layout, params, query } []

type Props = (
    layout :: Record LayoutProps
  , params :: TT.Params
  , query :: R.State Query
  )

docView :: R2.Component Props
docView = R.createElement docViewCpt

docViewCpt :: R.Component Props
docViewCpt = here.component "docView" cpt where
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
    cacheState' <- T.useLive T.unequal cacheState

    pure $ H.div { className: "doc-table-doc-view container1" }
      [ R2.row
        [ chart
        , if showSearch then searchBar query else H.div {} []
        , H.div {className: "col-md-12"}
          [ pageLayout { cacheState
                       , frontends
                       , key: "docView-" <> (show cacheState')
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
    el = here.component "SearchBar" cpt
    cpt {} _children = do
      queryText <- R.useState' query

      pure $ H.div {className: "col-md-12 row"}
        [ H.div {className: "col-md-3"} []
        , H.div {className: "col-md-1"} [if query /= "" then clearButton else H.div {} []]
        , H.div {className: "col-md-3 form-group"}
          [ H.input { type: "text"
                    , className: "form-control"
                    , on: {change: onSearchChange queryText, keyUp: onSearchKeyup queryText}
                    , placeholder: query
                    , defaultValue: query}
          ]
        , H.div {className: "col-md-1"} [searchButton queryText]
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
               , className: "btn btn-primary"
               , on: {click: \e -> setQuery $ const queryText}}
      [ H.span {className: "fa fa-search"} [] ]

    clearButton =
      H.button { className: "btn btn-danger"
               , on: {click: \e -> setQuery $ const ""}}
      [ H.span {className: "fa fa-times"} [] ]

mock :: Boolean
mock = false

type PageParams = {
    listId    :: Int
  , mCorpusId :: Maybe Int
  , nodeId    :: Int
  , tabType   :: TabType
  , query     :: Query
  , params    :: TT.Params
  }

getPageHash :: Session -> PageParams -> Aff String
getPageHash session { nodeId, tabType } = do
  (get session $ tableHashRoute nodeId tabType) :: Aff String

convOrderBy :: Maybe (TT.OrderByDirection TT.ColumnName) -> Maybe OrderBy
convOrderBy (Just (TT.ASC  (TT.ColumnName "Date")))  = Just DateAsc
convOrderBy (Just (TT.DESC (TT.ColumnName "Date")))  = Just DateDesc
convOrderBy (Just (TT.ASC  (TT.ColumnName "Title"))) = Just TitleAsc
convOrderBy (Just (TT.DESC (TT.ColumnName "Title"))) = Just TitleDesc
convOrderBy (Just (TT.ASC  (TT.ColumnName "Source"))) = Just SourceAsc
convOrderBy (Just (TT.DESC (TT.ColumnName "Source"))) = Just SourceDesc
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
pageLayoutCpt = here.component "pageLayout" cpt where
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
    cacheState' <- T.useLive T.unequal cacheState

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
    case cacheState' of
      NT.CacheOn -> do
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
      NT.CacheOff -> do
        localCategories <- R.useState' (mempty :: LocalUserScore)
        paramsS <- T.useBox params
        paramsS' <- T.useLive T.unequal paramsS
        let loader p = do
              let route = tableRouteWithPage (p { params = paramsS', query = query })
              res <- get session $ route
              liftEffect $ do
                log2 "[pageLayout] table route" route
                log2 "[pageLayout] table res" res
              pure $ handleResponse res
            render (Tuple count documents) = pagePaintRaw { documents
                                                          , layout: props { params = paramsS'
                                                                          , totalRecords = count }
                                                          , localCategories
                                                          , params: paramsS } []
        useLoader (path { params = paramsS' }) loader render

type PageProps = (
    documents :: Array DocumentsView
  , layout :: Record PageLayoutProps
  , params :: TT.Params
  )

page :: R2.Component PageProps
page = R.createElement pageCpt

pageCpt :: R.Component PageProps
pageCpt = here.component "pageCpt" cpt where
  cpt { documents, layout, params } _ = do
    paramsS <- T.useBox params

    pure $ pagePaint { documents, layout, params: paramsS } []

type PagePaintProps = (
    documents :: Array DocumentsView
  , layout :: Record PageLayoutProps
  , params :: T.Box TT.Params
)

pagePaint :: R2.Component PagePaintProps
pagePaint = R.createElement pagePaintCpt

pagePaintCpt :: R.Component PagePaintProps
pagePaintCpt = here.component "pagePaintCpt" cpt
  where
    cpt { documents, layout, params } _ = do
      params' <- T.useLive T.unequal params

      localCategories <- R.useState' (mempty :: LocalUserScore)
      pure $ pagePaintRaw { documents: A.fromFoldable (filteredRows params')
                          , layout
                          , localCategories
                          , params } []
        where
          orderWith { orderBy } =
            case convOrderBy orderBy of
              Just DateAsc    -> sortWith \(DocumentsView { date })   -> date
              Just DateDesc   -> sortWith \(DocumentsView { date })   -> Down date
              Just SourceAsc  -> sortWith \(DocumentsView { source }) -> Str.toLower source
              Just SourceDesc -> sortWith \(DocumentsView { source }) -> Down $ Str.toLower source
              Just TitleAsc   -> sortWith \(DocumentsView { title })  -> Str.toLower title
              Just TitleDesc  -> sortWith \(DocumentsView { title })  -> Down $ Str.toLower title
              _               -> identity -- the server ordering is enough here
          filteredRows params' = TT.filterRows { params: params' } $ (orderWith params') $ A.toUnfoldable documents


type PagePaintRawProps = (
    documents :: Array DocumentsView
  , layout :: Record PageLayoutProps
  , localCategories :: R.State LocalUserScore
  , params :: T.Box TT.Params
  )

pagePaintRaw :: R2.Component PagePaintRawProps
pagePaintRaw = R.createElement pagePaintRawCpt

pagePaintRawCpt :: R.Component PagePaintRawProps
pagePaintRawCpt = here.component "pagePaintRawCpt" cpt where
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

    reload <- T.useBox T2.newReload

    pure $ TT.table
      { syncResetButton : [ H.div {} [] ]
      , colNames
      , container: TT.defaultContainer { title: "Documents" }
      , params
      , rows: rows reload localCategories
      , totalRecords
      , wrapColElts
      }
      where
        sid = sessionId session
        gi Star_1  = "fa fa-star"
        gi _ = "fa fa-star-empty"
        trashClassName Star_0 _ = "trash"
        trashClassName _ true = "active"
        trashClassName _ false = ""
        corpusDocument
          | Just cid <- mCorpusId = Routes.CorpusDocument sid cid listId
          | otherwise = Routes.Document sid listId
        colNames = TT.ColumnName <$> [ "Show", "Tag", "Date", "Title", "Source", "Score" ]
        wrapColElts = const identity
        getCategory (lc /\ _) {_id, category} = fromMaybe category (lc ^. at _id)
        rows reload lc@(_ /\ setLocalCategories) = row <$> A.toUnfoldable documents
          where
            row dv@(DocumentsView r) =
              { row:
                TT.makeRow [ -- H.div {} [ H.a { className, style, on: {click: click Favorite} } [] ]
                            H.div { className: "" } [ docChooser { listId, mCorpusId, nodeId: r._id, selected, sidePanelTriggers, tableReload: reload } []
                                                                   ]
                          --, H.div { className: "column-tag flex" } [ caroussel { category: cat, nodeId, row: dv, session, setLocalCategories } [] ]
                          , H.div { className: "column-tag flex" } [ rating { score: cat, nodeId, row: dv, session, setLocalCategories } [] ]
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
                checked    = Star_1 == cat
                tClassName = trashClassName cat selected
                className  = gi cat
                selected   = R.readRef currentDocIdRef == Just r._id

type DocChooser = (
    listId            :: ListId
  , mCorpusId         :: Maybe NodeID
  , nodeId            :: NodeID
  , selected          :: Boolean
  , sidePanelTriggers :: Record SidePanelTriggers
  , tableReload       :: T2.ReloadS
  )

docChooser :: R2.Component DocChooser
docChooser = R.createElement docChooserCpt

docChooserCpt :: R.Component DocChooser
docChooserCpt = here.component "docChooser" cpt
  where
    cpt { mCorpusId: Nothing } _ = do
      pure $ H.div {} []

    cpt { listId
        , mCorpusId: Just corpusId
        , nodeId
        , selected
        , sidePanelTriggers: { triggerAnnotatedDocIdChange }
        , tableReload } _ = do

      let eyeClass = if selected then "fa-eye" else "fa-eye-slash"

      pure $ H.div { className: "btn" } [
        H.span { className: "fa " <> eyeClass
               , on: { click: onClick } } []
      ]
      where
        onClick _ = do
          -- log2 "[docChooser] onClick, listId" listId
          -- log2 "[docChooser] onClick, corpusId" corpusId
          -- log2 "[docChooser] onClick, nodeId" nodeId
          R2.callTrigger triggerAnnotatedDocIdChange { corpusId, listId, nodeId }
          T2.reload tableReload


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
                      , params :: TT.Params
                      , query ::  Query
                      , tabType :: TabType
                      | row } -> SessionRoute
tableRouteWithPage { listId, nodeId, params: { limit, offset, orderBy, searchType }, query, tabType } =
  NodeAPI Node (Just nodeId) $ "table" <> joinQueryStrings [tt, lst, lmt, odb, ofs, st, q]
  where
    lmt = queryParam "limit" limit
    lst = queryParam "list" listId
    ofs = queryParam "offset" offset
    odb = mQueryParamS "orderBy" TT.orderByToForm orderBy
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
