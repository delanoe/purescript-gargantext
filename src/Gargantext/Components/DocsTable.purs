-- TODO: this module should be replaced by FacetsTable
module Gargantext.Components.DocsTable where

import Gargantext.Prelude

import DOM.Simple.Event as DE
import Data.Array (any)
import Data.Array as A
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Lens ((^.))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (class Newtype)
import Data.Ord.Down (Down(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as Str
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Timer (setTimeout)
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..))
import Gargantext.Components.Category (rating)
import Gargantext.Components.Category.Types (Star(..))
import Gargantext.Components.DocsTable.DocumentFormCreation as DFC
import Gargantext.Components.DocsTable.Types (DocumentsView(..), Hyperdata(..), LocalUserScore, Query, Response(..), Year, sampleData, showSource)
import Gargantext.Components.Nodes.Lists.Types as NT
import Gargantext.Components.Nodes.Texts.Types as TextsT
import Gargantext.Components.Reload (textsReloadContext)
import Gargantext.Components.Table as TT
import Gargantext.Components.Table.Types as TT
import Gargantext.Config.REST (AffRESTError, logRESTError)
import Gargantext.Config.Utils (handleRESTError)
import Gargantext.Ends (Frontends, url)
import Gargantext.Hooks.Loader (useLoader, useLoaderWithCacheAPI, HashedResponse(..))
import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId, get, delete)
import Gargantext.Types (ListId, NodeID, NodeType(..), OrderBy(..), SidePanelState(..), TabSubType, TabType, TableResult, showTabType')
import Gargantext.Types as GT
import Gargantext.Utils (sortWith, (?))
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.QueryString (joinQueryStrings, mQueryParam, mQueryParamS, queryParam, queryParamS)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as GUT
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON
import Toestand as T

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

type CommonProps =
  ( boxes          :: Boxes
  , cacheState     :: T.Box NT.CacheState
  , frontends      :: Frontends
  , listId         :: Int
  , mCorpusId      :: Maybe Int
  , nodeId         :: Int
  , session        :: Session
  , sidePanel      :: T.Box (Maybe (Record TextsT.SidePanel))
  , tabType        :: TabType
  -- ^ tabType is not ideal here since it is too much entangled with tabs and
  -- ngramtable. Let's see how this evolves.  )
  , totalRecords   :: Int
  , yearFilter     :: T.Box (Maybe Year)
  )

type LayoutProps =
  ( chart      :: R.Element
  , showSearch :: Boolean
  | CommonProps
  -- , path      :: Record (Path a)
  )

type PageLayoutProps =
  ( key    :: String  -- NOTE Necessary to clear the component when cache state changes
  , params :: TT.Params
  , query  :: Query
  | CommonProps
  )

_documentIdsDeleted  = prop (SProxy :: SProxy "documentIdsDeleted")
_localCategories     = prop (SProxy :: SProxy "localCategories")

docViewLayout :: Record LayoutProps -> R.Element
docViewLayout props = R.createElement docViewLayoutCpt props []
docViewLayoutCpt :: R.Component LayoutProps
docViewLayoutCpt = here.component "docViewLayout" cpt
  where
    cpt layout _children = do
      query <- T.useBox ""
      let params = TT.initialParams
      pure $ docView { layout, params, query } []

type Props = (
    layout :: Record LayoutProps
  , params :: TT.Params
  , query  :: T.Box Query
  )

docView :: R2.Component Props
docView = R.createElement docViewCpt
docViewCpt :: R.Component Props
docViewCpt = here.component "docView" cpt where
  cpt { layout: { boxes
                , cacheState
                , chart
                , frontends
                , listId
                , mCorpusId
                , nodeId
                , session
                , showSearch
                , sidePanel
                , tabType
                , totalRecords
                , yearFilter
                }
      , params
      , query
      } _ = do
    -- State
    cacheState' <- T.useLive T.unequal cacheState
    query' <- T.useLive T.unequal query
    isDocumentModalVisibleBox <- T.useBox false
    onDocumentCreationPending /\ onDocumentCreationPendingBox <-
      R2.useBox' false

    -- Context
    mReloadContext <- R.useContext textsReloadContext

    -- @toggleModalCallback
    toggleModal <- pure $ const $
      T.modify_ not isDocumentModalVisibleBox

    -- @onCreateDocumentEnd <AsyncProgress>
    onCreateDocumentEnd <- pure $ \asyncProgress -> do
      here.log2 "[DocsTables] NodeDocument task:" asyncProgress
      T.write_ false onDocumentCreationPendingBox
      toggleModal unit

      case mReloadContext of
        Nothing -> pure unit
        Just b  -> T2.reload b

    -- @createDocumentCallback
    createDocumentCallback <- pure $ \fdata -> launchAff_ do

      liftEffect $
        T.write_ true onDocumentCreationPendingBox

      eTask <- DFC.create session nodeId fdata

      handleRESTError boxes.errors eTask
        \t -> liftEffect $ launchDocumentCreationProgress
                              boxes
                              session
                              nodeId
                              t
                              onCreateDocumentEnd

    -- Render
    pure $

      R.fragment
      [
        H.div { className: "doc-table-doc-view" }
        [ R2.row
          [ chart
          , if showSearch then searchBar { query } [] else H.div {} []
          , H.div
            { className: "col-md-12 row mb-3" }
            [
              H.div { className: "col-md-4" } []
            ,
              H.button
              { className: "btn btn-light col-md-3"
              , on: { click: toggleModal }
              }
              [
                H.text "Add a document"
              ]
            ]
          , H.div {className: "col-md-12"}
            [ pageLayout { boxes
                        , cacheState
                        , frontends
                        , key: "docView-" <> (show cacheState')
                        , listId
                        , mCorpusId
                        , nodeId
                        , params
                        , query: query'
                        , session
                        , sidePanel
                        , tabType
                        , totalRecords
                        , yearFilter
                        } []
            ]
          ]
        ]
      ,
        -- Document Creation Modal
        B.baseModal
        { isVisibleBox: isDocumentModalVisibleBox
        , title: "Add a new document"
        , hasCollapsibleBackground: false
        }
        [
          DFC.documentFormCreation
          { callback: createDocumentCallback
          , status: onDocumentCreationPending ? Deferred $ Enabled
          }
        ]
      ]

launchDocumentCreationProgress ::
     Boxes
  -> Session
  -> GT.ID
  -> GT.AsyncTaskWithType
  -> (GT.AsyncProgress -> Effect Unit)
  -> Effect Unit
launchDocumentCreationProgress boxes session nodeId currentTask cbk
  = void $ setTimeout 1000 $ launchAff_ $
      scanDocumentCreationProgress boxes session nodeId currentTask cbk

scanDocumentCreationProgress ::
     Boxes
  -> Session
  -> GT.ID
  -> GT.AsyncTaskWithType
  -> (GT.AsyncProgress -> Effect Unit)
  -> Aff Unit
scanDocumentCreationProgress boxes session nodeId currentTask cbk = do

  eTask <- DFC.createProgress session nodeId currentTask

  handleRESTError boxes.errors eTask
    \asyncProgress -> liftEffect do
      let
        GT.AsyncProgress { status } = asyncProgress
        endingStatusList =
          [ GT.IsFinished
          , GT.IsKilled
          , GT.IsFailure
          ]
        hasEndingStatus s = any (_ # s # eq) endingStatusList

      if (hasEndingStatus status)
      then
        cbk asyncProgress
      else
        launchDocumentCreationProgress boxes session nodeId currentTask cbk

---------------------------------------------------

type SearchBarProps =
  ( query :: T.Box Query )

searchBar :: R2.Component SearchBarProps
searchBar = R.createElement searchBarCpt
searchBarCpt :: R.Component SearchBarProps
searchBarCpt = here.component "searchBar" cpt
  where
    cpt { query } _children = do
      query' <- T.useLive T.unequal query
      queryText <- T.useBox query'
      queryText' <- T.useLive T.unequal queryText

      pure $ H.div {className: "col-md-12 row"}
        [ H.div {className: "col-md-3"} []
        , H.div {className: "col-md-1"} [if query' /= "" then (clearButton query) else H.div {} []]
        , H.div {className: "col-md-3 form-group"}
          [ H.input { className: "form-control"
                    , defaultValue: query'
                    , on: { change: onSearchChange queryText
                          , keyUp: onSearchKeyup query queryText' }
                    , placeholder: query'
                    , type: "text" }
          ]
        , H.div {className: "col-md-1"} [ searchButton query queryText' ]
        ]

    onSearchChange :: forall e. T.Box Query -> e -> Effect Unit
    onSearchChange queryText e =
      T.write_ (R.unsafeEventValue e) queryText

    onSearchKeyup :: T.Box Query -> Query -> DE.KeyboardEvent -> Effect Unit
    onSearchKeyup query queryText e =
      if DE.key e == "Enter" then
        T.write_ queryText query
      else
        pure unit

    searchButton query queryText' =
      H.button { className: "btn btn-primary"
               , on: { click: \_ -> T.write_ queryText' query }
               , type: "submit" }
        [ H.span {className: "fa fa-search"} [] ]

    clearButton query =
      H.button { className: "btn btn-danger"
               , on: { click: \_ -> T.write_ "" query } }
        [ H.span {className: "fa fa-times"} [] ]

mock :: Boolean
mock = false

type PageParams = {
    listId      :: Int
  , mCorpusId   :: Maybe Int
  , nodeId      :: Int
  , tabType     :: TabType
  , query       :: Query
  , params      :: TT.Params
  , yearFilter  :: Maybe Year
  }

getPageHash :: Session -> PageParams -> AffRESTError String
getPageHash session { nodeId, tabType } =
  get session $ tableHashRoute nodeId tabType

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

filterDocsByYear :: Year -> Array Response -> Array Response
filterDocsByYear year docs = A.filter filterFunc docs
  where
    filterFunc :: Response -> Boolean
    filterFunc (Response { hyperdata: Hyperdata { pub_year } }) = eq year $ show pub_year

pageLayout :: R2.Component PageLayoutProps
pageLayout = R.createElement pageLayoutCpt
pageLayoutCpt :: R.Component PageLayoutProps
pageLayoutCpt = here.component "pageLayout" cpt where
  cpt props@{ boxes
            , cacheState
            , listId
            , mCorpusId
            , nodeId
            , params
            , query
            , session
            , tabType
            , yearFilter
            } _ = do
    cacheState' <- T.useLive T.unequal cacheState
    yearFilter' <- T.useLive T.unequal yearFilter

    let path = { listId, mCorpusId, nodeId, params, query, tabType, yearFilter: yearFilter' }
        handleResponse :: HashedResponse (TableResult Response) -> Tuple Int (Array DocumentsView)
        handleResponse (HashedResponse { value: res }) = ret
          where
            filterDocs' q ds = case cacheState' of
              NT.CacheOff -> ds
              NT.CacheOn -> filterDocs q ds
            filters = filterDocs' query
                    >>> \res' -> case yearFilter' of
                      Nothing -> res'
                      Just year -> filterDocsByYear year res'

            docs = res2corpus <$> filters res.docs

            ret = if mock then
                      --Tuple 0 (take limit $ drop offset sampleData)
                      Tuple 0 sampleData
                    else
                      Tuple res.count docs

    case cacheState' of
      NT.CacheOn -> do
        let paint (Tuple count docs) = page { boxes
                                            , documents: docs
                                            , layout: props { totalRecords = count }
                                            , params } []
            mkRequest :: PageParams -> GUC.Request
            mkRequest p = GUC.makeGetRequest session $ tableRoute p

        useLoaderWithCacheAPI
          { boxes
          , cacheEndpoint: getPageHash session
          , handleResponse
          , mkRequest
          , path
          , renderer: paint
          }
      NT.CacheOff -> do
        localCategories <- T.useBox (Map.empty :: LocalUserScore)
        paramsS <- T.useBox params
        paramsS' <- T.useLive T.unequal paramsS
        let loader p = do
              let route = tableRouteWithPage (p { params = paramsS', query = query })
              eRes <- get session $ route
              --liftEffect $ do
              --  here.log2 "table route" route
              --  here.log2 "table res" eRes
              pure $ handleResponse <$> eRes
        let render (Tuple count documents) = pagePaintRaw { documents
                                                          , layout: props { params = paramsS'
                                                                          , totalRecords = count }
                                                          , localCategories
                                                          , params: paramsS } []
        let errorHandler = logRESTError here "[pageLayout]"
        useLoader { errorHandler
                  , path: path { params = paramsS' }
                  , loader
                  , render }

type PageProps =
  ( boxes     :: Boxes
  , documents :: Array DocumentsView
  , layout    :: Record PageLayoutProps
  , params    :: TT.Params
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

      localCategories <- T.useBox (Map.empty :: LocalUserScore)
      pure $ pagePaintRaw { documents: A.fromFoldable (filteredRows params')
                          , layout
                          , localCategories
                          , params } []
        where
          orderWith { orderBy } =
            case convOrderBy orderBy of
              Just DateAsc    -> sortWith \(DocumentsView { date })   -> date
              Just DateDesc   -> sortWith \(DocumentsView { date })   -> Down date
              Just SourceAsc  -> sortWith \(DocumentsView { source }) -> Str.toLower $ fromMaybe "" source
              Just SourceDesc -> sortWith \(DocumentsView { source }) -> Down $ Str.toLower $ fromMaybe "" source
              Just TitleAsc   -> sortWith \(DocumentsView { title })  -> Str.toLower title
              Just TitleDesc  -> sortWith \(DocumentsView { title })  -> Down $ Str.toLower title
              _               -> identity -- the server ordering is enough here
          filteredRows params' = TT.filterRows { params: params' } $ (orderWith params') $ A.toUnfoldable documents


type PagePaintRawProps =
  ( documents       :: Array DocumentsView
  , layout          :: Record PageLayoutProps
  , localCategories :: T.Box LocalUserScore
  , params          :: T.Box TT.Params
  )

pagePaintRaw :: R2.Component PagePaintRawProps
pagePaintRaw = R.createElement pagePaintRawCpt
pagePaintRawCpt :: R.Component PagePaintRawProps
pagePaintRawCpt = here.component "pagePaintRawCpt" cpt where
  cpt { documents
      , layout: { boxes
                , frontends
                , listId
                , mCorpusId
                , nodeId
                , session
                , sidePanel
                , totalRecords }
      , localCategories
      , params } _ = do
    mCurrentDocId <- T.useFocused
          (maybe Nothing _.mCurrentDocId)
          (\val -> maybe Nothing (\sp -> Just $ sp { mCurrentDocId = val })) sidePanel
    mCurrentDocId' <- T.useLive T.unequal mCurrentDocId

    reload <- T.useBox GUT.newReload
    localCategories' <- T.useLive T.unequal localCategories

    pure $ TT.table
      { colNames
      , container: TT.defaultContainer
      , params
      , rows: rows reload localCategories' mCurrentDocId'
      , syncResetButton : [ H.div {} [] ]
      , totalRecords
      , wrapColElts
      }
      where
        sid = sessionId session
        trashClassName Star_0 _ = "trash"
        trashClassName _ true = "active"
        trashClassName _ false = ""
        corpusDocument
          | Just cid <- mCorpusId = Routes.CorpusDocument sid cid listId
          | otherwise = Routes.Document sid listId
        colNames = TT.ColumnName <$> [ "Show", "Tag", "Date", "Title", "Source", "Score" ]
        wrapColElts = const identity
        rows reload localCategories' mCurrentDocId' = row reload <$> A.toUnfoldable documents
          where
            row reload dv@(DocumentsView r@{ _id, category }) =
              { row:
                TT.makeRow [ -- H.div {} [ H.a { className, style, on: {click: click Favorite} } [] ]
                            H.div { className: "" }
                                  [ docChooser { boxes
                                               , listId
                                               , mCorpusId
                                               , nodeId: r._id
                                               , sidePanel } []
                                  ]
                          --, H.div { className: "column-tag flex" } [ caroussel { category: cat, nodeId, row: dv, session, setLocalCategories } [] ]
                          , H.div { className: "column-tag flex" }
                                  [ rating { nodeId
                                           , row: dv
                                           , score: cat
                                           , setLocalCategories: \lc -> T.modify_ lc localCategories
                                           , session } [] ]
                --, H.input { type: "checkbox", defaultValue: checked, on: {click: click Trash} }
                -- TODO show date: Year-Month-Day only
                , H.div { className: tClassName } [ R2.showText r.date ]
                , H.div { className: tClassName }
                        [ H.a { href: url frontends $ corpusDocument r._id, target: "_blank" }
                              [ H.text r.title ]
                        ]
                , H.div { className: tClassName } [ H.text $ showSource r.source ]
                , H.div {} [ H.text $ maybe "-" show r.ngramCount ]
                ]
              , delete: true }
              where
                cat         = fromMaybe category (localCategories' ^. at _id)
                -- checked    = Star_1 == cat
                selected   = mCurrentDocId' == Just r._id
                tClassName = trashClassName cat selected

type DocChooser = (
    boxes :: Boxes
  , listId         :: ListId
  , mCorpusId      :: Maybe NodeID
  , nodeId         :: NodeID
  , sidePanel      :: T.Box (Maybe (Record TextsT.SidePanel))
  )

docChooser :: R2.Component DocChooser
docChooser = R.createElement docChooserCpt
docChooserCpt :: R.Component DocChooser
docChooserCpt = here.component "docChooser" cpt
  where
    cpt { mCorpusId: Nothing } _ = do
      pure $ H.div {} []

    cpt { boxes: { sidePanelState }
        , listId
        , mCorpusId: Just corpusId
        , nodeId
        , sidePanel } _ = do
      mCurrentDocId <- T.useFocused
            (maybe Nothing _.mCurrentDocId)
            (\val -> maybe Nothing (\sp -> Just $ sp { mCurrentDocId = val })) sidePanel
      mCurrentDocId' <- T.useLive T.unequal mCurrentDocId

      let selected = mCurrentDocId' == Just nodeId
          eyeClass = if selected then "fa-eye" else "fa-eye-slash"

      pure $ H.div { className: "btn" } [
        H.span { className: "fa " <> eyeClass
               , on: { click: onClick selected } } []
      ]
      where
        onClick selected _ = do
          -- here.log2 "[docChooser] onClick, listId" listId
          -- here.log2 "[docChooser] onClick, corpusId" corpusId
          -- here.log2 "[docChooser] onClick, nodeId" nodeId
          -- R2.callTrigger triggerAnnotatedDocIdChange { corpusId, listId, nodeId }
          -- T2.reload tableReload
          if selected then do
            T.write_ Nothing sidePanel
            T.write_ Closed sidePanelState
          else do
            T.write_ (Just { corpusId: corpusId
                          , listId: listId
                          , mCurrentDocId: Just nodeId
                          , nodeId: nodeId }) sidePanel
            T.write_ Opened sidePanelState
          here.log2 "[docChooser] sidePanel opened" sidePanelState


newtype SearchQuery = SearchQuery {
    parent_id :: Int
  , query :: Array String
  }
derive instance Generic SearchQuery _
derive instance Newtype SearchQuery _
derive newtype instance JSON.ReadForeign SearchQuery


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
                      , yearFilter :: Maybe Year
                      | row } -> SessionRoute
tableRouteWithPage { listId, nodeId, params: { limit, offset, orderBy, searchType }, query, tabType, yearFilter } =
  NodeAPI Node (Just nodeId) $ "table" <> joinQueryStrings [tt, lst, lmt, odb, ofs, st, q, y]
  where
    lmt = queryParam "limit" limit
    lst = queryParam "list" listId
    ofs = queryParam "offset" offset
    odb = mQueryParamS "orderBy" TT.orderByToForm orderBy
    st  = queryParam "searchType" searchType
    tt  = queryParamS "tabType" (showTabType' tabType)
    q   = queryParamS "query" query
    y   = mQueryParam "year" yearFilter

deleteAllDocuments :: Session -> Int -> AffRESTError (Array Int)
deleteAllDocuments session = delete session <<< documentsRoute

-- TODO: not optimal but Data.Set lacks some function (Set.alter)
toggleSet :: forall a. Ord a => a -> Set a -> Set a
toggleSet a s
  | Set.member a s = Set.delete a s
  | otherwise      = Set.insert a s
