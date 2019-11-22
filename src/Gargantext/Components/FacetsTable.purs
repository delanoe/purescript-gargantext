-- TODO: this module should replace DocsTable
--      However the fix for favorites in commit 91cb6bd9906e128b3129b1db01ef6ef5ae13f7f8
--       has not been ported to this module yet.
module Gargantext.Components.FacetsTable where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Array (filter, (!!))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_)
import Reactix as R
import Reactix.DOM.HTML as H
------------------------------------------------------------------------
import Gargantext.Ends (url)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Components.Search.Types (Category(..), CategoryQuery(..), favCategory, decodeCategory, putCategories)
import Gargantext.Components.Table as T
import Gargantext.Routes (SessionRoute(Search,NodeAPI))
import Gargantext.Sessions (Session, sessionId, post, deleteWithBody)
import Gargantext.Types (NodeType(..), OrderBy(..), NodePath(..))
import Gargantext.Utils (toggleSet)
import Gargantext.Utils.DecodeMaybe ((.|))
------------------------------------------------------------------------

type NodeID = Int
type TotalRecords = Int

-- Example:
--   [["machine","learning"],["artificial","intelligence"]]
-- This searches for documents with "machine learning" or "artificial intelligence"
type TextQuery = Array (Array String)

newtype SearchQuery = SearchQuery { query :: TextQuery }

instance encodeJsonSearchQuery :: EncodeJson SearchQuery where
  encodeJson (SearchQuery {query})
     = "query"     := query !! 0 -- TODO anoe
    ~> jsonEmptyObject

newtype SearchResults = SearchResults { results :: Array Response }

instance decodeSearchResults :: DecodeJson SearchResults where
  decodeJson json = do
    obj     <- decodeJson json
    results <- obj .: "results"
    pure $ SearchResults {results}

type Props =
  ( nodeId :: Int
  , listId :: Int
  , query :: TextQuery
  , totalRecords :: Int
  , chart :: R.Element
  , container :: Record T.TableContainerProps -> R.Element
  , session :: Session
  )

-- | Tracks the ids of documents to delete and that have been deleted
type Deletions = { pending :: Set Int, deleted :: Set Int }

initialDeletions :: Deletions
initialDeletions = { pending: mempty, deleted: mempty }

newtype Pair = Pair { id :: Int, label :: String }

derive instance genericPair :: Generic Pair _

instance showPair :: Show Pair where
  show = genericShow

newtype DocumentsView =
  DocumentsView
  { id       :: Int
  , date     :: String
  , title    :: String
  , source   :: String
  , score    :: Int
  , pairs    :: Array Pair
  , delete   :: Boolean
  , category :: Category
  }

derive instance genericDocumentsView :: Generic DocumentsView _

instance showDocumentsView :: Show DocumentsView where
  show = genericShow

newtype Response = Response
  { id         :: Int
  , created    :: String
  , hyperdata  :: Hyperdata
  , category   :: Category
  , ngramCount :: Int
-- , date      :: String
-- , score     :: Int
-- , pairs     :: Array Pair
  }

newtype Hyperdata = Hyperdata
  { title  :: String
  , source :: String
  }

--instance decodeHyperdata :: DecodeJson Hyperdata where
--  decodeJson json = do
--    obj    <- decodeJson json
--    title  <- obj .: "title"
--    source <- obj .: "source"
--    pure $ Hyperdata { title,source }

instance decodePair :: DecodeJson Pair where
  decodeJson json = do
    obj   <- decodeJson json
    id    <- obj .: "id"
    label <- obj .: "label"
    pure $ Pair { id, label }

instance decodeHyperdata :: DecodeJson Hyperdata where
  decodeJson json = do
    obj    <- decodeJson json
    title  <- obj .| "title"
    source <- obj .| "source"
    pure $ Hyperdata { title,source }

{-
instance decodeResponse :: DecodeJson Response where
  decodeJson json = do
    obj       <- decodeJson json
    id        <- obj .: "id"
    -- date      <- obj .: "date" -- TODO
    date      <- pure "2018"
    score     <- obj .: "score"
    hyperdata <- obj .: "hyperdata"
    pairs     <- obj .: "pairs"
    pure $ Response { id, date, score, hyperdata, pairs }
-}

instance decodeResponse :: DecodeJson Response where
  decodeJson json = do
    obj        <- decodeJson json
    id         <- obj .: "id"
    created    <- obj .: "created"
    hyperdata  <- obj .: "hyperdata"
    favorite   <- obj .: "favorite"
    --ngramCount <- obj .: "ngramCount"
    let ngramCount = 1
    pure $ Response { id, created, hyperdata, category: decodeCategory favorite, ngramCount}

-- | Main layout of the Documents Tab of a Corpus
docView :: Record Props -> R.Element
docView props = R.createElement docViewCpt props []

docViewCpt :: R.Component Props
docViewCpt = R.hooksComponent "G.C.FacetsTable.DocView" cpt
  where
    cpt {session, nodeId, listId, query, totalRecords, chart, container} _ = do
      deletions <- R.useState' initialDeletions
      path <- R.useState' $ initialPagePath {nodeId, listId, query, session}
      pure $ H.div { className: "container1" }
        [ H.div { className: "row" }
          [ chart
          , H.div { className: "col-md-12" }
            [ pageLayout { deletions, totalRecords, container, session, path } ]
          , H.div { className: "col-md-12" }
            [ H.button { style: buttonStyle, on: { click: trashClick deletions } }
              [ H.i { className: "glyphitem glyphicon glyphicon-trash"
                    , style: { marginRight : "9px" }} []
              , H.text "Delete document!" ] ] ] ]
        where
          buttonStyle =
            { backgroundColor: "peru", padding: "9px", color: "white"
            , border: "white", float: "right" }
          trashClick deletions _ = performDeletions session nodeId deletions

performDeletions :: Session -> Int -> R.State Deletions -> Effect Unit
performDeletions session nodeId (deletions /\ setDeletions) =
  launchAff_ call *> setDeletions del
  where
    q = {documents: Set.toUnfoldable deletions.pending}
    call = deleteDocuments session nodeId (DeleteDocumentQuery q)
    del {pending, deleted} = {pending: mempty, deleted: deleted <> pending}

markCategory :: Session -> NodeID -> Category -> Array NodeID -> Effect Unit
markCategory session nodeId category nids =
  void $ launchAff_ $putCategories session nodeId (CategoryQuery q)
  where -- TODO add array of delete rows here
    q = {nodeIds: nids, category: favCategory category}

togglePendingDeletion :: R.State Deletions -> NodeID -> Effect Unit
togglePendingDeletion (_ /\ setDeletions) nid = setDeletions setter
  where setter deletions@{pending} = deletions { pending = toggleSet nid pending }

docViewGraph :: Record Props -> R.Element
docViewGraph props = R.createElement docViewCpt props []

docViewGraphCpt :: R.Component Props
docViewGraphCpt = R.hooksComponent "FacetsDocViewGraph" cpt
  where
    cpt {session, nodeId, listId, query, totalRecords, chart, container} _ = do
      deletions <- R.useState' initialDeletions
      let buttonStyle = { backgroundColor: "peru", padding : "9px"
                        , color : "white", border : "white", float: "right"}
      let performClick = \_ -> performDeletions session nodeId deletions
      path <- R.useState' $ initialPagePath { nodeId, listId, query, session }
      pure $ R.fragment
        [ H.br {}
        , H.p {} [ H.text "" ]
        , H.br {}
        , H.div { className: "container-fluid" }
          [ H.div { className: "row" }
            [ chart
            , H.div { className: "col-md-12" }
              [ pageLayout { totalRecords, deletions, container, session, path }
              , H.button { style: buttonStyle, on: { click: performClick } }
                [ H.i { className: "glyphitem glyphicon glyphicon-trash"
                      , style: { marginRight : "9px" } } []
                , H.text "Delete document!" ] ] ] ] ]

type PagePath = {nodeId :: Int, listId :: Int, query :: TextQuery, params :: T.Params, session :: Session}

initialPagePath :: {session :: Session, nodeId :: Int, listId :: Int, query :: TextQuery} -> PagePath
initialPagePath {session, nodeId, listId, query} = {session, nodeId, listId, query, params: T.initialParams}

loadPage :: PagePath -> Aff (Array DocumentsView)
loadPage {session, nodeId, listId, query, params: {limit, offset, orderBy}} = do
  liftEffect $ log "loading documents page: loadPage with Offset and limit"
  let p = Search { listId, offset, limit, orderBy: convOrderBy <$> orderBy } (Just nodeId)
  SearchResults res <- post session p $ SearchQuery {query}
  pure $ res2corpus <$> res.results
  where
    res2corpus :: Response -> DocumentsView
    res2corpus (Response { id, created: date, ngramCount: score, category
                         , hyperdata: Hyperdata {title, source} }) =
      DocumentsView { id, date, title, source, score, category, pairs: [], delete: false }
    convOrderBy (T.ASC  (T.ColumnName "Date")) = DateAsc
    convOrderBy (T.DESC (T.ColumnName "Date")) = DateDesc
    convOrderBy (T.ASC  (T.ColumnName "Title")) = TitleAsc
    convOrderBy (T.DESC (T.ColumnName "Title")) = TitleDesc
    convOrderBy (T.ASC  (T.ColumnName "Source")) = SourceAsc
    convOrderBy (T.DESC (T.ColumnName "Source")) = SourceDesc
    convOrderBy _ = DateAsc -- TODO

type PageLayoutProps =
  ( totalRecords :: Int
  , deletions :: R.State Deletions
  , container :: Record T.TableContainerProps -> R.Element
  , session :: Session
  , path :: R.State PagePath
  )

type PageProps = ( documents :: Array DocumentsView | PageLayoutProps )

-- | Loads and renders a page
pageLayout :: Record PageLayoutProps -> R.Element
pageLayout props = R.createElement pageLayoutCpt props []

pageLayoutCpt :: R.Component PageLayoutProps
pageLayoutCpt = R.hooksComponent "G.C.FacetsTable.PageLayout" cpt
  where
    cpt {totalRecords, deletions, container, session, path} _ = do
      useLoader (fst path) loadPage $ \documents ->
        page {totalRecords, deletions, container, session, path, documents}

page :: Record PageProps -> R.Element
page props = R.createElement pageCpt props []

pageCpt :: R.Component PageProps
pageCpt = R.staticComponent "G.C.FacetsTable.Page" cpt
  where
    cpt {totalRecords, container, deletions, documents, session, path: path@({nodeId, listId, query} /\ setPath)} _ = do
      T.table { rows, container, colNames, totalRecords, params }
      where
        setParams f = setPath $ \p@{params: ps} -> p {params = f ps}
        params = (fst path).params /\ setParams
        colNames = T.ColumnName <$> [ "", "Date", "Title", "Source", "Authors", "Delete" ]
        -- TODO: how to interprete other scores?
        gi Favorite = "glyphicon glyphicon-star-empty"
        gi _ = "glyphicon glyphicon-star"
        isChecked id = Set.member id (fst deletions).pending
        isDeleted (DocumentsView {id}) = Set.member id (fst deletions).deleted
        pairUrl (Pair {id,label})
          | id > 1 = H.a { href, target: "blank" } [ H.text label ]
            where href = url session $ NodePath (sessionId session) NodeContact (Just id)
          | otherwise = H.text label
        comma = H.span {} [ H.text ", " ]
        rows = row <$> filter (not <<< isDeleted) documents
          where
            row (DocumentsView {id,score,title,source,date,pairs,delete,category}) =
              { row:
                [ H.a { className, on: {click: markClick} } []
                  -- TODO show date: Year-Month-Day only
                , maybeStricken [ H.text date ]
                , maybeStricken [ H.text source ]
                  -- , maybeStricken $ intercalate [comma] (pairUrl <$> pairs)
                , H.input { type: "checkbox", checked: isChecked id, on: { click: toggleClick } }
                ]
              , delete: true }
              where
                markClick _ = markCategory session nodeId category [id]
                toggleClick _ = togglePendingDeletion deletions id
                className = gi category
                maybeStricken
                  | delete = H.div { style: { textDecoration: "line-through" } }
                  | otherwise = H.div {}

---------------------------------------------------------

newtype DeleteDocumentQuery = DeleteDocumentQuery { documents :: Array Int }

instance encodeJsonDDQuery :: EncodeJson DeleteDocumentQuery where
  encodeJson (DeleteDocumentQuery {documents}) =
    "documents" := documents ~> jsonEmptyObject

deleteDocuments :: Session -> Int -> DeleteDocumentQuery -> Aff (Array Int)
deleteDocuments session nodeId =
  deleteWithBody session $ NodeAPI Node (Just nodeId) "documents"

