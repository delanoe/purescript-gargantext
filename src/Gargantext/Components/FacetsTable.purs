-- TODO: this module should replace DocsTable
--      However the fix for favorites in commit 91cb6bd9906e128b3129b1db01ef6ef5ae13f7f8
--       has not been ported to this module yet.
module Gargantext.Components.FacetsTable where

import Control.Monad.Cont.Trans (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Array (filter, (!!))
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H
------------------------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Config (Ends, NodeType(..), OrderBy(..), NodePath(..), BackendRoute(..), TabType, url)
import Gargantext.Config.REST (put, post, deleteWithBody)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Components.Search.Types (Category(..), CategoryQuery(..), favCategory, trashCategory, decodeCategory, putCategories)

import Gargantext.Components.Table as T
import Gargantext.Router as Router
import Gargantext.Utils (toggleSet)
import Gargantext.Utils.DecodeMaybe ((.|))
import Gargantext.Utils.Reactix as R2
------------------------------------------------------------------------

type NodeID = Int
type TotalRecords = Int

-- Example:
--   [["machine","learning"],["artificial","intelligence"]]
-- This searches for documents with "machine learning" or "artificial intelligence"
type TextQuery = Array (Array String)

newtype SearchQuery = SearchQuery { query :: TextQuery }

instance encodeJsonSearchQuery :: EncodeJson SearchQuery where
  encodeJson (SearchQuery post)
     = "query"     := post.query !! 0 -- TODO anoe
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
  , ends :: Ends
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
    cpt {ends, nodeId, listId, query, totalRecords, chart, container} _ = do
      deletions <- R.useState' initialDeletions
      path <- R.useState' $ initialPagePath {nodeId, listId, query, ends}
      pure $ H.div { className: "container1" }
        [ H.div { className: "row" }
          [ chart
          , H.div { className: "col-md-12" }
            [ pageLayout { deletions, totalRecords, container, ends, path } ]
          , H.div { className: "col-md-12" }
            [ H.button { style: buttonStyle, on: { click: trashClick deletions } }
              [ H.i { className: "glyphitem glyphicon glyphicon-trash"
                    , style: { marginRight : "9px" }} []
              , H.text "Delete document!" ] ] ] ]
        where
          buttonStyle =
            { backgroundColor: "peru", padding: "9px", color: "white"
            , border: "white", float: "right" }
          trashClick deletions _ = performDeletions ends nodeId deletions

performDeletions :: Ends -> Int -> R.State Deletions -> Effect Unit
performDeletions ends nodeId (deletions /\ setDeletions) =
  launchAff_ call *> setDeletions del
  where
    q = {documents: Set.toUnfoldable deletions.pending}
    call = deleteDocuments ends nodeId (DeleteDocumentQuery q)
    del {pending, deleted} = {pending: mempty, deleted: deleted <> pending}

-- markCategory :: Ends -> NodeID -> _ -> Array NodeID -> Effect Unit
markCategory ends nodeId category nids =
  void $ launchAff_ $putCategories ends nodeId (CategoryQuery q)
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
    cpt {ends, nodeId, listId, query, totalRecords, chart, container} _ = do
      deletions <- R.useState' initialDeletions
      let buttonStyle = { backgroundColor: "peru", padding : "9px"
                        , color : "white", border : "white", float: "right"}
      let performClick = \_ -> performDeletions ends nodeId deletions
      path <- R.useState' $ initialPagePath { nodeId, listId, query, ends }
      pure $ R.fragment
        [ H.br {}
        , H.p {} [ H.text "" ]
        , H.br {}
        , H.div { className: "container-fluid" }
          [ H.div { className: "row" }
            [ chart
            , H.div { className: "col-md-12" }
              [ pageLayout { totalRecords, deletions, container, ends, path }
              , H.button { style: buttonStyle, on: { click: performClick } }
                [ H.i { className: "glyphitem glyphicon glyphicon-trash"
                      , style: { marginRight : "9px" } } []
                , H.text "Delete document!" ] ] ] ] ]

type PagePath = {nodeId :: Int, listId :: Int, query :: TextQuery, params :: T.Params, ends :: Ends}

initialPagePath :: {ends :: Ends, nodeId :: Int, listId :: Int, query :: TextQuery} -> PagePath
initialPagePath {ends, nodeId, listId, query} = {ends, nodeId, listId, query, params: T.initialParams}

loadPage :: PagePath -> Aff (Array DocumentsView)
loadPage {ends, nodeId, listId, query, params: {limit, offset, orderBy}} = do
  logs "loading documents page: loadPage with Offset and limit"
  let url2 = url ends $ Search { listId, offset, limit, orderBy: convOrderBy <$> orderBy } (Just nodeId)
  SearchResults res <- post url2 $ SearchQuery {query}
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
  , ends :: Ends
  , path :: R.State PagePath
  )

type PageProps = ( documents :: Array DocumentsView | PageLayoutProps )

-- | Loads and renders a page
pageLayout :: Record PageLayoutProps -> R.Element
pageLayout props = R.createElement pageLayoutCpt props []

pageLayoutCpt :: R.Component PageLayoutProps
pageLayoutCpt = R.hooksComponent "G.C.FacetsTable.PageLayout" cpt
  where
    cpt {totalRecords, deletions, container, ends, path} _ = do
      useLoader (fst path) loadPage $ \documents ->
        page {totalRecords, deletions, container, ends, path, documents}

page :: Record PageProps -> R.Element
page props = R.createElement pageCpt props []

pageCpt :: R.Component PageProps
pageCpt = R.staticComponent "G.C.FacetsTable.Page" cpt
  where
    cpt {totalRecords, container, deletions, documents, ends, path: path@({nodeId, listId, query} /\ setPath)} _ = do
      T.table { rows, container, colNames, totalRecords, setParams }
      where
        setParams params = setPath (_ {params = params})
        colNames = T.ColumnName <$> [ "", "Date", "Title", "Source", "Authors", "Delete" ]
        -- TODO: how to interprete other scores?
        gi Favorite = "glyphicon glyphicon-star-empty"
        gi _ = "glyphicon glyphicon-star"
        isChecked id = Set.member id (fst deletions).pending
        isDeleted (DocumentsView {id}) = Set.member id (fst deletions).deleted
        pairUrl (Pair {id,label})
          | id > 1 = H.a { href, target: "blank" } [ H.text label ]
            where href = url ends $ NodePath NodeContact (Just id)
          | otherwise = H.text label
        comma = H.span {} [ H.text ", " ]
        rows = row <$> filter (not <<< isDeleted) documents
          where
            row (DocumentsView {id,score,title,source,date,pairs,delete,category}) =
              { row:
                [ H.div {}
                  [ H.a { className, on: {click: markClick} } []
                    -- TODO show date: Year-Month-Day only
                  , maybeStricken [ H.text date ]
                  , maybeStricken [ H.text source ]
                  -- , maybeStricken $ intercalate [comma] (pairUrl <$> pairs)
                  , H.input { type: "checkbox", checked: isChecked id, on: { click: toggleClick } }
                  ] ]
              , delete: true }
              where
                markClick _ = markCategory ends nodeId category [id]
                toggleClick _ = togglePendingDeletion deletions id
                className = gi category
                maybeStricken
                  | delete = H.div { style: { textDecoration: "line-through" } }
                  | otherwise = H.div {}


---------------------------------------------------------

newtype DeleteDocumentQuery = DeleteDocumentQuery { documents :: Array Int }

instance encodeJsonDDQuery :: EncodeJson DeleteDocumentQuery where
  encodeJson (DeleteDocumentQuery post) =
    "documents" := post.documents ~> jsonEmptyObject

deleteDocuments :: Ends -> Int -> DeleteDocumentQuery -> Aff (Array Int)
deleteDocuments ends nodeId = deleteWithBody to
  where to = url ends (NodeAPI Node (Just nodeId)) <> "/documents"
