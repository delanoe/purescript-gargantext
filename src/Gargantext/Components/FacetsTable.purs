-- TODO: this module should replace DocsTable
--      However the fix for favorites in commit 91cb6bd9906e128b3129b1db01ef6ef5ae13f7f8
--       has not been ported to this module yet.
module Gargantext.Components.FacetsTable where

------------------------------------------------------------------------
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Array (concat, filter)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.String as String
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Gargantext.Components.Category (Category(..), CategoryQuery(..), favCategory, decodeCategory, putCategories)
import Gargantext.Components.Search
import Gargantext.Components.Table as T
import Gargantext.Ends (url, Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(Search, NodeAPI))
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId, post, deleteWithBody)
import Gargantext.Types (NodeType(..), OrderBy(..), NodePath(..), NodeID)
import Gargantext.Utils (toggleSet, zeroPad)
import Gargantext.Utils.DecodeMaybe ((.|))
import Gargantext.Utils.Reactix as R2
import Prelude
import Reactix as R
import Reactix.DOM.HTML as H
------------------------------------------------------------------------

type Props =
  ( chart :: R.Element
  , container :: Record T.TableContainerProps -> R.Element
  , frontends :: Frontends
  , listId :: Int
  , nodeId :: Int
  , query :: SearchQuery
  , session :: Session
  , totalRecords :: Int
  )

-- | Tracks the ids of documents to delete and that have been deleted
type Deletions = { pending :: Set Int, deleted :: Set Int }

initialDeletions :: Deletions
initialDeletions = { pending: mempty, deleted: mempty }

newtype Pair =
  Pair { id    :: Int
       , label :: String
       }

derive instance genericPair :: Generic Pair _

instance showPair :: Show Pair where
  show = genericShow

newtype DocumentsView =
  DocumentsView
  { id       :: Int
  , date     :: String
  , title    :: String
  , source   :: String
  , authors  :: String
  , score    :: Int
  , pairs    :: Array Pair
  , delete   :: Boolean
  , category :: Category
  , publication_year :: Int
  , publication_month :: Int
  , publication_day  :: Int
  }

publicationDate :: DocumentsView -> String
publicationDate (DocumentsView {publication_year, publication_month, publication_day}) =
  (zeroPad 2 publication_year) <> "-" <> (zeroPad 2 publication_month) <> "-" <> (zeroPad 2 publication_day)

derive instance genericDocumentsView :: Generic DocumentsView _

instance showDocumentsView :: Show DocumentsView where
  show = genericShow

-- | Main layout of the Documents Tab of a Corpus
docView :: Record Props -> R.Element
docView props = R.createElement docViewCpt props []

docViewCpt :: R.Component Props
docViewCpt = R.hooksComponent "G.C.FacetsTable.DocView" cpt
  where
    cpt {frontends, session, nodeId, listId, query, totalRecords, chart, container} _ = do
      deletions <- R.useState' initialDeletions
      path <- R.useState' $ initialPagePath {nodeId, listId, query, session}

      R.useEffect' $ do
        let ipp = initialPagePath {nodeId, listId, query, session}
        if fst path == ipp then
          pure unit
        else
          snd path $ const ipp

      pure $ H.div { className: "container1" }
        [ R2.row
          [ chart
          , H.div { className: "col-md-12" }
            [ pageLayout { deletions, frontends, totalRecords, container, session, path } ]
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
  void $ launchAff_ $ putCategories session nodeId (CategoryQuery q)
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
    cpt {frontends, session, nodeId, listId, query, totalRecords, chart, container} _ = do
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
          [ R2.row
            [ chart
            , H.div { className: "col-md-12" }
              [ pageLayout { frontends, totalRecords, deletions, container, session, path }
              , H.button { style: buttonStyle, on: { click: performClick } }
                [ H.i { className: "glyphitem glyphicon glyphicon-trash"
                      , style: { marginRight : "9px" } } []
                , H.text "Delete document!" 
                ]
              ]
            ]
          ]
        ]

type PagePath = { nodeId :: Int
                , listId :: Int
                , query   :: SearchQuery
                , params  :: T.Params
                , session :: Session
                }

initialPagePath :: {session :: Session, nodeId :: Int, listId :: Int, query :: SearchQuery} -> PagePath
initialPagePath {session, nodeId, listId, query} = {session, nodeId, listId, query, params: T.initialParams}

loadPage :: PagePath -> Aff (Array DocumentsView)
loadPage {session, nodeId, listId, query, params: {limit, offset, orderBy, searchType}} = do
  let
    convOrderBy (T.ASC  (T.ColumnName "Date")) = DateAsc
    convOrderBy (T.DESC (T.ColumnName "Date")) = DateDesc
    convOrderBy (T.ASC  (T.ColumnName "Title")) = TitleAsc
    convOrderBy (T.DESC (T.ColumnName "Title")) = TitleDesc
    convOrderBy (T.ASC  (T.ColumnName "Source")) = SourceAsc
    convOrderBy (T.DESC (T.ColumnName "Source")) = SourceDesc
    convOrderBy _ = DateAsc -- TODO

    p = Search { listId, offset, limit, orderBy: convOrderBy <$> orderBy } (Just nodeId)

  --SearchResult {result} <- post session p $ SearchQuery {query: concat query, expected:searchType}
  SearchResult {result} <- post session p query
  -- $ SearchQuery {query: concat query, expected: SearchDoc}
  pure $ case result of
              SearchResultDoc     {docs}     -> docs2view docs
              SearchResultContact {contacts} -> contacts2view contacts
              errMessage                     -> err2view errMessage

docs2view :: Array Document -> Array DocumentsView
docs2view docs = map toView docs
  where
    toView :: Document -> DocumentsView
    toView ( Document { id
                     , created: date
                     , hyperdata:  HyperdataRowDocument { authors
                                                    , title
                                                    , source
                                                    , publication_year
                                                    , publication_month
                                                    , publication_day
                                                    }
                     , category
                     , score
                     }
            ) = DocumentsView { id
                              , date
                              , title: fromMaybe "Title" title
                              , source: fromMaybe "Source" source
                              , score
                              , authors: fromMaybe "Authors" authors
                              , category: decodeCategory category
                              , pairs: []
                              , delete: false
                              , publication_year : fromMaybe 2020 publication_year
                              , publication_month: fromMaybe    1 publication_month
                              , publication_day  : fromMaybe    1 publication_day
                              }
contacts2view contacts = map toView contacts
  where
    toView :: Contact -> DocumentsView
    toView (Contact { c_id
                     , c_created: date
                     , c_hyperdata: HyperdataRowContact { firstname
                                                   , lastname
                                                   , labs
                                                   }
                     , c_score
                     }
            ) = DocumentsView { id: c_id
                              , date
                              , title : firstname <> lastname
                              , source: labs
                              , score: c_score
                              , authors: labs
                              , category: decodeCategory 1
                              , pairs: []
                              , delete: false
                              , publication_year: 2020
                              , publication_month: 10
                              , publication_day: 1
                              }

err2view message =
  [DocumentsView { id: 1
                 , date: "2020-01-01"
                 , title : "SearchNoResult"
                 , source: "Source"
                 , score: 1
                 , authors: "Authors"
                 , category: decodeCategory 1
                 , pairs: []
                 , delete: false
                 , publication_year: 2020
                 , publication_month: 10
                 , publication_day: 1
                 }
   ]




type PageLayoutProps =
  ( frontends    :: Frontends
  , totalRecords :: Int
  , deletions    :: R.State Deletions
  , container    :: Record T.TableContainerProps -> R.Element
  , session      :: Session
  , path         :: R.State PagePath
  )

type PageProps = ( documents :: Array DocumentsView | PageLayoutProps )

-- | Loads and renders a page
pageLayout :: Record PageLayoutProps -> R.Element
pageLayout props = R.createElement pageLayoutCpt props []

pageLayoutCpt :: R.Component PageLayoutProps
pageLayoutCpt = R.hooksComponent "G.C.FacetsTable.PageLayout" cpt
  where
    cpt {frontends, totalRecords, deletions, container, session, path} _ = do
      useLoader (fst path) loadPage $ \documents ->
        page {frontends, totalRecords, deletions, container, session, path, documents}

page :: Record PageProps -> R.Element
page props = R.createElement pageCpt props []

pageCpt :: R.Component PageProps
pageCpt = R.hooksComponent "G.C.FacetsTable.Page" cpt
  where
    cpt {frontends, totalRecords, container, deletions, documents, session, path: path@({nodeId, listId, query} /\ setPath)} _ = do
      pure $ T.table { rows, container, colNames, totalRecords, params, wrapColElts }
      where
        setParams f = setPath $ \p@{params: ps} -> p {params = f ps}
        params = (fst path).params /\ setParams
        colNames = T.ColumnName <$> [ "", "Date", "Title", "Source", "Authors", "Delete" ]
        wrapColElts = const identity
        -- TODO: how to interprete other scores?
        gi Favorite = "glyphicon glyphicon-star-empty"
        gi _ = "glyphicon glyphicon-star"
        isChecked id = Set.member id (fst deletions).pending
        isDeleted (DocumentsView {id}) = Set.member id (fst deletions).deleted
        pairUrl (Pair {id,label})
          | id > 1 = H.a { href, target: "blank" } [ H.text label ]
            where href = url session $ NodePath (sessionId session) NodeContact (Just id)
          | otherwise = H.text label
        documentUrl id =
            url frontends $ Routes.CorpusDocument (sessionId session) nodeId listId id
        comma = H.span {} [ H.text ", " ]
        rows = L.fromFoldable $ row <$> filter (not <<< isDeleted) documents
        row dv@(DocumentsView {id, score, title, source, authors, pairs, delete, category}) =
          { row:
            T.makeRow [
              H.div {} [ H.a { className: gi category, on: {click: markClick} } [] ]
              -- TODO show date: Year-Month-Day only
              , maybeStricken delete [ H.text $ publicationDate dv ]
              , maybeStricken delete [ H.a {target: "_blank", href: documentUrl id} [ H.text title ] ]
              , maybeStricken delete [ H.text source ]
              , maybeStricken delete [ H.text authors ]
                -- , maybeStricken $ intercalate [comma] (pairUrl <$> pairs)
              , H.input { type: "checkbox", checked: isChecked id, on: { click: toggleClick } }
              ]
          , delete: true }
          where
            markClick   _ = markCategory session nodeId category [id]
            toggleClick _ = togglePendingDeletion deletions id
        maybeStricken delete
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

