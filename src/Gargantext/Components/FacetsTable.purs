-- TODO: this module should replace DocsTable
--      However the fix for favorites in commit 91cb6bd9906e128b3129b1db01ef6ef5ae13f7f8
--       has not been ported to this module yet.
module Gargantext.Components.FacetsTable where

------------------------------------------------------------------------
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Array (concat, filter)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), split)
import Data.String as String
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Prelude
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.Category (CategoryQuery(..), putCategories)
import Gargantext.Components.Category.Types (Category(..), decodeCategory, favCategory)
import Gargantext.Components.Search
import Gargantext.Components.Table as T
import Gargantext.Components.Table.Types as T
import Gargantext.Ends (url, Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(Search, NodeAPI))
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId, post, deleteWithBody)
import Gargantext.Types (NodeType(..), OrderBy(..), NodePath(..), NodeID)
import Gargantext.Utils (toggleSet, zeroPad)
import Gargantext.Utils.DecodeMaybe ((.|))
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.FacetsTable"
------------------------------------------------------------------------

type Props =
  ( chart        :: R.Element
  , container    :: Record T.TableContainerProps -> R.Element
  , frontends    :: Frontends
  , listId       :: Int
  , nodeId       :: Int
  , query        :: SearchQuery
  , session      :: Session
  , totalRecords :: Int
  )

-- | Tracks the ids of documents to delete and that have been deleted
type Deletions = { pending :: Set Int
                 , deleted :: Set Int
                 }

initialDeletions :: Deletions
initialDeletions = { pending: mempty, deleted: mempty }

----------------------------------------------------------------------
newtype Pair =
  Pair { id    :: Int
       , label :: String
       }

derive instance genericPair :: Generic Pair _

instance showPair :: Show Pair where
  show = genericShow

----------------------------------------------------------------------
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

derive instance genericDocumentsView :: Generic DocumentsView _

instance showDocumentsView :: Show DocumentsView where
  show = genericShow

----------------------------------------------------------------------
newtype ContactsView =
  ContactsView
  { id         :: Int
  , hyperdata  :: HyperdataRowContact
  , score      :: Int
  , annuaireId :: Int
  , delete     :: Boolean
  }

derive instance genericContactsView :: Generic ContactsView _

instance showContactsView :: Show ContactsView where
  show = genericShow

----------------------------------------------------------------------
data Rows = Docs     { docs     :: Seq DocumentsView }
          | Contacts { contacts :: Seq ContactsView  }

----------------------------------------------------------------------

-- | Main layout of the Documents Tab of a Corpus
docView :: Record Props -> R.Element
docView props = R.createElement docViewCpt props []

docViewCpt :: R.Component Props
docViewCpt = R.hooksComponentWithModule thisModule "docView" cpt
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

      pure $ H.div { className: "facets-doc-view container1" }
        [ R2.row
          [ chart
          , H.div { className: "col-md-12" }
            [ pageLayout { deletions, frontends, totalRecords, container, session, path } ]
   {-     , H.div { className: "col-md-12" }
            [ H.button { style: buttonStyle, on: { click: trashClick deletions } }
              [ H.i { className: "glyphitem fa fa-trash"
                    , style: { marginRight : "9px" }} []
            , H.text "Delete document!" ] 
            ] 
    -}      ] 
       ]
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
docViewGraphCpt = R.hooksComponentWithModule thisModule "docViewGraph" cpt
  where
    cpt {frontends, session, nodeId, listId, query, totalRecords, chart, container} _ = do
      deletions <- R.useState' initialDeletions
      let buttonStyle = { backgroundColor: "peru", padding : "9px"
                        , color : "white", border : "white", float: "right"}
      let performClick = \_ -> performDeletions session nodeId deletions
      path <- R.useState' $ initialPagePath { nodeId, listId, query, session }
      pure $ R.fragment
        [ H.br {}
        , H.p  {} [ H.text "" ]
        , H.br {}
        , H.div { className: "container-fluid" }
          [ R2.row
            [ chart
            , H.div { className: "col-md-12" }
              [ pageLayout { frontends, totalRecords, deletions, container, session, path }
              , H.button { style: buttonStyle, on: { click: performClick } }
                [ H.i { className: "glyphitem fa fa-trash"
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

loadPage :: PagePath -> Aff Rows
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
          SearchResultDoc     {docs}     -> Docs     {docs: doc2view     <$> Seq.fromFoldable docs}
          SearchResultContact {contacts} -> Contacts {contacts: contact2view <$> Seq.fromFoldable contacts}
          errMessage                     -> Docs     {docs: Seq.fromFoldable [err2view errMessage]} -- TODO better error view

doc2view :: Document -> DocumentsView
doc2view ( Document { id
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

contact2view :: Contact -> ContactsView
contact2view (Contact { c_id
                      , c_created: date
                      , c_hyperdata
                      , c_annuaireId
                      , c_score
                      }
        ) = ContactsView { id: c_id
                         , hyperdata: c_hyperdata
                         , score: c_score
                         , annuaireId : c_annuaireId
                         , delete: false
                         }

err2view message =
  DocumentsView { id: 1
                , date: ""
                , title : "SearchNoResult"
                , source: ""
                , score: 1
                , authors: ""
                , category: decodeCategory 1
                , pairs: []
                , delete: false
                , publication_year: 2020
                , publication_month: 10
                , publication_day: 1
                }

type PageLayoutProps =
  ( frontends    :: Frontends
  , totalRecords :: Int
  , deletions    :: R.State Deletions
  , container    :: Record T.TableContainerProps -> R.Element
  , session      :: Session
  , path         :: R.State PagePath
  )

type PageProps = ( rowsLoaded :: Rows | PageLayoutProps )

-- | Loads and renders a page
pageLayout :: Record PageLayoutProps -> R.Element
pageLayout props = R.createElement pageLayoutCpt props []

pageLayoutCpt :: R.Component PageLayoutProps
pageLayoutCpt = R.hooksComponentWithModule thisModule "pageLayout" cpt
  where
    cpt {frontends, totalRecords, deletions, container, session, path} _ = do
      useLoader (fst path) loadPage $ \rowsLoaded ->
        page {frontends, totalRecords, deletions, container, session, path, rowsLoaded}

page :: Record PageProps -> R.Element
page props = R.createElement pageCpt props []

pageCpt :: R.Component PageProps
pageCpt = R.hooksComponentWithModule thisModule "page" cpt
  where
    cpt {frontends, totalRecords, container, deletions, rowsLoaded, session, path: path@({nodeId, listId, query} /\ setPath)} _ = do
      pure $ T.table { syncResetButton : [ H.div {} [] ]
                     , rows, container, colNames
                     , totalRecords, params, wrapColElts
                     }
      where
        setParams f = setPath $ \p@{params: ps} -> p {params = f ps}
        params = (fst path).params /\ setParams
        -- colNames = T.ColumnName <$> [ "", "Date", "Title", "Source", "Authors", "Delete" ]
        colNames = case rowsLoaded of
            Docs     _ -> T.ColumnName <$> [ "", "Date", "Title", "Journal", "", "" ]
            Contacts _ -> T.ColumnName <$> [ "", "Contact", "Organization", "", "", "" ]

        wrapColElts = const identity
        -- TODO: how to interprete other scores?
        gi Trash = "fa fa-star-empty"
        gi _ = "fa fa-star"

        isChecked id = Set.member id (fst deletions).pending
        isDeleted (DocumentsView {id}) = Set.member id (fst deletions).deleted

        pairUrl (Pair {id,label})
          | id > 1 = H.a { href, target: "blank" } [ H.text label ]
            where href = url session $ NodePath (sessionId session) NodeContact (Just id)
          | otherwise = H.text label
        documentUrl id =
            url frontends $ Routes.CorpusDocument (sessionId session) nodeId listId id

        rows = case rowsLoaded of
          Docs     {docs}     -> docRow     <$> Seq.filter (not <<< isDeleted) docs
          Contacts {contacts} -> contactRow <$>  contacts

        contactRow (ContactsView { id, hyperdata: HyperdataRowContact { firstname, lastname, labs}
                                 , score, annuaireId, delete
                               }) =
          { row:
            T.makeRow [ H.div {} [ H.a { className: gi Favorite, on: {click: markClick} } [] ]
                      , maybeStricken delete [ H.a {target: "_blank", href: contactUrl annuaireId id}
                                                   [ H.text $ firstname <> " " <> lastname ]
                                             ]
                      , maybeStricken delete [ H.text labs ]
                      ]
          , delete: true
          }
          where
            markClick   _     = markCategory session nodeId Favorite [id]
            contactUrl aId id = url frontends $ Routes.ContactPage (sessionId session) annuaireId id

        docRow dv@(DocumentsView {id, score, title, source, authors, pairs, delete, category}) =
          { row:
            T.makeRow [ H.div {} [ H.a { className: gi category, on: {click: markClick} } [] ]
                      , maybeStricken delete [ H.text $ publicationDate dv ]
                      , maybeStricken delete [ H.a {target: "_blank", href: documentUrl id} [ H.text title ] ]
                      , maybeStricken delete [ H.text source ]
                      -- , maybeStricken delete [ H.text authors ]
                        -- , maybeStricken $ intercalate [comma] (pairUrl <$> pairs)
                      {-, H.input { defaultChecked: isChecked id
                                , on: { click: toggleClick }
                                , type: "checkbox"
                                }
                      -}
                      ]
          , delete: true }
          where
            markClick   _ = markCategory session nodeId category [id]
            toggleClick _ = togglePendingDeletion deletions id
            -- comma = H.span {} [ H.text ", " ]

        maybeStricken delete
          | delete    = H.div { style: { textDecoration: "line-through" } }
          | otherwise = H.div {}

publicationDate :: DocumentsView -> String
publicationDate (DocumentsView {publication_year, publication_month, publication_day}) =
  (zeroPad 2 publication_year) <> "-" <> (zeroPad 2 publication_month)
  -- <> "-" <> (zeroPad 2 publication_day)


---------------------------------------------------------

newtype DeleteDocumentQuery = DeleteDocumentQuery { documents :: Array Int }

instance encodeJsonDDQuery :: EncodeJson DeleteDocumentQuery where
  encodeJson (DeleteDocumentQuery {documents}) =
    "documents" := documents ~> jsonEmptyObject

deleteDocuments :: Session -> Int -> DeleteDocumentQuery -> Aff (Array Int)
deleteDocuments session nodeId =
  deleteWithBody session $ NodeAPI Node (Just nodeId) "documents"

