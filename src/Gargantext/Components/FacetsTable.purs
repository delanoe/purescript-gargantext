-- TODO: this module should replace DocsTable
--      However the fix for favorites in commit 91cb6bd9906e128b3129b1db01ef6ef5ae13f7f8
--       has not been ported to this module yet.
module Gargantext.Components.FacetsTable where

import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Prelude

import Gargantext.Components.Category (CategoryQuery(..), putCategories)
import Gargantext.Components.Category.Types (Category(..), decodeCategory, favCategory)
import Gargantext.Components.Search
  ( Contact(..), Document(..), HyperdataRowContact(..), HyperdataRowDocument(..)
  , SearchQuery, SearchResult(..), SearchResultTypes(..) )

import Gargantext.Components.Table as T
import Gargantext.Components.Table.Types as T
import Gargantext.Ends (url, Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(Search, NodeAPI))
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId, post, deleteWithBody)
import Gargantext.Types (NodeType(..), OrderBy(..), NodePath(..), NodeID)
import Gargantext.Utils (toggleSet, zeroPad)
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.FacetsTable"

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
instance eqPair :: Eq Pair where
  eq = genericEq
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
instance eqDocumentsView :: Eq DocumentsView where
  eq = genericEq
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
instance eqContactsView :: Eq ContactsView where
  eq = genericEq
instance showContactsView :: Show ContactsView where
  show = genericShow

----------------------------------------------------------------------
data Rows = Docs     { docs     :: Seq DocumentsView }
          | Contacts { contacts :: Seq ContactsView  }
derive instance genericRows :: Generic Rows _
instance eqRows :: Eq Rows where
  eq = genericEq

----------------------------------------------------------------------

-- | Main layout of the Documents Tab of a Corpus
docView :: Record Props -> R.Element
docView props = R.createElement docViewCpt props []

docViewCpt :: R.Component Props
docViewCpt = here.component "docView" cpt
  where
    cpt {frontends, session, nodeId, listId, query, totalRecords, chart, container} _ = do
      deletions <- T.useBox initialDeletions
      path <- T.useBox $ initialPagePath {nodeId, listId, query, session}
      path' <- T.useLive T.unequal path

      R.useEffect' $ do
        let ipp = initialPagePath {nodeId, listId, query, session}
        if path' == ipp then
          pure unit
        else
          void $ T.write ipp path

      pure $ H.div { className: "facets-doc-view container1" }
        [ R2.row
          [ chart
          , H.div { className: "col-md-12" }
            [ pageLayout { container, deletions, frontends, path, session, totalRecords } [] ]
    {-     , H.div { className: "col-md-12" }
            [ H.button { style: buttonStyle, on: { click: trashClick deletions } }
              [ H.i { className: "glyphitem fa fa-trash"
                    , style: { marginRight : "9px" }} []
            , H.text "Delete document!" ] 
            ] 
    -}      ] 
        ]
      where
        buttonStyle = { backgroundColor: "peru"
                      , border: "white"
                      , color: "white"
                      , float: "right"
                      , padding: "9px" }

performDeletions :: Session -> Int -> T.Box Deletions -> Deletions -> Effect Unit
performDeletions session nodeId deletions deletions' = do
  launchAff_ $ deleteDocuments session nodeId (DeleteDocumentQuery q)
  T.modify_ del deletions
  where
    q = { documents: Set.toUnfoldable deletions'.pending }
    del { deleted, pending } = { deleted: deleted <> pending, pending: mempty }

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
docViewGraphCpt = here.component "docViewGraph" cpt
  where
    cpt {frontends, session, nodeId, listId, query, totalRecords, chart, container} _ = do
      deletions <- T.useBox initialDeletions
      deletions' <- T.useLive T.unequal deletions
      let buttonStyle = { backgroundColor: "peru", padding : "9px"
                        , color : "white", border : "white", float: "right"}
      let performClick = \_ -> performDeletions session nodeId deletions deletions'
      path <- T.useBox $ initialPagePath { nodeId, listId, query, session }

      pure $ R.fragment
        [ H.br {}
        , H.p  {} [ H.text "" ]
        , H.br {}
        , H.div { className: "container-fluid" }
          [ R2.row
            [ chart
            , H.div { className: "col-md-12" }
              [ pageLayout { container, deletions, frontends, path, session, totalRecords } []
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
  , deletions    :: T.Box Deletions
  , container    :: Record T.TableContainerProps -> R.Element
  , session      :: Session
  , path         :: T.Box PagePath
  )

type PageProps = ( rowsLoaded :: Rows | PageLayoutProps )

-- | Loads and renders a page
pageLayout :: R2.Component PageLayoutProps
pageLayout = R.createElement pageLayoutCpt

pageLayoutCpt :: R.Component PageLayoutProps
pageLayoutCpt = here.component "pageLayout" cpt
  where
    cpt { container, deletions, frontends, path, session, totalRecords } _ = do
      path' <- T.useLive T.unequal path

      useLoader path' loadPage $ \rowsLoaded ->
        page { container, deletions, frontends, path, rowsLoaded, session, totalRecords } []

page :: R2.Component PageProps
page = R.createElement pageCpt

pageCpt :: R.Component PageProps
pageCpt = here.component "page" cpt
  where
    cpt { container
        , deletions
        , frontends
        , path
        , rowsLoaded
        , session
        , totalRecords } _ = do
      path'@{ nodeId, listId, query } <- T.useLive T.unequal path
      params <- T.useFocused (_.params) (\a b -> b { params = a }) path
      deletions' <- T.useLive T.unequal deletions

      let isChecked id = Set.member id deletions'.pending
          isDeleted (DocumentsView {id}) = Set.member id deletions'.deleted

          rows path' = case rowsLoaded of
            Docs     {docs}     -> docRow path'     <$> Seq.filter (not <<< isDeleted) docs
            Contacts {contacts} -> contactRow path' <$>  contacts

      pure $ T.table { colNames
                     , container
                     , params
                     , rows: rows path'
                     , syncResetButton : [ H.div {} [] ]
                     , totalRecords
                     , wrapColElts
                     }
      where
        colNames = case rowsLoaded of
            Docs     _ -> T.ColumnName <$> [ "", "Date", "Title", "Journal", "", "" ]
            Contacts _ -> T.ColumnName <$> [ "", "Contact", "Organization", "", "", "" ]

        wrapColElts = const identity
        -- TODO: how to interprete other scores?
        gi Trash = "fa fa-star-empty"
        gi _ = "fa fa-star"

        documentUrl id { listId, nodeId } =
            url frontends $ Routes.CorpusDocument (sessionId session) nodeId listId id

        pairUrl (Pair {id,label})
          | id > 1 = H.a { href, target: "blank" } [ H.text label ]
            where href = url session $ NodePath (sessionId session) NodeContact (Just id)
          | otherwise = H.text label

        contactRow path' (ContactsView { id, hyperdata: HyperdataRowContact { firstname, lastname, labs }
                                       , score, annuaireId, delete
                               }) =
          { row:
            T.makeRow [ H.div {} [ H.a { className: gi Favorite, on: {click: markClick path'} } [] ]
                      , maybeStricken delete [ H.a {target: "_blank", href: contactUrl annuaireId id}
                                                   [ H.text $ firstname <> " " <> lastname ]
                                             ]
                      , maybeStricken delete [ H.text labs ]
                      ]
          , delete: true
          }
          where
            markClick { nodeId }  _     = markCategory session nodeId Favorite [id]
            contactUrl aId id' = url frontends $ Routes.ContactPage (sessionId session) annuaireId id'

        docRow path' dv@(DocumentsView {id, score, title, source, authors, pairs, delete, category}) =
          { row:
            T.makeRow [ H.div {} [ H.a { className: gi category, on: {click: markClick path'} } [] ]
                      , maybeStricken delete [ H.text $ publicationDate dv ]
                      , maybeStricken delete [ H.a {target: "_blank", href: documentUrl id path'} [ H.text title ] ]
                      , maybeStricken delete [ H.text source ]
                      ]
          , delete: true }
          where
            markClick { nodeId } _ = markCategory session nodeId category [id]
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

