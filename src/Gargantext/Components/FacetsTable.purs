-- TODO: this module should replace DocsTable
--      However the fix for favorites in commit 91cb6bd9906e128b3129b1db01ef6ef5ae13f7f8
--       has not been ported to this module yet.
module Gargantext.Components.FacetsTable where

import Gargantext.Prelude

import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.Category (CategoryQuery(..), putCategories)
import Gargantext.Components.Category.Types (Category(..), decodeCategory, favCategory)
import Gargantext.Components.DocsTable.Types (showSource)
import Gargantext.Components.Search (Contact(..), Document(..), HyperdataRowContact(..), HyperdataRowDocument(..), SearchQuery, SearchResult(..), SearchResultTypes(..))
import Gargantext.Components.Table as T
import Gargantext.Components.Table.Types as T
import Gargantext.Config.REST (RESTError(..), AffRESTError)
import Gargantext.Ends (url, Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(Search, NodeAPI))
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId, post, deleteWithBody)
import Gargantext.Types (NodeType(..), OrderBy(..), NodeID)
import Gargantext.Utils (toggleSet, zeroPad)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON
import Toestand as T

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

derive instance Generic Pair _
instance Eq Pair where eq = genericEq
instance Show Pair where show = genericShow

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
  , publication_year :: Maybe Int
  , publication_month :: Maybe Int
  , publication_day  :: Maybe Int
  }

derive instance Generic DocumentsView _
instance Eq DocumentsView where eq = genericEq
instance Show DocumentsView where show = genericShow

----------------------------------------------------------------------
newtype ContactsView =
  ContactsView
  { id         :: Int
  , hyperdata  :: HyperdataRowContact
  , score      :: Int
  , annuaireId :: Int
  , delete     :: Boolean
  }
derive instance Generic ContactsView _
instance Eq ContactsView where eq = genericEq
instance Show ContactsView where show = genericShow

----------------------------------------------------------------------
data Rows = Docs     { docs     :: Seq DocumentsView }
          | Contacts { contacts :: Seq ContactsView  }
derive instance Generic Rows _
instance Eq Rows where eq = genericEq

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

      pure $ H.div { className: "facets-doc-view" }
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

loadPage :: PagePath -> AffRESTError Rows
loadPage { session, nodeId, listId, query, params: {limit, offset, orderBy } } = do
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
  eSearchResult <- post session p query
  case eSearchResult of
    Left err -> pure $ Left err
    Right (SearchResult {result}) -> do
      liftEffect $ here.log2 "[loadPage] result" result
      -- $ SearchQuery {query: concat query, expected: SearchDoc}
      pure $ Right $ case result of
              SearchResultDoc     {docs}     -> Docs     {docs: doc2view     <$> Seq.fromFoldable docs}
              SearchResultContact {contacts} -> Contacts {contacts: contact2view <$> Seq.fromFoldable contacts}
              errMessage                     -> Docs     {docs: Seq.fromFoldable [err2view errMessage]} -- TODO better error view

doc2view :: Document -> DocumentsView
doc2view ( Document { id
                    , created: date
                    , hyperdata:  HyperdataRowDocument { authors
                                                       , source
                                                       , publication_year
                                                       , publication_month
                                                       , publication_day
                                                       }
                    , category
                    , score
                    , title
                    }
        ) = DocumentsView { id
                          , date
                          , title
                          , source: showSource source
                          , score
                          , authors: fromMaybe "Authors" authors
                          , category: decodeCategory category
                          , pairs: []
                          , delete: false
                          , publication_year
                          , publication_month
                          , publication_day
                          }

contact2view :: Contact -> ContactsView
contact2view (Contact { c_id
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

err2view :: forall a. a -> DocumentsView
err2view _message =
  DocumentsView { id: 1
                , date: ""
                , title : "SearchNoResult"
                , source: ""
                , score: 1
                , authors: ""
                , category: decodeCategory 1
                , pairs: []
                , delete: false
                , publication_year: Just 2020
                , publication_month: Just 10
                , publication_day: Just 1
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

      useLoader { errorHandler
                , loader: loadPage
                , path: path'
                , render: \rowsLoaded -> page { container, deletions, frontends, path, rowsLoaded, session, totalRecords } [] }
    errorHandler err = do
      here.log2 "[pageLayout] RESTError" err
      case err of
        ReadJSONError err' -> here.log2 "[pageLayout] ReadJSONError" $ show err'
        _ -> pure unit

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
      path' <- T.useLive T.unequal path
      params <- T.useFocused (_.params) (\a b -> b { params = a }) path
      deletions' <- T.useLive T.unequal deletions

      let isDeleted (DocumentsView {id}) = Set.member id deletions'.deleted

          rows = case rowsLoaded of
            Docs     {docs}     -> docRow path'     <$> Seq.filter (not <<< isDeleted) docs
            Contacts {contacts} -> contactRow path' <$>  contacts

      pure $ T.table { colNames
                     , container
                     , params
                     , rows
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

        contactRow path' (ContactsView { id, hyperdata: HyperdataRowContact { firstname, lastname, labs }
                                       , annuaireId, delete
                               }) =
          { row:
            T.makeRow [ H.div {} [ H.a { className: gi Favorite, on: {click: markClick path'} } [] ]
                      , maybeStricken delete [ H.a { target: "_blank", href: contactUrl id }
                                                   [ H.text $ firstname <> " " <> lastname ]
                                             ]
                      , maybeStricken delete [ H.text labs ]
                      ]
          , delete: true
          }
          where
            markClick { nodeId }  _     = markCategory session nodeId Favorite [id]
            contactUrl id' = url frontends $ Routes.ContactPage (sessionId session) annuaireId id'

        docRow path' dv@(DocumentsView {id, title, source, delete, category}) =
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
publicationDate (DocumentsView { publication_year: Just publication_year, publication_month: Just publication_month }) =
  (zeroPad 2 publication_year) <> "-" <> (zeroPad 2 publication_month)
  -- <> "-" <> (zeroPad 2 publication_day)
publicationDate _ = "-"


---------------------------------------------------------

newtype DeleteDocumentQuery = DeleteDocumentQuery { documents :: Array Int }
derive instance Generic DeleteDocumentQuery _
derive instance Newtype DeleteDocumentQuery _
derive newtype instance JSON.WriteForeign DeleteDocumentQuery

deleteDocuments :: Session -> Int -> DeleteDocumentQuery -> AffRESTError (Array Int)
deleteDocuments session nodeId =
  deleteWithBody session $ NodeAPI Node (Just nodeId) "documents"
