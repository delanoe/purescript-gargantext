-- TODO: this module should replace DocsTable
--       However the fix for favorites in commit 91cb6bd9906e128b3129b1db01ef6ef5ae13f7f8
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
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import React as React
import React (ReactClass, ReactElement, Children)
------------------------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Config (End(..), NodeType(..), OrderBy(..), Path(..), TabType, toUrl, endConfigStateful)
import Gargantext.Config.REST (put, post, deleteWithBody)
import Gargantext.Components.Loader as Loader
import Gargantext.Components.Search.Types (Category(..), CategoryQuery(..), favCategory, decodeCategory, putCategories)
import Gargantext.Components.Table as T
import Gargantext.Router as Router
import Gargantext.Utils (toggleSet)
import Gargantext.Utils.DecodeMaybe ((.|))
import React.DOM (a, br', button, div, i, input, p, text, span)
import React.DOM.Props (_type, className, href, onClick, style, checked, target)
import Thermite (PerformAction, Render, Spec, defaultPerformAction, modifyState_, simpleSpec, hideState)
------------------------------------------------------------------------

type NodeID = Int
type TotalRecords = Int

-- Example:
--   [["machine","learning"],["artificial","intelligence"]]
-- This searches for documents with "machine learning" or "artificial intelligence"
type TextQuery = Array (Array String)

newtype SearchQuery = SearchQuery
  { query :: TextQuery
  }

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
  { nodeId :: Int
  , listId :: Int
  , query :: TextQuery
  , totalRecords :: Int
  , chart :: ReactElement
  , container :: T.TableContainerProps -> Array ReactElement
  }

type State =
  { documentIdsToDelete :: Set Int
  , documentIdsDeleted  :: Set Int
  }

initialState :: State
initialState =
  { documentIdsToDelete: mempty
  , documentIdsDeleted:  mempty
  }

data Action
  = MarkCategory Category (Array Int)
  | ToggleDocumentToDelete Int
  | TrashDocuments

newtype Pair = Pair
  { id    :: Int
  , label :: String
  }

derive instance genericPair :: Generic Pair _

instance showPair :: Show Pair where
  show = genericShow

newtype DocumentsView
  = DocumentsView
    { id     :: Int
    , date   :: String
    , title  :: String
    , source :: String
    , score  :: Int
    , pairs  :: Array Pair
    , delete :: Boolean
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

-- | Filter
-- TODO: unused
filterSpec :: forall state props action. Spec state props action
filterSpec = simpleSpec defaultPerformAction render
  where
    render d p s c = [] {-[div [ className "col-md-2", style {textAlign : "center", marginLeft : "0px", paddingLeft : "0px"}] [ text "    Filter "
                     , input [className "form-control", placeholder "Filter here"]
                     ]]
                     -}

docViewSpec :: Spec {} Props Void
docViewSpec = hideState (const initialState) layoutDocviewGraph

-- | Main layout of the Documents Tab of a Corpus
layoutDocview :: Spec State Props Action
layoutDocview = simpleSpec performAction render
  where
    performAction :: PerformAction State Props Action
    performAction (MarkCategory category nids) {nodeId} _ =
      void $ lift $ putCategories nodeId $ CategoryQuery {nodeIds: nids, category: favCategory category}
    --TODO add array of delete rows here
    performAction (ToggleDocumentToDelete nid) _ _ =
      modifyState_ \state -> state {documentIdsToDelete = toggleSet nid state.documentIdsToDelete}
    performAction TrashDocuments {nodeId} {documentIdsToDelete} = do
      void $ lift $ deleteDocuments nodeId (DeleteDocumentQuery {documents: Set.toUnfoldable documentIdsToDelete})
      modifyState_ \{documentIdsToDelete, documentIdsDeleted} ->
        { documentIdsToDelete: mempty
        , documentIdsDeleted: documentIdsDeleted <> documentIdsToDelete
        }

    render :: Render State Props Action
    render dispatch {nodeId, listId, query, totalRecords, chart, container} deletionState _ =
      [ {- br'
      , div [ style {textAlign : "center"}] [ text "    Filter "
                     , input [className "form-control", style {width : "120px", display : "inline-block"}, placeholder "Filter here"]
                     ]
      , p [] [text ""]
      , br' 
      -}
       div [className "container1"]
        [ div [className "row"]
          [ chart
          , div [className "col-md-12"]
            [ pageLoader
                { path: initialPageParams {nodeId, listId, query}
                , totalRecords
                , deletionState
                , dispatch
                , container
                }
            ]
          , div [className "col-md-12"]
             [ button [ style {backgroundColor: "peru", padding : "9px", color : "white", border : "white", float: "right"}
                      , onClick $ (\_ -> dispatch TrashDocuments)
                      ]
               [  i [className "glyphitem glyphicon glyphicon-trash", style {marginRight : "9px"}] []
               ,  text "Trash it !"
               ]
             ]
          ]
        ]
      ]



layoutDocviewGraph :: Spec State Props Action
layoutDocviewGraph = simpleSpec performAction render
  where
    performAction :: PerformAction State Props Action
    performAction (MarkCategory category nids) {nodeId} _ =
      void $ lift $ putCategories nodeId $ CategoryQuery {nodeIds: nids, category: favCategory category}
    --TODO add array of delete rows here
    performAction (ToggleDocumentToDelete nid) _ _ =
      modifyState_ \state -> state {documentIdsToDelete = toggleSet nid state.documentIdsToDelete}
    performAction TrashDocuments {nodeId} {documentIdsToDelete} = do
      void $ lift $ deleteDocuments nodeId (DeleteDocumentQuery {documents: Set.toUnfoldable documentIdsToDelete})
      modifyState_ \{documentIdsToDelete, documentIdsDeleted} ->
        { documentIdsToDelete: mempty
        , documentIdsDeleted: documentIdsDeleted <> documentIdsToDelete
        }

    render :: Render State Props Action
    render dispatch {nodeId, listId, query, totalRecords, chart, container} deletionState _ =
      [ br'

      , p [] [text ""]
      , br'
      , div [className "container-fluid"]
        [ div [className "row"]
          [ chart
          , div [className "col-md-12"]
            [ pageLoader
                { path: initialPageParams {nodeId, listId, query}
                , totalRecords
                , deletionState
                , dispatch
                , container
                }
            , button [ style {backgroundColor: "peru", padding : "9px", color : "white", border : "white", float: "right"}
                      , onClick $ (\_ -> dispatch TrashDocuments)
                      ]
               [  i [className "glyphitem glyphicon glyphicon-trash", style {marginRight : "9px"}] []
               ,  text "Trash it !"
               ]
            ]

          ]
        ]
      ]



type PageParams = {nodeId :: Int, listId :: Int, query :: TextQuery, params :: T.Params}

initialPageParams :: {nodeId :: Int, listId :: Int, query :: TextQuery} -> PageParams
initialPageParams {nodeId, listId, query} = {nodeId, listId, query, params: T.initialParams}

loadPage :: PageParams -> Aff (Array DocumentsView)
loadPage {nodeId, listId, query, params: {limit, offset, orderBy}} = do
  logs "loading documents page: loadPage with Offset and limit"
  let url = toUrl endConfigStateful Back (Search { listId, offset, limit, orderBy: convOrderBy <$> orderBy }) (Just nodeId)
  SearchResults res <- post url $ SearchQuery {query}
  pure $ res2corpus <$> res.results
  where
    res2corpus :: Response -> DocumentsView
    res2corpus (Response { id, created: date, ngramCount: score
                         , hyperdata: Hyperdata {title, source}
                         , category
                         }) =
      DocumentsView
        { id
        , date
        , title
        , source
        , score
        , pairs: []
        , delete: false
        , category
        }
    convOrderBy (T.ASC  (T.ColumnName "Date")) = DateAsc
    convOrderBy (T.DESC (T.ColumnName "Date")) = DateDesc
    convOrderBy (T.ASC  (T.ColumnName "Title")) = TitleAsc
    convOrderBy (T.DESC (T.ColumnName "Title")) = TitleDesc
    convOrderBy (T.ASC  (T.ColumnName "Source")) = SourceAsc
    convOrderBy (T.DESC (T.ColumnName "Source")) = SourceDesc

    convOrderBy _ = DateAsc -- TODO

type PageLoaderProps row =
  { path :: PageParams
  , totalRecords :: Int
  , dispatch :: Action -> Effect Unit
  , deletionState :: State
  , container :: T.TableContainerProps -> Array ReactElement
  | row
  }

renderPage :: forall props path.
              Render (Loader.State {nodeId :: Int, listId :: Int, query :: TextQuery | path} (Array DocumentsView))
                     { totalRecords :: Int
                     , dispatch :: Action -> Effect Unit
                     , deletionState :: State
                     , container :: T.TableContainerProps -> Array ReactElement
                     | props
                     }
                     (Loader.Action PageParams)
renderPage _ _ {loaded: Nothing} _ = [] -- TODO loading spinner
renderPage loaderDispatch { totalRecords, dispatch, container
                          , deletionState: {documentIdsToDelete, documentIdsDeleted}}
                          {currentPath: {nodeId, listId, query}, loaded: Just res} _ =
  [ T.tableElt
      { rows
      , setParams: \params -> liftEffect $ loaderDispatch (Loader.SetPath {nodeId, listId, query, params})
      , container
      , colNames:
          T.ColumnName <$>
          [ ""
          , "Date"
          , "Title"
          , "Source"
          , "Authors"
          , "Delete"
          ]
      , totalRecords
      }
  ]
  where
    -- TODO: how to interprete other scores?
    gi Favorite = "glyphicon glyphicon-star-empty"
    gi _ = "glyphicon glyphicon-star"
    isChecked id = Set.member id documentIdsToDelete
    isDeleted (DocumentsView {id}) = Set.member id documentIdsDeleted
    pairUrl (Pair {id,label})
      | id > 1    = [a [href (toUrl endConfigStateful Front NodeContact (Just id)), target "blank"] [text label]]
      | otherwise = [text label]
    comma = span [] [text ", "]
    rows = (\(DocumentsView {id,score,title,source,date,pairs,delete,category}) ->
                let
                  strikeIfDeleted
                    | delete    = [style {textDecoration : "line-through"}]
                    | otherwise = []
                in
                { row:
                    [ div []
                      [ a [ className $ gi category
                          , onClick $ const $ dispatch $ MarkCategory category [id]
                          ] []
                      ]
                    -- TODO show date: Year-Month-Day only
                    , div strikeIfDeleted [text date]
                    , a (strikeIfDeleted <> [ href $ toLink endConfigStateful $ Router.Document listId id
                                            , target "blank"])
                        [ text title ]
                    , div strikeIfDeleted [text source]
                    , div strikeIfDeleted $ intercalate [comma] $ pairUrl <$> pairs
                    , input [ _type "checkbox"
                            , checked (isChecked id)
                            , onClick $ const $ dispatch $ ToggleDocumentToDelete id]
                    ]
                , delete: true
                }) <$> filter (not <<< isDeleted) res

pageLoaderClass :: ReactClass (PageLoaderProps (children :: Children))
pageLoaderClass = Loader.createLoaderClass' "PageLoader" loadPage renderPage

pageLoader :: PageLoaderProps () -> ReactElement
pageLoader props = React.createElement pageLoaderClass props []

---------------------------------------------------------

newtype DeleteDocumentQuery = DeleteDocumentQuery
  {
    documents :: Array Int
  }


instance encodeJsonDDQuery :: EncodeJson DeleteDocumentQuery where
  encodeJson (DeleteDocumentQuery post)
     = "documents" := post.documents
       ~> jsonEmptyObject

putFavorites :: Int -> FavoriteQuery -> Aff (Array Int)
putFavorites nodeId = put (toUrl endConfigStateful Back Node (Just nodeId) <> "/favorites")

deleteFavorites :: Int -> FavoriteQuery -> Aff (Array Int)
deleteFavorites nodeId = deleteWithBody (toUrl endConfigStateful Back Node (Just nodeId) <> "/favorites")

deleteDocuments :: Int -> DeleteDocumentQuery -> Aff (Array Int)
deleteDocuments nodeId = deleteWithBody (toUrl endConfigStateful Back Node (Just nodeId) <> "/documents")
