module Gargantext.Components.FacetsTable where

import Affjax (defaultRequest, request)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (printResponseFormatError)
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Cont.Trans (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Array (drop, take, (:), filter)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import React as React
import React (ReactClass, ReactElement, Children)
------------------------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Config (End(..), NodeType(..), OrderBy(..), Path(..), TabType, toUrl)
import Gargantext.Config.REST (put, post, deleteWithBody)
import Gargantext.Components.Loader as Loader
import Gargantext.Components.Table as T
import Gargantext.Utils.DecodeMaybe ((.|))
import React.DOM (a, br', button, div, i, input, p, text, span)
import React.DOM.Props (_type, className, href, onClick, placeholder, style, checked, target)
import Thermite (PerformAction, Render, Spec, defaultPerformAction, modifyState_, simpleSpec, hideState)
------------------------------------------------------------------------
-- TODO: Search is pending
-- TODO: Fav is pending
-- TODO: Sort is Pending
-- TODO: Filter is Pending

type NodeID = Int
type TotalRecords = Int

newtype SearchQuery = SearchQuery
  { query :: Array String
  , id    :: Int
  }

instance encodeJsonSearchQuery :: EncodeJson SearchQuery where
  encodeJson (SearchQuery post)
     = "query"     := post.query
    ~> "corpus_id" := post.id
    ~> jsonEmptyObject

newtype SearchResults = SearchResults { results :: Array Response }

instance decodeSearchResults :: DecodeJson SearchResults where
  decodeJson json = do
    obj     <- decodeJson json
    results <- obj .? "results"
    pure $ SearchResults {results}

type Props =
  { nodeId :: Int
  , query :: Array String
  , totalRecords :: Int
  , chart :: ReactElement
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
  = MarkFavorites (Array Int)
  | ToggleDocumentToDelete Int
  | Trash

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
    }


derive instance genericDocumentsView :: Generic DocumentsView _

instance showDocumentsView :: Show DocumentsView where
  show = genericShow

newtype Response = Response
  { id        :: Int
  , date      :: String
  , hyperdata :: Hyperdata
  , score     :: Int
  , pairs     :: Array Pair
  }


newtype Hyperdata = Hyperdata
  { title  :: String
  , source :: String
  }

--instance decodeHyperdata :: DecodeJson Hyperdata where
--  decodeJson json = do
--    obj    <- decodeJson json
--    title  <- obj .? "title"
--    source <- obj .? "source"
--    pure $ Hyperdata { title,source }
--instance decodeResponse :: DecodeJson Response where
--  decodeJson json = do
--    obj        <- decodeJson json
--    cid        <- obj .? "id"
--    created    <- obj .? "created"
--    favorite   <- obj .? "favorite"
--    ngramCount <- obj .? "ngramCount"
--    hyperdata  <- obj .? "hyperdata"
--    pure $ Response { cid, created, favorite, ngramCount, hyperdata }


instance decodePair :: DecodeJson Pair where
  decodeJson json = do
    obj   <- decodeJson json
    id    <- obj .? "id"
    label <- obj .? "label"
    pure $ Pair { id, label }

instance decodeHyperdata :: DecodeJson Hyperdata where
  decodeJson json = do
    obj    <- decodeJson json
    title  <- obj .| "title"
    source <- obj .| "source"
    pure $ Hyperdata { title,source }

instance decodeResponse :: DecodeJson Response where
  decodeJson json = do
    obj       <- decodeJson json
    id        <- obj .? "id"
    -- date      <- obj .? "date" -- TODO
    date      <- pure "2018"
    score     <- obj .? "score"
    hyperdata <- obj .? "hyperdata"
    pairs     <- obj .? "pairs"
    pure $ Response { id, date, score, hyperdata, pairs }



-- | Filter
filterSpec :: forall state props action. Spec state props action
filterSpec = simpleSpec defaultPerformAction render
  where
    render d p s c = [div [ className "col-md-2", style {textAlign : "center", marginLeft : "0px", paddingLeft : "0px"}] [ text "    Filter "
                     , input [className "form-control", placeholder "Filter here"]
                     ]]

docViewSpec :: Spec {} Props Void
docViewSpec = hideState (const initialState) layoutDocview

-- | Main layout of the Documents Tab of a Corpus
layoutDocview :: Spec State Props Action
layoutDocview = simpleSpec performAction render
  where
    performAction :: PerformAction State Props Action
    performAction (MarkFavorites nids) {nodeId} _ =
      void $ lift $ putFavorites nodeId (FavoriteQuery {favorites: nids})
    --TODO add array of delete rows here
    performAction (ToggleDocumentToDelete nid) _ _ =
      modifyState_ \state -> state {documentIdsToDelete = toggleSet nid state.documentIdsToDelete}
    performAction Trash {nodeId} {documentIdsToDelete} = do
      void $ lift $ deleteDocuments nodeId (DeleteDocumentQuery {documents: Set.toUnfoldable documentIdsToDelete})
      modifyState_ \{documentIdsToDelete, documentIdsDeleted} ->
        { documentIdsToDelete: mempty
        , documentIdsDeleted: documentIdsDeleted <> documentIdsToDelete
        }

    render :: Render State Props Action
    render dispatch {nodeId, query, totalRecords, chart} deletionState _ =
      [ br'
      , div [ style {textAlign : "center"}] [ text "    Filter "
                     , input [className "form-control", style {width : "120px", display : "inline-block"}, placeholder "Filter here"]
                     ]
      , p [] [text ""]
      , br'
      , div [className "container1"]
        [ div [className "row"]
          [ chart
          , div [className "col-md-12"]
            [ pageLoader
                { path: initialPageParams {nodeId, query}
                , totalRecords
                , deletionState
                , dispatch
                }
            ]
          , div [className "col-md-12"]
             [ button [ style {backgroundColor: "peru", padding : "9px", color : "white", border : "white", float: "right"}
                      , onClick $ (\_ -> dispatch Trash)
                      ]
               [  i [className "glyphitem glyphicon glyphicon-trash", style {marginRight : "9px"}] []
               ,  text "Trash it !"
               ]
             ]
          ]
        ]
      ]

type PageParams = {nodeId :: Int, query :: Array String, params :: T.Params}

initialPageParams :: {nodeId :: Int, query :: Array String} -> PageParams
initialPageParams {nodeId, query} = {nodeId, query, params: T.initialParams}

loadPage :: PageParams -> Aff (Array DocumentsView)
loadPage {nodeId, query, params: {limit, offset, orderBy}} = do
  logs "loading documents page: loadPage with Offset and limit"
  let url = toUrl Back (Search { offset, limit, orderBy: convOrderBy <$> orderBy }) Nothing
  SearchResults res <- post url $ SearchQuery {id: nodeId, query}
  pure $ res2corpus <$> res.results
  where
    res2corpus :: Response -> DocumentsView
    res2corpus (Response { id, date, score, pairs
                         , hyperdata: Hyperdata {title, source}
                         }) =
      DocumentsView
        { id
        , date
        , title
        , source
        , score
        , pairs
        , delete : false
        }
    convOrderBy (T.ASC  (T.ColumnName "Date")) = DateAsc
    convOrderBy (T.DESC (T.ColumnName "Date")) = DateDesc
    convOrderBy (T.ASC  (T.ColumnName "Title")) = TitleAsc
    convOrderBy (T.DESC (T.ColumnName "Title")) = TitleDesc

    convOrderBy _ = DateAsc -- TODO

type PageLoaderProps row =
  { path :: PageParams
  , totalRecords :: Int
  , dispatch :: Action -> Effect Unit
  , deletionState :: State
  | row
  }

renderPage :: forall props path.
              Render (Loader.State {nodeId :: Int, query :: Array String | path} (Array DocumentsView))
                     { totalRecords :: Int
                     , dispatch :: Action -> Effect Unit
                     , deletionState :: State
                     | props
                     }
                     (Loader.Action PageParams)
renderPage _ _ {loaded: Nothing} _ = [] -- TODO loading spinner
renderPage loaderDispatch { totalRecords, dispatch
                          , deletionState: {documentIdsToDelete, documentIdsDeleted}}
                          {currentPath: {nodeId, query}, loaded: Just res} _ =
  [ T.tableElt
      { rows
      , setParams: \params -> liftEffect $ loaderDispatch (Loader.SetPath {nodeId, query, params})
      , container: T.defaultContainer { title: "Documents" }
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
    fa 0 = "far "
    fa _ = "fas "
    isChecked id = Set.member id documentIdsToDelete
    isDeleted (DocumentsView {id}) = Set.member id documentIdsDeleted
    pairUrl (Pair {id,label})
      | id > 1    = [a [href (toUrl Front NodeContact (Just id)), target "blank"] [text label]]
      | otherwise = [text label]
    comma = span [] [text ", "]
    rows = (\(DocumentsView {id,score,title,source,date,pairs,delete}) ->
                let
                  strikeIfDeleted
                    | delete    = [style {textDecoration : "line-through"}]
                    | otherwise = []
                in
                { row:
                    [ div []
                      [ a [ className $ fa score <> "fa-star"
                          , onClick $ const $ dispatch $ MarkFavorites [id]
                          ] []
                      ]
                    -- TODO show date: Year-Month-Day only
                    , div strikeIfDeleted [text date]
                    , a (strikeIfDeleted <> [ href (toUrl Front Url_Document (Just id))
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

newtype FavoriteQuery = FavoriteQuery
                        { favorites :: Array Int
                        }

instance encodeJsonFQuery :: EncodeJson FavoriteQuery where
  encodeJson (FavoriteQuery post)
     = "favorites" := post.favorites
       ~> jsonEmptyObject

newtype DeleteDocumentQuery = DeleteDocumentQuery
  {
    documents :: Array Int
  }


instance encodeJsonDDQuery :: EncodeJson DeleteDocumentQuery where
  encodeJson (DeleteDocumentQuery post)
     = "documents" := post.documents
       ~> jsonEmptyObject

putFavorites :: Int -> FavoriteQuery -> Aff (Array Int)
putFavorites nodeId = put (toUrl Back Node (Just nodeId) <> "/favorites")

deleteFavorites :: Int -> FavoriteQuery -> Aff (Array Int)
deleteFavorites nodeId = deleteWithBody (toUrl Back Node (Just nodeId) <> "/favorites")

deleteDocuments :: Int -> DeleteDocumentQuery -> Aff (Array Int)
deleteDocuments nodeId = deleteWithBody (toUrl Back Node (Just nodeId) <> "/documents")

-- TODO: not optimal but Data.Set lacks some function (Set.alter)
toggleSet :: forall a. Ord a => a -> Set a -> Set a
toggleSet a s
  | Set.member a s = Set.delete a s
  | otherwise      = Set.insert a s
