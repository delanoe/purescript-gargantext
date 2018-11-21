module Gargantext.Pages.Corpus.Tabs.Documents where

import Data.Array (take, drop)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.?), (:=), (~>))

import Affjax (defaultRequest, request)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (printResponseFormatError)
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Cont.Trans (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Array (drop, take, (:))
import Data.Either (Either(..))
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
import Gargantext.Config (NodeType(..), TabType(..), toUrl, End(..), OrderBy(..))
import Gargantext.Config.REST (get, put, post, deleteWithBody)
import Gargantext.Utils.DecodeMaybe ((.|))
import Gargantext.Components.Charts.Options.ECharts (chart)
import Gargantext.Components.Loader as Loader
import Gargantext.Components.Table as T
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Table as T
import Gargantext.Pages.Corpus.Dashboard (globalPublis)
import Gargantext.Pages.Corpus.Tabs.Types (CorpusInfo(..), Props)
import Gargantext.Utils.DecodeMaybe ((.|))
import React.DOM (a, br', button, div, i, input, p, text)
import React.DOM.Props (_type, className, href, name, onClick, placeholder, style, value)
import Thermite (PerformAction, Render, Spec, defaultPerformAction, modifyState_, simpleSpec)
------------------------------------------------------------------------
-- TODO: Pagination Details are not available from the BackEnd
-- TODO: Search is pending
-- TODO: Fav is pending
-- TODO: Sort is Pending
-- TODO: Filter is Pending
-- TODO: When a pagination link is clicked, reload data. 

type State =
  { documentIdsToDelete :: Set Int
  }

data Action
  = MarkFavorites (Array Int)
  | ToggleDocumentToDelete Int
  | Trash

newtype DocumentsView
  = DocumentsView
    { _id    :: Int
    , url    :: String
    , date   :: String
    , title  :: String
    , source :: String
    , fav    :: Boolean
    , ngramCount :: Int
    , delete :: Boolean
    }


derive instance genericCorpus :: Generic DocumentsView _

instance showCorpus :: Show DocumentsView where
  show = genericShow


newtype Response = Response
  { cid        :: Int
  , created    :: String
  , hyperdata  :: Hyperdata
  , favorite   :: Boolean
  , ngramCount :: Int
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


instance decodeHyperdata :: DecodeJson Hyperdata where
  decodeJson json = do
    obj    <- decodeJson json
    title  <- obj .| "title"
    source <- obj .| "source"
    pure $ Hyperdata { title,source }

instance decodeResponse :: DecodeJson Response where
  decodeJson json = do
    obj        <- decodeJson json
    cid        <- obj .? "id"
    created    <- pure "2018"
    --created    <- obj .? "date"
    favorite   <- pure true
    ngramCount <- obj .? "id"
    hyperdata  <- obj .? "hyperdata"
    pure $ Response { cid, created, favorite, ngramCount, hyperdata }



-- | Filter
filterSpec :: Spec State Props Action
filterSpec = simpleSpec defaultPerformAction render
  where
    render d p s c = [div [] [ text "    Filter "
                     , input []
                     ]]

-- | Main layout of the Documents Tab of a Corpus
layoutDocview :: Spec State Props Action
layoutDocview = simpleSpec performAction render
  where
    performAction :: PerformAction State Props Action
    performAction (MarkFavorites nids) {path : nodeId} _ =
      void $ lift $ putFavorites nodeId (FavoriteQuery {favorites: nids})
    --TODO add array of delete rows here
    performAction (ToggleDocumentToDelete nid) _ _ =
      modifyState_ \state -> state {documentIdsToDelete = toggleSet nid state.documentIdsToDelete}
    performAction Trash {path: nodeId} {documentIdsToDelete} =
      void $ lift $ deleteDocuments nodeId (DeleteDocumentQuery {documents: Set.toUnfoldable documentIdsToDelete})
      -- TODO: what to do now that the documents are deleted
      -- * should we reload?
      -- * should we locally update our data?
      -- * should we reset documentIdsToDelete?
      -- * if so, how to un-check the checkboxes since the inputs are uncontrolled?
      --   (maybe there is no need to uncheck them if they disapear because we
      --    either reload or local update our data)

    render :: Render State Props Action
    render dispatch {path: nodeId, loaded: corpusInfo} _ _ =
      [ p [] []
      , div [ style {textAlign : "center"}] [input [placeholder "Filter here"]]
      , br'
      , div [className "container1"]
        [ div [className "row"]
          [ chart globalPublis
          , div [className "col-md-12"]
            [ pageLoader
                { path: initialPageParams nodeId
                , corpusInfo
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

mock :: Boolean
mock = false

type PageParams = {nodeId :: Int, params :: T.Params}

initialPageParams :: Int -> PageParams
initialPageParams nodeId = {nodeId, params: T.initialParams}

loadPage :: PageParams -> Aff (Array DocumentsView)
loadPage {nodeId, params: {limit, offset, orderBy}} = do
  logs "loading documents page: loadPage with Offset and limit"
  --res <- get $ toUrl Back (Children Url_Document offset limit) nodeId
  res <- get $ toUrl Back (Tab TabDocs offset limit (convOrderBy <$> orderBy)) (Just nodeId)
  let docs = res2corpus <$> res
  _ <- logs "Ok: loading page documents"
  _ <- logs $ map show docs
  pure $
    if mock then take limit $ drop offset sampleData else
    docs
  where
    res2corpus :: Response -> DocumentsView
    res2corpus (Response r) =
      DocumentsView { _id : r.cid
      , url    : ""
      , date   :  r.created
      , title  : (\(Hyperdata hr) -> hr.title) r.hyperdata
      , source : (\(Hyperdata hr) -> hr.source) r.hyperdata
      , fav    : r.favorite
      , ngramCount : r.ngramCount
      , delete : false
     }
    convOrderBy (T.ASC  (T.ColumnName "Date")) = DateAsc
    convOrderBy (T.DESC (T.ColumnName "Date")) = DateDesc
    convOrderBy (T.ASC  (T.ColumnName "Title")) = TitleAsc
    convOrderBy (T.DESC (T.ColumnName "Title")) = TitleDesc

    convOrderBy _ = DateAsc -- TODO

type PageLoaderProps ext =
  { path :: PageParams
  , corpusInfo :: Maybe (NodePoly CorpusInfo)
  , dispatch :: Action -> Effect Unit
  | ext
  }

renderPage :: forall props path.
              Render (Loader.State {nodeId :: Int | path} (Array DocumentsView))
                     { corpusInfo :: Maybe (NodePoly CorpusInfo)
                     , dispatch :: Action -> Effect Unit
                     | props
                     }
                     (Loader.Action PageParams)
renderPage _ _ {loaded: Nothing} _ = [] -- TODO loading spinner
renderPage loaderDispatch {corpusInfo, dispatch} {currentPath: {nodeId}, loaded: Just res} _ =
  [ T.tableElt
      { rows
      , setParams: \params -> liftEffect $ loaderDispatch (Loader.SetPath {nodeId, params})
      , container: T.defaultContainer { title: "Documents" }
      , colNames:
          T.ColumnName <$>
          [ ""
          , "Date"
          , "Title"
          , "Source"
          , "Delete"
          ]
      , totalRecords: maybe 47361 -- TODO
                        identity
                        ((\(NodePoly n) -> n.hyperdata)
                         >>>
                         (\(CorpusInfo c) -> c.totalRecords)
                        <$> corpusInfo)
      }
  ]
  where
    fa true  = "fas "
    fa false = "far "
    rows = (\(DocumentsView r) ->
                { row:
                    [ div []
                      [ a [className $ fa r.fav <> "fa-star" ,onClick $ (\_->
                          dispatch $ MarkFavorites [r._id])] []
                      ]
                    -- TODO show date: Year-Month-Day only
                    , if (r.delete) then
                        div [ style {textDecoration : "line-through"}][text r.date]
                      else
                        div [ ][text r.date]
                    , if (r.delete) then
                        a [ href (toUrl Front Url_Document (Just r._id)), style {textDecoration : "line-through"} ] [ text r.title ]
                      else
                        a [ href (toUrl Front Url_Document (Just r._id)) ] [ text r.title ]
                    , if (r.delete) then
                        div [style {textDecoration : "line-through"}] [ text r.source]
                      else
                        div [] [ text r.source]
                    , input [ _type "checkbox", onClick $ (\_ -> dispatch $ ToggleDocumentToDelete r._id)]
                    ]
                , delete: true
                }) <$> res

pageLoaderClass :: ReactClass (PageLoaderProps (children :: Children))
pageLoaderClass = Loader.createLoaderClass' "PageLoader" loadPage renderPage

pageLoader :: PageLoaderProps () -> ReactElement
pageLoader props = React.createElement pageLoaderClass props []

---------------------------------------------------------
sampleData' :: DocumentsView
sampleData' = DocumentsView {_id : 1, url : "", date : "date3", title : "title", source : "source", fav : false, ngramCount : 1, delete : false}

sampleData :: Array DocumentsView
--sampleData = replicate 10 sampleData'
sampleData = map (\(Tuple t s) -> DocumentsView {_id : 1, url : "", date : "2017", title: t, source: s, fav : false, ngramCount : 10, delete : false}) sampleDocuments

sampleDocuments :: Array (Tuple String String)
sampleDocuments = [Tuple "Macroscopic dynamics of the fusion process" "Journal de Physique Lettres",Tuple "Effects of static and cyclic fatigue at high temperature upon reaction bonded silicon nitride" "Journal de Physique Colloques",Tuple "Reliability of metal/glass-ceramic junctions made by solid state bonding" "Journal de Physique Colloques",Tuple "High temperature mechanical properties and intergranular structure of sialons" "Journal de Physique Colloques",Tuple "SOLUTIONS OF THE LANDAU-VLASOV EQUATION IN NUCLEAR PHYSICS" "Journal de Physique Colloques",Tuple "A STUDY ON THE FUSION REACTION 139La + 12C AT 50 MeV/u WITH THE VUU EQUATION" "Journal de Physique Colloques",Tuple "Atomic structure of \"vitreous\" interfacial films in sialon" "Journal de Physique Colloques",Tuple "MICROSTRUCTURAL AND ANALYTICAL CHARACTERIZATION OF Al2O3/Al-Mg COMPOSITE INTERFACES" "Journal de Physique Colloques",Tuple "Development of oxidation resistant high temperature NbTiAl alloys and intermetallics" "Journal de Physique IV Colloque",Tuple "Determination of brazed joint constitutive law by inverse method" "Journal de Physique IV Colloque",Tuple "Two dimensional estimates from ocean SAR images" "Nonlinear Processes in Geophysics",Tuple "Comparison Between New Carbon Nanostructures Produced by Plasma with Industrial Carbon Black Grades" "Journal de Physique III",Tuple "<i>Letter to the Editor:</i> SCIPION, a new flexible ionospheric sounder in Senegal" "Annales Geophysicae",Tuple "Is reducibility in nuclear multifragmentation related to thermal scaling?" "Physics Letters B",Tuple "Independence of fragment charge distributions of the size of heavy multifragmenting sources" "Physics Letters B",Tuple "Hard photons and neutral pions as probes of hot and dense nuclear matter" "Nuclear Physics A",Tuple "Surveying the nuclear caloric curve" "Physics Letters B",Tuple "A hot expanding source in 50 A MeV Xe+Sn central reactions" "Physics Letters B"]

newtype SearchQuery = SearchQuery
  {
    query :: Array String
  , parent_id :: Int
  }


instance encodeJsonSQuery :: EncodeJson SearchQuery where
  encodeJson (SearchQuery post)
     = "query" := post.query
    ~> "parent_id" := post.parent_id
    ~> jsonEmptyObject



searchResults :: SearchQuery -> Aff Int
searchResults squery = post "http://localhost:8008/count" unit
  -- TODO



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
