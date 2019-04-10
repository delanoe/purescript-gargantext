-- TODO: this module should be replaced by FacetsTable
module Gargantext.Components.DocsTable where

import Affjax (defaultRequest, request)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (printResponseFormatError)
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Cont.Trans (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Array (drop, take, (:), filter)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..))
import Data.Lens
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import React as React
import React (ReactClass, ReactElement, Children)
------------------------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Config (End(..), NodeType(..), OrderBy(..), Path(..), TabType, toUrl)
import Gargantext.Config.REST (get, put, post, deleteWithBody)
import Gargantext.Components.Loader as Loader
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Table as T
import Gargantext.Utils.DecodeMaybe ((.|))
import React.DOM (a, br', button, div, i, input, p, text)
import React.DOM.Props (_type, className, href, onClick, placeholder, style, checked, target)
import Thermite (PerformAction, Render, Spec, defaultPerformAction, modifyState_, simpleSpec, hideState)
------------------------------------------------------------------------
-- TODO: Search is pending
-- TODO: Fav is pending
-- TODO: Sort is Pending
-- TODO: Filter is Pending

type NodeID = Int
type TotalRecords = Int

type Props =
  { nodeId       :: Int
  , totalRecords :: Int
  , chart        :: ReactElement
  , tabType      :: TabType
  -- ^ tabType is not ideal here since it is too much entangled with tabs and
  -- ngramtable. Let's see how this evolves.
  }

type State =
  { documentIdsToDelete :: Set Int
  , documentIdsDeleted  :: Set Int
  , localFavorites      :: Map Int Boolean
  }

initialState :: State
initialState =
  { documentIdsToDelete: mempty
  , documentIdsDeleted:  mempty
  , localFavorites:      mempty
  }

_documentIdsToDelete = prop (SProxy :: SProxy "documentIdsToDelete")
_documentIdsDeleted  = prop (SProxy :: SProxy "documentIdsDeleted")
_localFavorites      = prop (SProxy :: SProxy "localFavorites")

data Action
  = MarkFavorites Int Boolean
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


derive instance genericDocumentsView :: Generic DocumentsView _

instance showDocumentsView :: Show DocumentsView where
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
    created    <- pure "2019"
    --created    <- obj .? "date"
    favorite   <- obj .? "favorite"
    ngramCount <- obj .? "id"
    hyperdata  <- obj .? "hyperdata"
    pure $ Response { cid, created, favorite, ngramCount, hyperdata }



-- | Filter
filterSpec :: forall state props action. Spec state props action
filterSpec = simpleSpec defaultPerformAction render
  where
    render d p s c = [] {-[div [ className "col-md-2", style {textAlign : "center", marginLeft : "0px", paddingLeft : "0px"}] [ text "    Filter "
                     , input [className "form-control", placeholder "Filter here"]
                     ]] -}

docViewSpec :: Spec {} Props Void
docViewSpec = hideState (const initialState) layoutDocview

-- | Main layout of the Documents Tab of a Corpus
layoutDocview :: Spec State Props Action
layoutDocview = simpleSpec performAction render
  where
    performAction :: PerformAction State Props Action
    performAction (MarkFavorites nid fav) {nodeId} _ = do
      modifyState_ $ _localFavorites <<< at nid ?~ fav
      void $ lift $ if fav
        then putFavorites    nodeId (FavoriteQuery {favorites: [nid]})
        else deleteFavorites nodeId (FavoriteQuery {favorites: [nid]})
    --TODO add array of delete rows here
    performAction (ToggleDocumentToDelete nid) _ _ =
      modifyState_ \state -> state {documentIdsToDelete = toggleSet nid state.documentIdsToDelete}
    performAction Trash {nodeId} {documentIdsToDelete} = do
      void $ lift $ deleteDocuments nodeId (DeleteDocumentQuery {documents: Set.toUnfoldable documentIdsToDelete})
      modifyState_ $
        (_documentIdsToDelete .~ mempty) >>>
        (_documentIdsDeleted <>~ documentIdsToDelete)

    render :: Render State Props Action
    render dispatch {nodeId, tabType, totalRecords, chart} deletionState _ =
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
                { path: initialPageParams {nodeId, tabType}
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

mock :: Boolean
mock = false

type PageParams = {nodeId :: Int, tabType :: TabType, params :: T.Params}

initialPageParams :: {nodeId :: Int, tabType :: TabType} -> PageParams
initialPageParams {nodeId, tabType} = {nodeId, tabType, params: T.initialParams}

loadPage :: PageParams -> Aff (Array DocumentsView)
loadPage {nodeId, tabType, params: {limit, offset, orderBy}} = do
  logs "loading documents page: loadPage with Offset and limit"
  res <- get $ toUrl Back (Tab tabType offset limit (convOrderBy <$> orderBy)) (Just nodeId)
  let docs = res2corpus <$> res
  --_ <- logs "Ok: loading page documents"
  --_ <- logs $ map show docs
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
    convOrderBy (T.ASC  (T.ColumnName "Date"))  = DateAsc
    convOrderBy (T.DESC (T.ColumnName "Date"))  = DateDesc
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
              Render (Loader.State {nodeId :: Int, tabType :: TabType | path} (Array DocumentsView))
                     { totalRecords :: Int
                     , dispatch :: Action -> Effect Unit
                     , deletionState :: State
                     | props
                     }
                     (Loader.Action PageParams)
renderPage _ _ {loaded: Nothing} _ = [] -- TODO loading spinner
renderPage loaderDispatch { totalRecords, dispatch
                          , deletionState: {documentIdsToDelete, documentIdsDeleted, localFavorites}}
                          {currentPath: {nodeId, tabType}, loaded: Just res} _ =
  [ T.tableElt
      { rows
      , setParams: \params -> liftEffect $ loaderDispatch (Loader.SetPath {nodeId, tabType, params})
      , container: T.defaultContainer { title: "Documents" }
      , colNames:
          T.ColumnName <$>
          [ ""
          , "Date"
          , "Title"
          , "Source"
          , "Delete"
          ]
      , totalRecords
      }
  ]
  where
    fa true  = "fas "
    fa false = "far "
    isChecked _id = Set.member _id documentIdsToDelete
    toDelete  (DocumentsView {_id}) = Set.member _id documentIdsToDelete
    isDeleted (DocumentsView {_id}) = Set.member _id documentIdsDeleted
    isFavorite {_id,fav} = maybe fav identity (localFavorites ^. at _id)
    rows = (\(DocumentsView r) ->
                let isFav = isFavorite r in
                { row:
                    [ div []
                      [ a [ className $ fa isFav <> "fa-star"
                          , if (toDelete $ DocumentsView r) then style {textDecoration : "line-through"}
                                                            else style {textDecoration : ""}
                          , onClick $ (\_-> dispatch $ MarkFavorites r._id (not isFav))] []
                      ]
                    -- TODO show date: Year-Month-Day only
                    , if (toDelete $ DocumentsView r) then
                        div [ style {textDecoration : "line-through"}][text r.date]
                      else
                        div [ ][text r.date]
                    , if (toDelete $ DocumentsView r) then
                        a [ href (toUrl Front Url_Document (Just r._id))
                          , style {textDecoration : "line-through"}
                          , target "_blank"
                        ] [ text r.title ]
                      else
                        a [ href (toUrl Front Url_Document (Just r._id))
                        , target "_blank" ] [ text r.title ]
                    , if (toDelete $ DocumentsView r) then
                        div [style {textDecoration : "line-through"}] [ text r.source]
                      else
                        div [] [ text r.source]
                    , input [ _type "checkbox"
                            , checked (isChecked r._id)
                            , onClick $ (\_ -> dispatch $ ToggleDocumentToDelete r._id)]
                    ]
                , delete: true
                }) <$> filter (not <<< isDeleted) res

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
