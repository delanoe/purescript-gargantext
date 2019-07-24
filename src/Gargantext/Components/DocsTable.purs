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
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..))
import Data.Lens
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Int (fromString)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import React as React
import React (ReactClass, ReactElement, Children)
------------------------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Config (End(..), NodeType(..), OrderBy(..), Path(..), TabType, toUrl, toLink)
import Gargantext.Config.REST (get, put, post, deleteWithBody, delete)
import Gargantext.Components.Loader as Loader
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Table as T
import Gargantext.Utils.DecodeMaybe ((.|))
import Gargantext.Router as R
import React.DOM (a, br', button, div, i, input, p, text)
import React.DOM.Props (_type, className, href, onClick, placeholder, style, checked, target)
import Thermite (PerformAction, Render, Spec, defaultPerformAction, modifyState_, simpleSpec, hideState)
------------------------------------------------------------------------

type NodeID = Int
type TotalRecords = Int
data Category = Trash | Normal | Favorite
derive instance genericFavorite :: Generic Category _
instance showCategory :: Show Category where
  show = genericShow
instance eqCategory :: Eq Category where
  eq = genericEq
instance encodeJsonCategory :: EncodeJson Category where
  encodeJson Trash = encodeJson 0
  encodeJson Normal = encodeJson 1
  encodeJson Favorite = encodeJson 2

favCategory :: Category -> Category
favCategory Normal = Favorite
favCategory Trash = Favorite
favCategory Favorite = Normal

trashCategory :: Category -> Category
trashCategory Normal = Trash
trashCategory Trash = Normal
trashCategory Favorite = Trash

type Props =
  { nodeId       :: Int
  , totalRecords :: Int
  , chart        :: ReactElement
  , tabType      :: TabType
  , listId       :: Int
  , corpusId     :: Maybe Int
  -- ^ tabType is not ideal here since it is too much entangled with tabs and
  -- ngramtable. Let's see how this evolves.
  }

type State =
  { documentIdsDeleted  :: Set Int
  , localCategories     :: Map Int Category
  }

initialState :: State
initialState =
  { documentIdsDeleted:  mempty
  , localCategories:     mempty
  }

_documentIdsDeleted  = prop (SProxy :: SProxy "documentIdsDeleted")
_localCategories     = prop (SProxy :: SProxy "localCategories")

data Action
  = MarkCategory Int Category
  | TrashAll

newtype DocumentsView
  = DocumentsView
    { _id    :: Int
    , url    :: String
    , date   :: Int
    , title  :: String
    , source :: String
    , category :: Category
    , ngramCount :: Int
    }


derive instance genericDocumentsView :: Generic DocumentsView _

instance showDocumentsView :: Show DocumentsView where
  show = genericShow


newtype Response = Response
  { cid        :: Int
  , hyperdata  :: Hyperdata
  , category   :: Category
  , ngramCount :: Int
  }


newtype Hyperdata = Hyperdata
  { title  :: String
  , source :: String
  , pub_year   :: Int
  }


instance decodeHyperdata :: DecodeJson Hyperdata where
  decodeJson json = do
    obj    <- decodeJson json
    title  <- obj .? "title"
    source <- obj .? "source"
    pub_year <- obj .? "publication_year"
    pure $ Hyperdata { title,source, pub_year}

decodeCategory :: Int -> Category
decodeCategory 0 = Trash
decodeCategory 1 = Normal
decodeCategory 2 = Favorite
decodeCategory _ = Normal

instance decodeResponse :: DecodeJson Response where
  decodeJson json = do
    obj        <- decodeJson json
    cid        <- obj .? "id"
    favorite   <- obj .? "favorite"
    ngramCount <- obj .? "id"
    hyperdata  <- obj .? "hyperdata"
    pure $ Response { cid, category: decodeCategory favorite, ngramCount, hyperdata }



-- | Filter
filterSpec :: forall state props action. Spec state props action
filterSpec = simpleSpec defaultPerformAction render
  where
    render d p s c = []

docViewSpec :: Spec {} Props Void
docViewSpec = hideState (const initialState) layoutDocview

-- | Main layout of the Documents Tab of a Corpus
layoutDocview :: Spec State Props Action
layoutDocview = simpleSpec performAction render
  where
    performAction :: PerformAction State Props Action
    performAction (MarkCategory nid cat) {nodeId} _ = do
      modifyState_ $ _localCategories <<< at nid ?~ cat
      void $ lift $ putCategories  nodeId $ CategoryQuery {nodeIds: [nid], category: cat}
    performAction TrashAll {nodeId} {documentIdsDeleted} = do
      ids <- lift $ deleteAllDocuments nodeId
      modifyState_ $ _ {documentIdsDeleted = Set.union documentIdsDeleted $ Set.fromFoldable ids}

    render :: Render State Props Action
    render dispatch {nodeId, tabType, listId, corpusId, totalRecords, chart} deletionState _ =
      [
      div [className "container1"]
        [ div [className "row"]
          [ chart
          , div [className "col-md-12"]
            [ pageLoader
                { path: initialPageParams {nodeId, tabType, listId, corpusId}
                , listId
                , corpusId
                , totalRecords
                , deletionState
                , dispatch
                }
            ]
          , div [className "col-md-1 col-md-offset-11"]
             [ button [ className "btn"
                      , style {backgroundColor: "peru", color : "white", border : "white"}
                      , onClick $ (\_ -> dispatch TrashAll)
                      ]
               [  i [className "glyphitem glyphicon glyphicon-trash"] []
               ,  text "Trash all"
               ]
             ]
          ]
        ]
      ]

mock :: Boolean
mock = false

type PageParams = {nodeId :: Int, listId :: Int, corpusId :: Maybe Int, tabType :: TabType, params :: T.Params}

initialPageParams :: {nodeId :: Int, listId :: Int, corpusId :: Maybe Int, tabType :: TabType} -> PageParams
initialPageParams {nodeId, listId, corpusId, tabType} =
  {nodeId, tabType, listId, corpusId, params: T.initialParams}

loadPage :: PageParams -> Aff (Array DocumentsView)
loadPage {nodeId, tabType, listId, corpusId, params: {limit, offset, orderBy}} = do
  logs "loading documents page: loadPage with Offset and limit"
  res <- get $ toUrl Back (Tab tabType offset limit (convOrderBy <$> orderBy)) (Just nodeId)
  let docs = res2corpus <$> res
  pure $
    if mock then take limit $ drop offset sampleData else
    docs
  where
    res2corpus :: Response -> DocumentsView
    res2corpus (Response r) =
      DocumentsView { _id : r.cid
      , url    : ""
      , date   : (\(Hyperdata hr) -> hr.pub_year) r.hyperdata
      , title  : (\(Hyperdata hr) -> hr.title) r.hyperdata
      , source : (\(Hyperdata hr) -> hr.source) r.hyperdata
      , category : r.category
      , ngramCount : r.ngramCount
     }
    convOrderBy (T.ASC  (T.ColumnName "Date"))  = DateAsc
    convOrderBy (T.DESC (T.ColumnName "Date"))  = DateDesc
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
  , listId :: Int
  , corpusId :: Maybe Int
  | row
  }

renderPage :: forall props path.
              Render (Loader.State {nodeId :: Int, listId :: Int, corpusId :: Maybe Int, tabType :: TabType | path} (Array DocumentsView))
                     { totalRecords :: Int
                     , dispatch :: Action -> Effect Unit
                     , deletionState :: State
                     , listId :: Int
                     , corpusId :: Maybe Int
                     | props
                     }
                     (Loader.Action PageParams)
renderPage _ _ {loaded: Nothing} _ = [] -- TODO loading spinner
renderPage loaderDispatch { totalRecords, dispatch, listId, corpusId
                          , deletionState: {documentIdsDeleted, localCategories}}
                          {currentPath: {nodeId, tabType}, loaded: Just res} _ =
  [ T.tableElt
      { rows
      , setParams: \params -> liftEffect $ loaderDispatch (Loader.SetPath {nodeId, tabType, listId, corpusId, params})
      , container: T.defaultContainer { title: "Documents" }
      , colNames:
          T.ColumnName <$>
          [ "Map"
          , "Stop"
          , "Date"
          , "Title"
          , "Source"
          ]
      , totalRecords
      }
  ]
  where
    gi Favorite  = "glyphicon glyphicon-star"
    gi _ = "glyphicon glyphicon-star-empty"
    trashStyle Trash = style {textDecoration: "line-through"}
    trashStyle _ = style {textDecoration: "none"}
    getCategory {_id, category} = maybe category identity (localCategories ^. at _id)
    corpusDocument (Just corpusId) = R.CorpusDocument corpusId
    corpusDocument _ = R.Document
    rows = (\(DocumentsView r) ->
                let cat = getCategory r
                    isDel = Trash == cat in
                { row:
                    [ div []
                      [ a [ className $ gi cat
                          , trashStyle cat
                          , onClick $ \_-> dispatch $ MarkCategory r._id $ favCategory cat
                          ] []
                      ]
                    , input [ _type "checkbox"
                            , checked isDel
                            , onClick $ \_ -> dispatch $ MarkCategory r._id $ trashCategory cat
                            ]
                    -- TODO show date: Year-Month-Day only
                    , div [ trashStyle cat ][text (show r.date)]
                    , a [ href (toLink $ (corpusDocument corpusId) listId r._id)
                        , trashStyle cat
                        , target "_blank"
                        ] [ text r.title ]
                    , div [trashStyle cat] [ text r.source ]
                    ]
                , delete: true
                }) <$> res

pageLoaderClass :: ReactClass (PageLoaderProps (children :: Children))
pageLoaderClass = Loader.createLoaderClass' "PageLoader" loadPage renderPage

pageLoader :: PageLoaderProps () -> ReactElement
pageLoader props = React.createElement pageLoaderClass props []

---------------------------------------------------------
sampleData' :: DocumentsView
sampleData' = DocumentsView { _id : 1
                            , url : ""
                            , date : 2010
                            , title : "title"
                            , source : "source"
                            , category : Normal
                            , ngramCount : 1}

sampleData :: Array DocumentsView
--sampleData = replicate 10 sampleData'
sampleData = map (\(Tuple t s) -> DocumentsView { _id : 1
                                                , url : ""
                                                , date : 2017
                                                , title: t
                                                , source: s
                                                , category : Normal
                                                , ngramCount : 10}) sampleDocuments

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


newtype CategoryQuery = CategoryQuery {
    nodeIds :: Array Int
  , category :: Category
  }

instance encodeJsonCategoryQuery :: EncodeJson CategoryQuery where
  encodeJson (CategoryQuery post) =
       "ntc_nodesId" := post.nodeIds
    ~> "ntc_category" := encodeJson post.category
    ~> jsonEmptyObject

categoryUrl :: Int -> String
categoryUrl nodeId = toUrl Back Node (Just nodeId) <> "/category"

putCategories :: Int -> CategoryQuery -> Aff (Array Int)
putCategories nodeId = put $ categoryUrl nodeId

documentsUrl :: Int -> String
documentsUrl nodeId = toUrl Back Node (Just nodeId) <> "/documents"

deleteAllDocuments :: Int -> Aff (Array Int)
deleteAllDocuments nodeId = delete $ documentsUrl nodeId

-- TODO: not optimal but Data.Set lacks some function (Set.alter)
toggleSet :: forall a. Ord a => a -> Set a -> Set a
toggleSet a s
  | Set.member a s = Set.delete a s
  | otherwise      = Set.insert a s
