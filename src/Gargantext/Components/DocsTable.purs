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
import Data.Map (Map, insert)
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Int (fromString)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Event as DE
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import React as React
import React (ReactClass, ReactElement, Children)
import Reactix as R
import Reactix.SyntheticEvent as RE
import Reactix.DOM.HTML as H
import Unsafe.Coerce (unsafeCoerce)
------------------------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Config (End(..), NodeType(..), OrderBy(..), Path(..), TabType, TabPostQuery(..), toUrl, toLink)
import Gargantext.Config.REST (get, put, post, deleteWithBody, delete)
import Gargantext.Components.Loader2 (useLoader)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Search.Types (Category(..), CategoryQuery(..), favCategory, trashCategory, decodeCategory, putCategories)
import Gargantext.Components.Table as T
import Gargantext.Utils.DecodeMaybe ((.|))
import Gargantext.Utils.Reactix as R2
import Gargantext.Router as Router
import Thermite (PerformAction, Render, Spec, defaultPerformAction, modifyState_, simpleSpec, hideState)
------------------------------------------------------------------------

type NodeID = Int
type TotalRecords = Int

type Props =
  { nodeId       :: Int
  , totalRecords :: Int
  , chart        :: R.Element
  , tabType      :: TabType
  , listId       :: Int
  , corpusId     :: Maybe Int
  , showSearch   :: Boolean
  -- ^ tabType is not ideal here since it is too much entangled with tabs and
  -- ngramtable. Let's see how this evolves.
  }

type PageLoaderProps =
  { nodeId    :: Int
  , totalRecords :: Int
  , tabType      :: TabType
  , listId       :: Int
  , corpusId     :: Maybe Int
  , query       :: Query
  }

type LocalCategories = Map Int Category
type Query = String

_documentIdsDeleted  = prop (SProxy :: SProxy "documentIdsDeleted")
_localCategories     = prop (SProxy :: SProxy "localCategories")

data Action
  = MarkCategory Int Category


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

instance decodeResponse :: DecodeJson Response where
  decodeJson json = do
    obj        <- decodeJson json
    cid        <- obj .? "id"
    favorite   <- obj .? "favorite"
    ngramCount <- obj .? "id"
    hyperdata  <- obj .? "hyperdata"
    pure $ Response { cid, category: decodeCategory favorite, ngramCount, hyperdata }


docViewSpec :: Props -> R.Element
docViewSpec p = R.createElement el p []
  where
    el = R.hooksComponent "DocView" cpt
    cpt p _children = do
      query <- R.useState' ("" :: Query)
      tableParams <- R.useState' T.initialParams

      pure $ layoutDocview query tableParams p

-- | Main layout of the Documents Tab of a Corpus
layoutDocview :: R.State Query -> R.State T.Params -> Props -> R.Element
layoutDocview query tableParams@(params /\ _) p = R.createElement el p []
  where
    el = R.hooksComponent "LayoutDocView" cpt
    cpt {nodeId, tabType, listId, corpusId, totalRecords, chart, showSearch} _children = do
      pure $ H.div {className: "container1"}
        [ H.div {className: "row"}
          [ chart
          , if showSearch then searchBar query else H.div {} []
          , H.div {className: "col-md-12"}
            [ pageLoader tableParams {nodeId, totalRecords, tabType, listId, corpusId, query: fst query} ]
          , H.div {className: "col-md-1 col-md-offset-11"}
            [ H.button { className: "btn"
                       , style: {backgroundColor: "peru", color : "white", border : "white"}
                       , onClick: onClickTrashAll nodeId
                       }
              [  H.i {className: "glyphitem glyphicon glyphicon-trash"} []
              ,  H.text "Trash all"
              ]
            ]
          ]
        ]

    onClickTrashAll nodeId = mkEffectFn1 $ \_ -> do
      launchAff $ deleteAllDocuments nodeId

searchBar :: R.State Query -> R.Element
searchBar (query /\ setQuery) = R.createElement el {} []
  where
    el = R.hooksComponent "SearchBar" cpt
    cpt {} _children = do
      queryText <- R.useState' query

      pure $ H.div {className: "row"}
        [ H.div {className: "col col-md-3"} []
        , H.div {className: "col col-md-1"} [searchButton queryText]
        , H.div {className: "col col-md-3 form-group"}
          [ H.input { type: "text"
                    , className: "form-control"
                    , on: {change: onSearchChange queryText, keyUp: onSearchKeyup queryText}
                    , placeholder: query
                    , defaultValue: query}
          ]
        , H.div {className: "col col-md-1"} [if query /= "" then clearButton else H.div {} []]
        ]

    onSearchChange :: forall e. R.State Query -> e -> Effect Unit
    onSearchChange (_ /\ setQueryText) = \e ->
      setQueryText $ const $ R2.unsafeEventValue e

    onSearchKeyup :: R.State Query -> DE.KeyboardEvent -> Effect Unit
    onSearchKeyup (queryText /\ _) = \e ->
      if DE.key e == "Enter" then
        setQuery $ const queryText
      else
        pure $ unit

    searchButton (queryText /\ _) =
      H.button { type: "submit"
               , className: "btn btn-default"
               , on: {click: \e -> setQuery $ const queryText}}
      [ H.span {className: "glyphicon glyphicon-search"} [] ]

    clearButton =
      H.button { className: "btn btn-danger"
               , on: {click: \e -> setQuery $ const ""}}
      [ H.span {className: "glyphicon glyphicon-remove"} [] ]

mock :: Boolean
mock = false

type PageParams = { nodeId :: Int
                  , listId :: Int
                  , corpusId :: Maybe Int
                  , tabType :: TabType
                  , query   :: Query
                  , params :: T.Params}

loadPage :: PageParams -> Aff (Array DocumentsView)
loadPage {nodeId, tabType, query, listId, corpusId, params: {limit, offset, orderBy}} = do
  logs "loading documents page: loadPage with Offset and limit"
  -- res <- get $ toUrl Back (Tab tabType offset limit (convOrderBy <$> orderBy)) (Just nodeId)
  let url = (toUrl Back Node (Just nodeId)) <> "/table"
  res <- post url $ TabPostQuery {
      offset
    , limit
    , orderBy: convOrderBy orderBy
    , tabType
    , query
    }
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
    convOrderBy (Just (T.ASC  (T.ColumnName "Date")))  = DateAsc
    convOrderBy (Just (T.DESC (T.ColumnName "Date")))  = DateDesc
    convOrderBy (Just (T.ASC  (T.ColumnName "Title"))) = TitleAsc
    convOrderBy (Just (T.DESC (T.ColumnName "Title"))) = TitleDesc
    convOrderBy (Just (T.ASC  (T.ColumnName "Source"))) = SourceAsc
    convOrderBy (Just (T.DESC (T.ColumnName "Source"))) = SourceDesc

    convOrderBy _ = DateAsc -- TODO

renderPage :: R.State T.Params -> PageLoaderProps -> Array DocumentsView -> R.Element
renderPage (_ /\ setTableParams) p res = R.createElement el p []
  where
    el = R.hooksComponent "RenderPage" cpt

    gi Favorite  = "glyphicon glyphicon-star"
    gi _ = "glyphicon glyphicon-star-empty"
    trashStyle Trash = {textDecoration: "line-through"}
    trashStyle _ = {textDecoration: "none"}
    corpusDocument (Just corpusId) = Router.CorpusDocument corpusId
    corpusDocument _ = Router.Document

    cpt {nodeId, corpusId, listId, totalRecords} _children = do
      localCategories <- R.useState' (mempty :: LocalCategories)

      pure $ R2.buff $ T.tableElt
          { rows: rows localCategories
          -- , setParams: \params -> liftEffect $ loaderDispatch (Loader.SetPath {nodeId, tabType, listId, corpusId, params, query})
          , setParams: \params -> setTableParams $ const params
          , container: T.defaultContainer { title: "Documents" }
          , colNames:
            T.ColumnName <$>
            [ "Favorites"
            , "Trash"
            , "Date"
            , "Title"
            , "Source"
            ]
          , totalRecords
          }
      where
        getCategory (localCategories /\ _) {_id, category} = maybe category identity (localCategories ^. at _id)
        rows localCategories = (\(DocumentsView r) ->
                    let cat = getCategory localCategories r
                        isDel = Trash == cat in
                    { row: map R2.scuff $ [
                          H.div {}
                          [ H.a { className: gi cat
                                , style: trashStyle cat
                                , on: {click: onClick localCategories Favorite r._id cat}
                                } []
                          ]
                        , H.input { type: "checkbox"
                                  , checked: isDel
                                  , on: {click: onClick localCategories Trash r._id cat}
                                  }
                        -- TODO show date: Year-Month-Day only
                        , H.div { style: trashStyle cat } [ H.text (show r.date) ]
                        , H.a { href: toLink $ (corpusDocument corpusId) listId r._id
                              , style: trashStyle cat
                              , target: "_blank"
                              } [ H.text r.title ]
                        , H.div { style: trashStyle cat} [ H.text r.source ]
                        ]
                    , delete: true
                    }) <$> res
        onClick (_ /\ setLocalCategories) catType nid cat = \_-> do
          let newCat = if (catType == Favorite) then (favCategory cat) else (trashCategory cat)
          setLocalCategories $ insert nid newCat
          void $ launchAff $ putCategories nodeId $ CategoryQuery {nodeIds: [nid], category: newCat}

pageLoader :: R.State T.Params -> PageLoaderProps -> R.Element
pageLoader tableParams@(pageParams /\ _) p = R.createElement el p []
  where
    el = R.hooksComponent "PageLoader" cpt
    cpt p@{nodeId, listId, corpusId, tabType, query} _children = do
      useLoader {nodeId, listId, corpusId, tabType, query, params: pageParams} loadPage $ \{loaded} ->
        renderPage tableParams p loaded

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

documentsUrl :: Int -> String
documentsUrl nodeId = toUrl Back Node (Just nodeId) <> "/documents"

deleteAllDocuments :: Int -> Aff (Array Int)
deleteAllDocuments nodeId = delete $ documentsUrl nodeId

-- TODO: not optimal but Data.Set lacks some function (Set.alter)
toggleSet :: forall a. Ord a => a -> Set a -> Set a
toggleSet a s
  | Set.member a s = Set.delete a s
  | otherwise      = Set.insert a s
