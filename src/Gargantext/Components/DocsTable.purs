-- TODO: this module should be replaced by FacetsTable
module Gargantext.Components.DocsTable where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>), encodeJson)
import Data.Array (drop, take)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((^.))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log3)
import DOM.Simple.Event as DE
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.Loader (loader)
import Gargantext.Components.Table as T
import Gargantext.Ends (Frontends, url)
import Gargantext.Routes as Routes
import Gargantext.Routes (AppRoute, SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, sessionId, post, delete, put)
import Gargantext.Types (NodeType(..), OrderBy(..), TabType, TabPostQuery(..))
import Gargantext.Utils.Reactix as R2

data Category = Trash | UnRead | Checked | Topic | Favorite

categories :: Array Category
categories = [Trash, UnRead, Checked, Topic, Favorite]

derive instance genericFavorite :: Generic Category _
instance showCategory :: Show Category where
  show = genericShow
instance eqCategory :: Eq Category where
  eq = genericEq
instance encodeJsonCategory :: EncodeJson Category where
  encodeJson cat    = encodeJson (cat2score cat)

favCategory :: Category -> Category
favCategory Favorite = Topic
favCategory _        = Favorite

trashCategory :: Category -> Category
trashCategory _     = Trash
trashCategory Trash = UnRead

decodeCategory :: Int -> Category
decodeCategory 0 = Trash
decodeCategory 1 = UnRead
decodeCategory 2 = Checked
decodeCategory 3 = Topic
decodeCategory 4 = Favorite
decodeCategory _ = UnRead

cat2score :: Category -> Int
cat2score Trash    = 0
cat2score UnRead   = 1
cat2score Checked  = 2
cat2score Topic    = 3
cat2score Favorite = 4

-- carousel :: Category -> R.Element
carousel session nodeId setLocalCategories r cat = H.div {className:"flex"} divs
  where
    divs = map (\c -> if cat == c
                        then
                          H.div { className : icon c (cat == c) } []

                        else
                          H.div { className : icon c (cat == c)
                            , on: { click: onClick nodeId setLocalCategories r c}
                             } []
                    ) (carousel' cat)

    carousel' :: Category -> Array Category
    carousel' Trash = take 2 categories
    carousel' cat   = take 3 $ drop (cat2score cat - 1 ) categories

    onClick nodeId' setLocalCats r' cat' = \_-> do
      setLocalCats $ Map.insert r'._id cat'
      void $ launchAff $ putCategories session nodeId' $ CategoryQuery {nodeIds: [r'._id], category: cat'}


icon :: Category -> Boolean -> String
icon cat b = btn b $ "glyphicon glyphicon-" <> (color $ size b $ icon' cat b)
  where
    icon' :: Category -> Boolean -> String
    icon' Trash   false = "remove"
    icon' Trash   true  = "remove-sign"

    icon' UnRead  true  = "question-sign"
    icon' UnRead  false = "question-sign"

    icon' Checked true  = "ok-sign"
    icon' Checked false = "ok"

    icon' Topic  true  = "star"
    icon' Topic  false = "star-empty"

    icon' Favorite true = "heart"
    icon' Favorite false = "heart-empty"

    size :: Boolean -> String -> String
    size true  s = s <> " btn-lg"
    size false s = s <> " btn-xs"

    color :: String -> String
    color x = x <> " text-primary"

    btn :: Boolean -> String -> String
    btn true s = s
    btn false s = "btn " <> s


newtype CategoryQuery = CategoryQuery {
    nodeIds :: Array Int
  , category :: Category
  }

instance encodeJsonCategoryQuery :: EncodeJson CategoryQuery where
  encodeJson (CategoryQuery post) =
    "ntc_nodesId" := post.nodeIds
    ~> "ntc_category" := encodeJson post.category
    ~> jsonEmptyObject

categoryRoute :: Int -> SessionRoute
categoryRoute nodeId = NodeAPI Node (Just nodeId) "category"

putCategories :: Session -> Int -> CategoryQuery -> Aff (Array Int)
putCategories session nodeId = put session $ categoryRoute nodeId

type NodeID = Int
type TotalRecords = Int

type LayoutProps =
  ( nodeId       :: Int
  , totalRecords :: Int
  , chart        :: R.Element
  , tabType      :: TabType
  , listId       :: Int
  , corpusId     :: Maybe Int
  , showSearch   :: Boolean
  , frontends    :: Frontends
  , session      :: Session )
  -- ^ tabType is not ideal here since it is too much entangled with tabs and
  -- ngramtable. Let's see how this evolves.  )

type PageLayoutProps =
  ( nodeId       :: Int
  , totalRecords :: Int
  , tabType      :: TabType
  , listId       :: Int
  , corpusId     :: Maybe Int
  , query        :: Query
  , session      :: Session
  , frontends    :: Frontends
  , params       :: R.State T.Params )

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
    title  <- obj .: "title"
    source <- obj .: "source"
    pub_year <- obj .: "publication_year"
    pure $ Hyperdata { title,source, pub_year}

instance decodeResponse :: DecodeJson Response where
  decodeJson json = do
    obj        <- decodeJson json
    cid        <- obj .: "id"
    favorite   <- obj .: "favorite"
    ngramCount <- obj .: "id"
    hyperdata  <- obj .: "hyperdata"
    pure $ Response { cid, category: decodeCategory favorite, ngramCount, hyperdata }


docViewLayout :: Record LayoutProps -> R.Element
docViewLayout props = R.createElement docViewLayoutCpt props []

docViewLayoutCpt :: R.Component LayoutProps
docViewLayoutCpt = R.hooksComponent "G.C.DocsTable.docViewLayout" cpt
  where
    cpt layout _children = do
      query <- R.useState' ""
      params <- R.useState' T.initialParams
      pure $ docView {query, params, layout}

type Props =
  ( query :: R.State Query
  , params :: R.State T.Params
  , layout :: Record LayoutProps )

docView :: Record Props -> R.Element
docView props = R.createElement docViewCpt props []

docViewCpt :: R.Component Props
docViewCpt = R.hooksComponent "G.C.DocsTable.docView" cpt where
  cpt { query, params
      , layout: { frontends, session, nodeId, tabType, listId
                , corpusId, totalRecords, chart, showSearch } } _ = do
    pure $ H.div {className: "container1"}
      [ H.div {className: "row"}
        [ chart
        , if showSearch then searchBar query else H.div {} []
        , H.div {className: "col-md-12"}
          [ pageLayout {frontends, session, nodeId, totalRecords, tabType, listId, corpusId, query: fst query, params} ] ] ]
    -- onClickTrashAll nodeId _ = do
    --   launchAff $ deleteAllDocuments p.session nodeId
          
          {-, H.div {className: "col-md-1 col-md-offset-11"}
            [ pageLayout p.session params {nodeId, totalRecords, tabType, listId, corpusId, query: fst query} ]
          , H.div {className: "col-md-1 col-md-offset-11"}
            [ H.button { className: "btn"
                       , style: {backgroundColor: "peru", color : "white", border : "white"}
                       , on: { click: onClickTrashAll nodeId } }
              [  H.i {className: "glyphitem glyphicon glyphicon-trash"} []
              ,  H.text "Trash all"
              ]
            ]
           -}

searchBar :: R.State Query -> R.Element
searchBar (query /\ setQuery) = R.createElement el {} []
  where
    el = R.hooksComponent "SearchBar" cpt
    cpt {} _children = do
      queryText <- R.useState' query

      pure $ H.div {className: "row"}
        [ H.div {className: "col col-md-3"} []
        , H.div {className: "col col-md-1"} [if query /= "" then clearButton else H.div {} []]
        , H.div {className: "col col-md-3 form-group"}
          [ H.input { type: "text"
                    , className: "form-control"
                    , on: {change: onSearchChange queryText, keyUp: onSearchKeyup queryText}
                    , placeholder: query
                    , defaultValue: query}
          ]
        , H.div {className: "col col-md-1"} [searchButton queryText]
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

type PageParams =
  { nodeId :: Int
  , listId :: Int
  , corpusId :: Maybe Int
  , tabType :: TabType
  , query   :: Query
  , params :: T.Params}

loadPage :: Session -> PageParams -> Aff (Array DocumentsView)
loadPage session {nodeId, tabType, query, listId, corpusId, params: {limit, offset, orderBy}} = do
  liftEffect $ log3 "loading documents page: loadPage with Offset and limit" offset limit
  -- res <- get $ toUrl endConfigStateful Back (Tab tabType offset limit (convOrderBy <$> orderBy)) (Just nodeId)
  let p = NodeAPI Node (Just nodeId) "table"
  res <- post session p $ TabPostQuery {
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

pageLayout :: Record PageLayoutProps -> R.Element
pageLayout props = R.createElement pageLayoutCpt props []

pageLayoutCpt :: R.Memo PageLayoutProps
pageLayoutCpt = R.memo' $ R.staticComponent "G.C.DocsTable.pageLayout" cpt where
  cpt props@{frontends, session, nodeId, listId, corpusId, tabType, query, params} _ =
    loader path (loadPage session) paint
    where
      path = {nodeId, listId, corpusId, tabType, query, params: fst params}
      paint loaded = page params props loaded

type PageProps =
  ( params :: R.State T.Params
  , layout :: Record PageLayoutProps
  , documents :: Array DocumentsView )

page ::  R.State T.Params -> Record PageLayoutProps -> Array DocumentsView -> R.Element
page params layout documents = R.createElement pageCpt {params, layout, documents} []

pageCpt :: R.Memo PageProps
pageCpt = R.memo' $ R.hooksComponent "G.C.DocsTable.pageCpt" cpt where
  cpt { layout: {frontends, session, nodeId, corpusId, listId, totalRecords}, documents, params } _ = do
    localCategories <- R.useState' (mempty :: LocalCategories)
    pure $ T.table
      { rows: rows localCategories
      , container: T.defaultContainer { title: "Documents" }
      , params, colNames, totalRecords, wrapColElts }
      where
        sid = sessionId session
        gi Favorite  = "glyphicon glyphicon-star"
        gi _ = "glyphicon glyphicon-star-empty"
        trashStyle Trash = {textDecoration: "line-through"}
        trashStyle _ = {textDecoration: "none"}
        corpusDocument
          | Just cid <- corpusId = Routes.CorpusDocument sid cid listId
          | otherwise = Routes.Document sid listId
        colNames = T.ColumnName <$> [ "Tag", "Date", "Title", "Source"]
        wrapColElts = const identity
        getCategory (localCategories /\ _) {_id, category} = maybe category identity (localCategories ^. at _id)
        rows localCategories = row <$> documents
          where
            row (DocumentsView r) =
              { row:
                [ -- H.div {} [ H.a { className, style, on: {click: click Favorite} } [] ]
                 carousel session nodeId setLocalCategories r cat
                --, H.input { type: "checkbox", defaultValue: checked, on: {click: click Trash} }
                -- TODO show date: Year-Month-Day only
                , H.div { style } [ R2.showText r.date ]
                , H.div { style }
                  [ H.a { href: url frontends $ corpusDocument r._id } [ H.text r.title ] ]
                , H.div { style } [ H.text $ if r.source == "" then "Source" else r.source ]
                ]
              , delete: true }
              where
                cat         = getCategory localCategories r
                (_ /\ setLocalCategories) = localCategories
                checked    = Trash == cat
                style      = trashStyle cat
                className  = gi cat

---------------------------------------------------------
sampleData' :: DocumentsView
sampleData' = DocumentsView { _id : 1
                            , url : ""
                            , date : 2010
                            , title : "title"
                            , source : "source"
                            , category : UnRead
                            , ngramCount : 1}

sampleData :: Array DocumentsView
--sampleData = replicate 10 sampleData'
sampleData = map (\(Tuple t s) -> DocumentsView { _id : 1
                                                , url : ""
                                                , date : 2017
                                                , title: t
                                                , source: s
                                                , category : UnRead
                                                , ngramCount : 10}) sampleDocuments

sampleDocuments :: Array (Tuple String String)
sampleDocuments = [Tuple "Macroscopic dynamics of the fusion process" "Journal de Physique Lettres",Tuple "Effects of static and cyclic fatigue at high temperature upon reaction bonded silicon nitride" "Journal de Physique Colloques",Tuple "Reliability of metal/glass-ceramic junctions made by solid state bonding" "Journal de Physique Colloques",Tuple "High temperature mechanical properties and intergranular structure of sialons" "Journal de Physique Colloques",Tuple "SOLUTIONS OF THE LANDAU-VLASOV EQUATION IN NUCLEAR PHYSICS" "Journal de Physique Colloques",Tuple "A STUDY ON THE FUSION REACTION 139La + 12C AT 50 MeV/u WITH THE VUU EQUATION" "Journal de Physique Colloques",Tuple "Atomic structure of \"vitreous\" interfacial films in sialon" "Journal de Physique Colloques",Tuple "MICROSTRUCTURAL AND ANALYTICAL CHARACTERIZATION OF Al2O3/Al-Mg COMPOSITE INTERFACES" "Journal de Physique Colloques",Tuple "Development of oxidation resistant high temperature NbTiAl alloys and intermetallics" "Journal de Physique IV Colloque",Tuple "Determination of brazed joint constitutive law by inverse method" "Journal de Physique IV Colloque",Tuple "Two dimensional estimates from ocean SAR images" "Nonlinear Processes in Geophysics",Tuple "Comparison Between New Carbon Nanostructures Produced by Plasma with Industrial Carbon Black Grades" "Journal de Physique III",Tuple "<i>Letter to the Editor:</i> SCIPION, a new flexible ionospheric sounder in Senegal" "Annales Geophysicae",Tuple "Is reducibility in nuclear multifragmentation related to thermal scaling?" "Physics Letters B",Tuple "Independence of fragment charge distributions of the size of heavy multifragmenting sources" "Physics Letters B",Tuple "Hard photons and neutral pions as probes of hot and dense nuclear matter" "Nuclear Physics A",Tuple "Surveying the nuclear caloric curve" "Physics Letters B",Tuple "A hot expanding source in 50 A MeV Xe+Sn central reactions" "Physics Letters B"]

newtype SearchQuery = SearchQuery
  { query :: Array String
  , parent_id :: Int
  }


instance encodeJsonSQuery :: EncodeJson SearchQuery where
  encodeJson (SearchQuery {query, parent_id})
     = "query" := query
    ~> "parent_id" := parent_id
    ~> jsonEmptyObject



searchResults :: SearchQuery -> Aff Int
searchResults squery = pure 42 -- TODO post "http://localhost:8008/count" unit

documentsRoute :: Int -> SessionRoute
documentsRoute nodeId = NodeAPI Node (Just nodeId) "documents"

deleteAllDocuments :: Session -> Int -> Aff (Array Int)
deleteAllDocuments session = delete session <<< documentsRoute

-- TODO: not optimal but Data.Set lacks some function (Set.alter)
toggleSet :: forall a. Ord a => a -> Set a -> Set a
toggleSet a s
  | Set.member a s = Set.delete a s
  | otherwise      = Set.insert a s
