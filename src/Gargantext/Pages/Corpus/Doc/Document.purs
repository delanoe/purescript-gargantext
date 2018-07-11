module Gargantext.Pages.Corpus.Doc.Document where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Generic (class Generic, gShow)
import Data.Tuple (Tuple(..))
import Gargantext.Components.Charts.Charts (p'')
import Gargantext.Config.REST (get)
import Network.HTTP.Affjax (AJAX)
import React (ReactElement)
import React.DOM (a, b, b', br', div, input, option, select, span, table, tbody, td, text, th, thead, tr)
import React.DOM.Props (_type, className, href, onChange, onClick, scope, selected, value)
import Thermite (PerformAction, Render, Spec, cotransform, defaultPerformAction, modifyState, simpleSpec)
import Unsafe.Coerce (unsafeCoerce)


--main :: forall e. Eff (dom:: DOM, console :: CONSOLE, ajax :: AJAX | e) Unit
--main = do
--  case createReactSpec layoutDocview tdata of
--    { spec, dispatcher } -> void $ do
--      document  <- DOM.window >>= DOM.document
--      container <- unsafePartial (fromJust  <$> DOM.querySelector (QuerySelector "#app") (DOM.htmlDocumentToParentNode document))
--      RDOM.render (R.createFactory (R.createClass spec) {}) container
--
-- TODO: Pagination Details are not available from the BackEnd
-- TODO: PageSize Change manually sets the totalPages, need to get from backend and reload the data
-- TODO: Search is pending
-- TODO: Delete is pending
-- TODO: Fav is pending
-- TODO: Sort is Pending
-- TODO: Filter is Pending
-- TODO: When a pagination link is clicked, reload data. Right now it doesn't make sense to reload mock data.

data Action
  = LoadData
  | ChangePageSize PageSizes
  | ChangePage Int

type State = CorpusTableData

type CorpusTableData = TableData Corpus

newtype TableData a
  = TableData
    { rows         :: Array { row    :: a
                            , delete :: Boolean
                            }
    , totalPages   :: Int
    , currentPage  :: Int
    , pageSize     :: PageSizes
    , totalRecords :: Int
    , title        :: String
   -- , tree         :: FTree
    }

newtype Corpus
  = Corpus
    { _id    :: Int
    , url    :: String
    , date   :: String
    , title  :: String
    , source :: String
    , fav    :: Boolean
    , ngramCount :: Int
    }



derive instance genericCorpus :: Generic Corpus

instance showCorpus :: Show Corpus where
  show = gShow


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
    title  <- obj .? "nom"
    source <- obj .? "fonction"
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
filterSpec :: forall eff props. Spec eff State props Action
filterSpec = simpleSpec defaultPerformAction render
  where
    render d p s c = [div [] [ text "    Filter "
                     , input [] []
                     ]]

layoutDocview :: Spec _ State _ Action
layoutDocview = simpleSpec performAction render
  where
    render :: Render State _ Action
    render dispatch _ state@(TableData d) _ =
      [ div [className "container1"]
        [ div [className "row"]
          [
           div [className "col-md-12"]
            [ p''
            , div [] [ text "    Filter ", input [] []]
            , br' []
            , div [className "row"]
              [  div [className "col-md-1"] [b [] [text d.title]]
              , div [className "col-md-2"] [sizeDD d.pageSize dispatch]
              , div [className "col-md-3"] [textDescription d.currentPage d.pageSize d.totalRecords]
              , div [className "col-md-3"] [pagination dispatch d.totalPages d.currentPage]
                     ]
            , table [ className "table"]
              [thead  [ className "thead-dark"]
                         [tr [] [ th [scope "col"] [ b' [text ""]    ]
                                , th [scope "col"] [ b' [text "Date"]]
                                , th [scope "col"] [ b' [text "Name"]   ]
                                --, th [scope "col"] [ b' [text "Title"]   ]
                                --, th [scope "col"] [ b' [text "Source"]  ]
                                , th [scope "col"] [ b' [text "Fonction"]  ]
                                , th [scope "col"] [ b' [text "Delete"]  ]
                                ]
                         ]
              , tbody [] $ map showRow d.rows
              ]
            ]
          ]
        ]
      ]


performAction :: PerformAction _ State _ Action
performAction (ChangePageSize ps) _ _ = void (cotransform (\state ->  changePageSize ps state ))

performAction (ChangePage p) _ _ = void (cotransform (\(TableData td) -> TableData $ td { currentPage = p} ))

performAction LoadData _ _ = void do
  res <- lift $ loadPage
  case res of
     Left err      -> cotransform $ \state ->  state
     Right resData -> modifyState (\s -> resData)


loadPage :: forall eff. Aff (ajax :: AJAX, console :: CONSOLE | eff) (Either String CorpusTableData)
loadPage = do
  res <- get "http://localhost:8008/node/452132/children"
  -- res <- get "http://localhost:8008/corpus/472764/facet/documents/table?offset=0&limit=10"
  case res of
     Left err -> do
       _ <- liftEff $ log $ show err
       pure $ Left $ show err
     Right resData -> do
       let docs = toTableData (res2corpus $ resData)
       _ <- liftEff $ log $ show $ map (\({ row: r, delete :_}) -> show r) ((\(TableData docs') -> docs'.rows) docs)
       _ <- liftEff $ log $ show "loading"
       pure $ Right docs
      where
        res2corpus :: Array Response -> Array Corpus
        res2corpus rs = map (\(Response r) ->
          Corpus { _id : r.cid
          , url    : ""
          , date   :  r.created
          , title  : (\(Hyperdata r) -> r.title) r.hyperdata
          , source : (\(Hyperdata r) -> r.source) r.hyperdata
          , fav    : r.favorite
          , ngramCount : r.ngramCount
         }) rs


        toTableData :: Array Corpus -> CorpusTableData
        toTableData ds = TableData
                { rows         : map (\d -> { row : d , delete : false}) ds
                , totalPages   : 474
                , currentPage  : 1
                , pageSize     : PS100
                , totalRecords : 47361
                , title        : "Documents"
             --   , tree         : exampleTree
                }

---------------------------------------------------------

sampleData' :: Corpus
sampleData' = Corpus {_id : 1, url : "", date : "date3", title : "title", source : "source", fav : false, ngramCount : 1}
--
sampleData :: Array Corpus
--sampleData = replicate 10 sampleData'
sampleData = map (\(Tuple t s) -> Corpus {_id : 1, url : "", date : "2017", title: t, source: s, fav : false, ngramCount : 10}) sampleDocuments

sampleDocuments :: Array (Tuple String String)
sampleDocuments = [Tuple "Macroscopic dynamics of the fusion process" "Journal de Physique Lettres",Tuple "Effects of static and cyclic fatigue at high temperature upon reaction bonded silicon nitride" "Journal de Physique Colloques",Tuple "Reliability of metal/glass-ceramic junctions made by solid state bonding" "Journal de Physique Colloques",Tuple "High temperature mechanical properties and intergranular structure of sialons" "Journal de Physique Colloques",Tuple "SOLUTIONS OF THE LANDAU-VLASOV EQUATION IN NUCLEAR PHYSICS" "Journal de Physique Colloques",Tuple "A STUDY ON THE FUSION REACTION 139La + 12C AT 50 MeV/u WITH THE VUU EQUATION" "Journal de Physique Colloques",Tuple "Atomic structure of \"vitreous\" interfacial films in sialon" "Journal de Physique Colloques",Tuple "MICROSTRUCTURAL AND ANALYTICAL CHARACTERIZATION OF Al2O3/Al-Mg COMPOSITE INTERFACES" "Journal de Physique Colloques",Tuple "Development of oxidation resistant high temperature NbTiAl alloys and intermetallics" "Journal de Physique IV Colloque",Tuple "Determination of brazed joint constitutive law by inverse method" "Journal de Physique IV Colloque",Tuple "Two dimensional estimates from ocean SAR images" "Nonlinear Processes in Geophysics",Tuple "Comparison Between New Carbon Nanostructures Produced by Plasma with Industrial Carbon Black Grades" "Journal de Physique III",Tuple "<i>Letter to the Editor:</i> SCIPION, a new flexible ionospheric sounder in Senegal" "Annales Geophysicae",Tuple "Is reducibility in nuclear multifragmentation related to thermal scaling?" "Physics Letters B",Tuple "Independence of fragment charge distributions of the size of heavy multifragmenting sources" "Physics Letters B",Tuple "Hard photons and neutral pions as probes of hot and dense nuclear matter" "Nuclear Physics A",Tuple "Surveying the nuclear caloric curve" "Physics Letters B",Tuple "A hot expanding source in 50 A MeV Xe+Sn central reactions" "Physics Letters B"]




data' :: Array Corpus -> Array {row :: Corpus, delete :: Boolean}
data' = map {row : _, delete : false}

sdata :: Array { row :: Corpus, delete :: Boolean }
sdata = data' sampleData


tdata = TableData
        { rows         : sdata
        , totalPages   : 10
        , currentPage  : 1
        , pageSize     : PS10
        , totalRecords : 100
        , title        : "Documents"
     --   , tree         : exampleTree
        }


showRow :: {row :: Corpus, delete :: Boolean} -> ReactElement
showRow {row : (Corpus c), delete} =
  tr []
  [ td [] [div [className $ fa <> "fa-star"][]]
  -- TODO show date: Year-Month-Day only
  , td [] [text c.date]
  , td [] [ a [ if c.fav == true then href "#/userPage" else href "#/documentView/1" ] [ text c.title ] ]
  , td [] [text c.source]
  , td [] [input [ _type "checkbox"] []]
  ]
    where
      fa = case c.fav of
                true  -> "fas "
                false -> "far "



--------------------------------------------------------------
-- | Action
-- ChangePageSize
changePageSize :: PageSizes -> CorpusTableData -> CorpusTableData
changePageSize ps (TableData td) =
  TableData $ td { pageSize      = ps
                 , totalPages    = td.totalRecords / pageSizes2Int ps
                 , currentPage   = 1
                 }


data PageSizes = PS10 | PS20 | PS50 | PS100

derive instance eqPageSizes :: Eq PageSizes

instance showPageSize :: Show PageSizes where
  show PS10 = "10"
  show PS20 = "20"
  show PS50 = "50"
  show PS100 = "100"

pageSizes2Int :: PageSizes -> Int
pageSizes2Int PS10 = 10
pageSizes2Int PS20 = 20
pageSizes2Int PS50 = 50
pageSizes2Int PS100 = 100

aryPS :: Array PageSizes
aryPS = [PS10, PS20, PS50, PS100]

string2PageSize :: String -> PageSizes
string2PageSize "10" = PS10
string2PageSize "20" = PS20
string2PageSize "50" = PS50
string2PageSize "100" = PS100
string2PageSize _    = PS10

sizeDD :: PageSizes -> _ -> ReactElement
sizeDD ps d
  = span []
    [ text "Show : "
    , select [onChange (\e -> d (ChangePageSize $ string2PageSize $ (unsafeCoerce e).target.value))] $ map (optps ps) aryPS
    ]

optps :: PageSizes -> PageSizes -> ReactElement
optps cv val = option [ selected (cv == val), value $ show val ] [text $ show val]


textDescription :: Int -> PageSizes -> Int -> ReactElement
textDescription currPage pageSize totalRecords
  =  div [className "row1"]
          [ div [className ""]
                [ text $ "Showing " <> show start <> " to " <> show end <> " of " <> show totalRecords ]
          ]
    where
      start = (currPage - 1) * pageSizes2Int pageSize + 1
      end' = currPage * pageSizes2Int pageSize
      end  = if end' > totalRecords then totalRecords else end'


pagination :: _ -> Int -> Int -> ReactElement
pagination d tp cp
  = span [] $
    [ text "Pages: ", prev, first, ldots]
    <>
    lnums
    <>
    [b' [text $ " " <> show cp <> " "]]
    <>
    rnums
    <>
    [ rdots, last, next ]
    where
      prev = if cp == 1 then
               text " Previous "
               else
               span []
               [ text " "
               , a [ href "javascript:void()"
                   , onClick (\e -> d $ ChangePage $ cp - 1)
                   ] [text "Previous"]
               , text " "
               ]
      next = if cp == tp then
               text " Next "
               else
               span []
               [ text " "
               , a [ href "javascript:void()"
                   , onClick (\e -> d $ ChangePage $ cp + 1)
                   ] [text "Next"]
               , text " "
               ]
      first = if cp == 1 then
                text ""
                else
                span []
                [ text " "
                , a [ href "javascript:void()"
                    , onClick (\e -> d $ ChangePage 1)
                    ] [text "1"]
                , text " "
                ]
      last = if cp == tp then
               text ""
             else
               span []
               [ text " "
               , a [ href "javascript:void()"
                   , onClick (\e -> d $ ChangePage tp)
                   ] [text $ show tp]
               , text " "
               ]
      ldots = if cp >= 5 then
                text " ... "
                else
                text ""
      rdots = if cp + 3 < tp then
                text " ... "
                else
                text ""
      lnums = map (\i -> fnmid d i) $ filter (lessthan 1) [cp - 2, cp - 1]
      rnums = map (\i -> fnmid d i) $ filter (greaterthan tp) [cp + 1, cp + 2]

fnmid :: _ -> Int -> ReactElement
fnmid d i
  = span []
    [ text " "
    , a [ href "javascript:void()"
        , onClick (\e -> d $ ChangePage i)
        ] [text $ show i]
    , text " "
    ]


lessthan :: forall t28. Ord t28 => t28 -> t28 -> Boolean
lessthan x y = x < y

greaterthan :: forall t28. Ord t28 => t28 -> t28 -> Boolean
greaterthan x y = x > y
