module DocView where

import Data.Argonaut
import Data.Generic (class Generic, gShow)

import Chart (histogram2, p'')
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (QuerySelector(..))
import DOM.Node.ParentNode (querySelector) as DOM
import Data.Array (filter, replicate)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (fromJust)
import Data.MediaType.Common (applicationJSON)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Ord, class Show, Unit, bind, map, not, pure, show, void, ($), (*), (+), (-), (/), (<), (<$>), (<>), (==), (>), (>=), (>>=))
import React (ReactElement)
import React as R
import React.DOM (a, b, b', br', div, h3, i, input, li, option, select, span, table, tbody, td, text, thead, th, tr, ul, nav)
import React.DOM.Props (Props, _type, className, href, onChange, onClick, selected, value, scope, _id, role, _data, aria)
import ReactDOM as RDOM
import Thermite (PerformAction, Render, Spec, cotransform, createReactSpec, defaultPerformAction, modifyState, simpleSpec)
import Unsafe.Coerce (unsafeCoerce)

import Gargantext.REST (get)


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



instance decodeHyperdata :: DecodeJson Hyperdata where
  decodeJson json = do
    obj    <- decodeJson json
    title  <- obj .? "title"
    source <- obj .? "source"
    pure $ Hyperdata { title,source }


instance decodeResponse :: DecodeJson Response where
  decodeJson json = do
    obj        <- decodeJson json
    cid        <- obj .? "id"
    created    <- obj .? "created"
    favorite   <- obj .? "favorite"
    ngramCount <- obj .? "ngramCount"
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
            , h3 [] [text "Chart Title"]
            , histogram2
            , p''
            , br' []
            , div [] [ b [] [text d.title]
                     , sizeDD d.pageSize dispatch
                     , textDescription d.currentPage d.pageSize d.totalRecords
                     , pagination dispatch d.totalPages d.currentPage
                     ]
            , table [ className "table"]
              [thead  [ className "thead-dark"]
                         [tr [] [ th [scope "col"] [ b' [text ""]    ]
                                , th [scope "col"] [ b' [text "Date"]]
                                , th [scope "col"] [ b' [text "Title"]   ]
                                , th [scope "col"] [ b' [text "Source"]  ]
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
  res <- get "http://localhost:8008/corpus/472764/facet/documents/table"
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
                , totalPages   : 10
                , currentPage  : 1
                , pageSize     : PS10
                , totalRecords : 100
                , title        : "Documents"
             --   , tree         : exampleTree
                }

---------------------------------------------------------

sampleData' :: Corpus
sampleData' = Corpus {_id : 1, url : "", date : "date3", title : "title", source : "source", fav : false, ngramCount : 1}
--
sampleData :: Array Corpus
sampleData = replicate 10 sampleData'

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
  , td [] [ a [ href "#/documentView/1"] [ text c.title ] ]
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
  =  div [className "row"]
          [ div [className "col-md-12"]
                [ text $ "Showing " <> show start <> " to " <> show end <> " of " <> show totalRecords ]
          ]
    where
      start = (currPage - 1) * pageSizes2Int pageSize + 1
      end' = currPage * pageSizes2Int pageSize
      end  = if end' > totalRecords then totalRecords else end'


pagination :: _ -> Int -> Int -> ReactElement
pagination d tp cp
  = span [] $
    [ text "Pages: "
    , prev
    , first
    , ldots
    ]
    <>
    lnums
    <>
    [b' [text $ " " <> show cp <> " "]]
    <>
    rnums
    <>
    [ rdots
    , last
    , next
    ]
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


