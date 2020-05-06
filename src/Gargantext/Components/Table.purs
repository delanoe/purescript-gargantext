module Gargantext.Components.Table where

import Prelude
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Reactix as R
import Reactix.DOM.HTML as H
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Reactix (effectLink)

type TableContainerProps =
  ( pageSizeControl     :: R.Element
  , pageSizeDescription :: R.Element
  , paginationLinks     :: R.Element
  , tableHead           :: R.Element
  , tableBody           :: Array R.Element
  )

type Row = { row :: R.Element, delete :: Boolean }
type Rows = Array Row

type OrderBy = Maybe (OrderByDirection ColumnName)

type Params = { offset :: Int, limit :: Int, orderBy :: OrderBy }

newtype ColumnName = ColumnName String

derive instance genericColumnName :: Generic ColumnName _

instance showColumnName :: Show ColumnName where
  show = genericShow

derive instance eqColumnName :: Eq ColumnName

columnName :: ColumnName -> String
columnName (ColumnName c) = c

data OrderByDirection a = ASC a | DESC a

derive instance genericOrderByDirection :: Generic (OrderByDirection a) _

instance showOrderByDirection :: Show a => Show (OrderByDirection a) where
  show = genericShow

derive instance eqOrderByDirection :: Eq a => Eq (OrderByDirection a)

type Props =
  ( colNames     :: Array ColumnName
  , wrapColElts  :: ColumnName -> Array R.Element -> Array R.Element
                 -- ^ Use `const identity` as a default behavior.
  , totalRecords :: Int
  , params       :: R.State Params
  , rows         :: Rows
  , container    :: Record TableContainerProps -> R.Element
  )

type State =
  { page     :: Int
  , pageSize :: PageSizes
  , orderBy  :: OrderBy
  }

paramsState :: Params -> State
paramsState {offset, limit, orderBy} = {pageSize, page, orderBy}
  where
    pageSize = int2PageSizes limit
    page = offset / limit + 1

stateParams :: State -> Params
stateParams {pageSize, page, orderBy} = {offset, limit, orderBy}
  where
    limit = pageSizes2Int pageSize
    offset = limit * (page - 1)

type TableHeaderLayoutProps =
  ( title :: String
  , desc  :: String
  , query :: String
  , date  :: String
  , user  :: String
  )

initialParams :: Params
initialParams = stateParams {page: 1, pageSize: PS10, orderBy: Nothing}
-- TODO: Not sure this is the right place for this

tableHeaderLayout :: Record TableHeaderLayoutProps -> R.Element
tableHeaderLayout props = R.createElement tableHeaderLayoutCpt props []

tableHeaderLayoutCpt :: R.Component TableHeaderLayoutProps
tableHeaderLayoutCpt = R.hooksComponent "G.C.Table.tableHeaderLayout" cpt
  where
    cpt {title, desc, query, date, user} _ =
      pure $ R.fragment
      [ R2.row
        [ H.div {className: "col-md-3"} [ H.h3 {} [H.text title] ]
        , H.div {className: "col-md-9"}
          [ H.hr {style: {height: "2px", backgroundColor: "black"}} ]
        ]
      , R2.row
        [ H.div {className: "jumbotron1", style: {padding: "12px 0px 20px 12px"}}
          [ H.div {className: "col-md-8 content"}
            [ H.p {}
              [ H.i {className: "glyphicon glyphicon-globe"} []
              , H.text $ " " <> desc
          ]
            , H.p {}
              [ H.i {className: "glyphicon glyphicon-zoom-in"} []
              , H.text $ " " <> query
              ]
            ]
          , H.div {className: "col-md-4 content"}
            [ H.p {}
              [ H.i {className: "glyphicon glyphicon-calendar"} []
              , H.text $ " " <> date
              ]
            , H.p {}
              [ H.i {className: "glyphicon glyphicon-user"} []
              , H.text $ " " <> user
              ]
            ]
          ]
        ]
      ]
  
table :: Record Props -> R.Element
table props = R.createElement tableCpt props []

tableCpt :: R.Component Props
tableCpt = R.hooksComponent "G.C.Table.table" cpt
  where
    cpt {container, colNames, wrapColElts, totalRecords, rows, params} _ = do
      let
        state = paramsState $ fst params
        ps = pageSizes2Int state.pageSize
        totalPages = (totalRecords / ps) + min 1 (totalRecords `mod` ps)
        colHeader :: ColumnName -> R.Element
        colHeader c = H.th {scope: "col"} [ H.b {} cs ]
          where
            lnk mc = effectLink $ snd params $ _ { orderBy = mc }
            cs :: Array R.Element
            cs =
              wrapColElts c $
              case state.orderBy of
                Just (ASC d)  | c == d -> [lnk (Just (DESC c)) "ASC ", lnk Nothing (columnName c)]
                Just (DESC d) | c == d -> [lnk (Just (ASC  c)) "DESC ",  lnk Nothing (columnName c)]
                _ -> [lnk (Just (ASC c)) (columnName c)]
      pure $ container
        { pageSizeControl: sizeDD { params }
        , pageSizeDescription: textDescription state.page state.pageSize totalRecords
        , paginationLinks: pagination params totalPages
        , tableBody: map _.row rows
        , tableHead: H.tr {} (colHeader <$> colNames)
        }

makeRow :: Array R.Element -> R.Element
makeRow els = H.tr {} $ (\c -> H.td {} [c]) <$> els


type FilterRowsParams =
  (
    params :: Params
  )

filterRows :: Record FilterRowsParams -> Rows -> Rows
filterRows { params: { limit, offset, orderBy } } rs = newRs
  where
    newRs = A.take limit $ A.drop offset $ rs

defaultContainer :: {title :: String} -> Record TableContainerProps -> R.Element
defaultContainer {title} props = R.fragment
  [ R2.row
    [ H.div {className: "col-md-4"} [ props.pageSizeDescription ]
    , H.div {className: "col-md-4"} [ props.paginationLinks ]
    , H.div {className: "col-md-4"} [ props.pageSizeControl ]
    ]
  , H.table {className: "table"}
    [ H.thead {className: "thead-dark"} [ props.tableHead ]
    , H.tbody {} props.tableBody
    ]
  ]

-- TODO: this needs to be in Gargantext.Pages.Corpus.Graph.Tabs
graphContainer :: {title :: String} -> Record TableContainerProps -> R.Element
graphContainer {title} props =
  -- TODO title in tabs name (above)
  H.table {className: "table"}
  [ H.thead {className: "thead-dark"} [ props.tableHead ]
  , H.tbody {} props.tableBody
  ]
   -- TODO better rendering of the paginationLinks
   -- , props.pageSizeControl
   -- , props.pageSizeDescription
   -- , props.paginationLinks

type SizeDDProps =
  (
    params :: R.State Params
  )

sizeDD :: Record SizeDDProps -> R.Element
sizeDD p = R.createElement sizeDDCpt p []

sizeDDCpt :: R.Component SizeDDProps
sizeDDCpt = R.hooksComponent "G.C.T.sizeDD" cpt
  where
    cpt {params: params /\ setParams} _ = do
      pure $ H.span {} [
        R2.select { className, defaultValue: show pageSize, on: {change} } sizes
      ]
      where
        {pageSize} = paramsState params
        className = "form-control"
        change e = do
          let ps = string2PageSize $ R2.unsafeEventValue e
          setParams $ \p -> stateParams $ (paramsState p) { pageSize = ps }
        sizes = map option pageSizes
        option size = H.option {value} [H.text value]
          where value = show size

textDescription :: Int -> PageSizes -> Int -> R.Element
textDescription currPage pageSize totalRecords =
  H.div {className: "row1"} [ H.div {className: ""} [ H.text msg ] ] -- TODO or col-md-6 ?
  where
    start = (currPage - 1) * pageSizes2Int pageSize + 1
    end' = currPage * pageSizes2Int pageSize
    end  = if end' > totalRecords then totalRecords else end'
    msg = "Showing " <> show start <> " to " <> show end <> " of " <> show totalRecords

pagination :: R.State Params -> Int -> R.Element
pagination (params /\ setParams) tp =
  H.span {} $
    [ H.text " ", prev, first, ldots]
    <>
    lnums
    <>
    [H.b {} [H.text $ " " <> show page <> " "]]
    <>
    rnums
    <>
    [ rdots, last, next ]
    where
      {page} = paramsState params
      changePage page = setParams $ \p -> stateParams $ (paramsState p) { page = page }
      prev = if page == 1 then
               H.text " Prev. "
             else
               changePageLink (page - 1) "Prev."
      next = if page == tp then
               H.text " Next "
             else
               changePageLink (page + 1) "Next"
      first = if page == 1 then
                H.text ""
              else
                changePageLink' 1
      last = if page == tp then
               H.text ""
             else
               changePageLink' tp
      ldots = if page >= 5 then
                H.text " ... "
                else
                H.text ""
      rdots = if page + 3 < tp then
                H.text " ... "
                else
                H.text ""
      lnums = map changePageLink' $ A.filter (1  < _) [page - 2, page - 1]
      rnums = map changePageLink' $ A.filter (tp > _) [page + 1, page + 2]

      changePageLink :: Int -> String -> R.Element
      changePageLink i s =
        H.span {}
          [ H.text " "
          , effectLink (changePage i) s
          , H.text " "
          ]

      changePageLink' :: Int -> R.Element
      changePageLink' i = changePageLink i (show i)

data PageSizes = PS10 | PS20 | PS50 | PS100 | PS200

derive instance eqPageSizes :: Eq PageSizes

instance showPageSize :: Show PageSizes where
  show PS10  = "10"
  show PS20  = "20"
  show PS50  = "50"
  show PS100 = "100"
  show PS200 = "200"

int2PageSizes :: Int -> PageSizes
int2PageSizes i = string2PageSize $ show i

pageSizes2Int :: PageSizes -> Int
pageSizes2Int PS10  = 10
pageSizes2Int PS20  = 20
pageSizes2Int PS50  = 50
pageSizes2Int PS100 = 100
pageSizes2Int PS200 = 200

pageSizes :: Array PageSizes
pageSizes = [PS10, PS20, PS50, PS100, PS200]

string2PageSize :: String -> PageSizes
string2PageSize "10" = PS10
string2PageSize "20" = PS20
string2PageSize "50" = PS50
string2PageSize "100" = PS100
string2PageSize "200" = PS200
string2PageSize _    = PS10
