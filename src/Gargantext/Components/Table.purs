module Gargantext.Components.Table where

import Data.Array (filter)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Reactix as R
import Reactix.DOM.HTML as H
import Thermite (PerformAction, Render, Spec, modifyState_, simpleSpec, StateCoTransformer, createClass)
import Unsafe.Coerce (unsafeCoerce)

import Gargantext.Prelude
import Gargantext.Utils.Reactix as R2

type TableContainerProps =
  { pageSizeControl     :: R.Element
  , pageSizeDescription :: R.Element
  , paginationLinks     :: R.Element
  , tableHead           :: R.Element
  , tableBody           :: Array R.Element
  }

type Rows = Array { row    :: Array R.Element
                  , delete :: Boolean
                  }

type CurrentPage = Int
type OrderBy = Maybe (OrderByDirection ColumnName)

type TableParams = {currentPage :: CurrentPage, pageSize :: PageSizes, orderBy :: OrderBy}

initialTableParams = {
    currentPage: 1
  , pageSize: PS10
  , orderBy: Nothing
  }

type Params = { offset :: Int, limit :: Int, orderBy :: OrderBy }

initialParams = toParams initialTableParams

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

type Props' =
  ( colNames     :: Array ColumnName
  , totalRecords :: Int
  , rows         :: Rows
  , container    :: TableContainerProps -> R.Element
  )

type Props = Record Props'

-- TODO: Not sure this is the right place for this function.
renderTableHeaderLayout :: { title :: String
                           , desc  :: String
                           , query :: String
                           , date  :: String
                           , user  :: String
                           } -> Array R.Element
renderTableHeaderLayout {title, desc, query, date, user} =
  [ H.div {className: "row"}
    [ H.div {className: "col-md-3"} [ H.h3 {} [H.text title] ]
    , H.div {className: "col-md-9"} [ H.hr {style: {height : "2px",backgroundColor : "black"}} ]
    ]
  , H.div {className: "row"} [ H.div {className: "jumbotron1", style: {padding : "12px 0px 20px 12px"}}
        [ H.div { className: "col-md-8 content"}
              [ H.p {} [ H.i {className: "glyphicon glyphicon-globe"} []
                     , H.text $ " " <> desc
                     ]
              , H.p {} [ H.i {className: "glyphicon glyphicon-zoom-in"} []
                     , H.text $ " " <> query
                     ]
              ]
        , H.div { className: "col-md-4 content"}
              [ H.p {} [ H.i {className: "glyphicon glyphicon-calendar"} []
                     , H.text $ " " <> date
                     ]
              , H.p {} [ H.i {className: "glyphicon glyphicon-user"} []
                     , H.text $ " " <> user
                     ]
              ]
        ]
    ]
  ]

-- tableSpec :: Spec State Props Action
-- tableSpec = simpleSpec performAction render
tableSpec :: R.State TableParams -> Props -> R.Element
tableSpec tableParams p = R.createElement el p []
  where
    el = R.hooksComponent "tableSpec" cpt
    cpt {container, colNames, totalRecords, rows} _ = do
      pure $ container
        { pageSizeControl: sizeDD tableParams
        , pageSizeDescription: textDescription tableParams totalRecords
        , paginationLinks: pagination tableParams totalPages
        , tableHead:
          H.tr {} (renderColHeader tableParams <$> colNames)
        , tableBody:
          map (H.tr {} <<< map (\c -> H.td {} [c]) <<< _.row) rows
        }
      where
        ps = pageSizes2Int ((fst tableParams).pageSize)
        totalPages = (totalRecords / ps) + min 1 (totalRecords `mod` ps)

    renderColHeader :: R.State TableParams -> ColumnName -> R.Element
    renderColHeader ({orderBy} /\ setTableParams) c =
      -- H.th {scope: "col"} [H.text $ columnName c]
      H.th {scope: "col"} [H.b {} cs]
      where
        lnk mc = effectLink $ \_ -> setTableParams $ \tp -> tp {orderBy = mc}
        cs :: Array R.Element
        cs =
          case orderBy of
            Just (ASC d)  | c == d -> [lnk (Just (DESC c)) "DESC ", lnk Nothing (columnName c)]
            Just (DESC d) | c == d -> [lnk (Just (ASC  c)) "ASC ", lnk Nothing (columnName c)]
            _ -> [lnk (Just (ASC c)) (columnName c)]

defaultContainer :: {title :: String} -> TableContainerProps -> R.Element
defaultContainer {title} props = R.createElement el props []
  where
    el = R.hooksComponent "defaultContainer" cpt
    cpt p _ = do
      pure $ H.div {} [
        H.div {className: "row"}
        [ H.div {className: "col-md-4"} [props.pageSizeDescription]
        , H.div {className: "col-md-4"} [props.paginationLinks]
        , H.div {className: "col-md-4"} [props.pageSizeControl]
        ]
      , H.table { className: "table"}
        [ H.thead {className: "thead-dark"} [ props.tableHead ]
        , H.tbody {} props.tableBody
        ]
      ]

-- TODO: this needs to be in Gargantext.Pages.Corpus.Graph.Tabs
graphContainer :: {title :: String} -> TableContainerProps -> R.Element
graphContainer {title} props = R.createElement el props []
  where
    el = R.hooksComponent "GraphContainer" cpt
    cpt p _ = do
      --[ -- TODO title in tabs name (above)
      pure $ H.table { className: "table"}
        [ H.thead {className: "thead-dark"} [ props.tableHead ]
        , H.tbody {} props.tableBody
        ]
      -- TODO better rendering of the paginationLinks
      -- , props.pageSizeControl
      -- , props.pageSizeDescription
      -- , props.paginationLinks
      --]



toParams :: TableParams -> Params
toParams {pageSize, currentPage, orderBy} = {offset, limit, orderBy}
  where
    limit = pageSizes2Int pageSize
    offset = limit * (currentPage - 1)

toTableParams :: Params -> TableParams
toTableParams {offset, limit, orderBy} = {pageSize, currentPage, orderBy}
  where
    pageSize = string2PageSize $ show limit
    currentPage = offset / limit + 1

tableElt :: R.State TableParams -> Props -> R.Element
tableElt tableParams p = R.createElement el p []
  where
    el = R.hooksComponent "TableElt" cpt
    cpt props _ = do
      pure $ tableSpec tableParams props

tableEltFromParams :: Params -> Props -> R.Element
tableEltFromParams params p = R.createElement el p []
  where
    el = R.hooksComponent "TableEltFromParams" cpt
    cpt props _ = do
      tableParams <- R.useState' $ toTableParams params

      pure $ tableElt tableParams props

sizeDD :: R.State TableParams -> R.Element
sizeDD tp@({pageSize} /\ setTableParams) = R.createElement el {} []
  where
    el = R.hooksComponent "SizeDD" cpt
    cpt {} _ = do
      pure $ H.span {}
        [ R2.select { className: "form-control"
                    , onChange: onChange
                    } $ map (optps pageSize) aryPS
        ]
    onChange = mkEffectFn1 $ \e -> setTableParams $ \tp ->
      tp {pageSize = string2PageSize $ (unsafeCoerce e).target.value}

textDescription :: R.State TableParams -> Int -> R.Element
textDescription ({currentPage, pageSize} /\ _) totalRecords = R.createElement el {} []
  where
    el = R.hooksComponent "TextDescription" cpt
    cpt {} _ = do
      pure $ H.div {className: "row1"}
        [ H.div {className: ""} -- TODO or col-md-6 ?
          [ H.text $ "Showing " <> show start <> " to " <> show end <> " of " <> show totalRecords ]
        ]
    start = (currentPage - 1) * pageSizes2Int pageSize + 1
    end' = currentPage * pageSizes2Int pageSize
    end  = if end' > totalRecords then totalRecords else end'

effectLink :: forall e. (e -> Effect Unit) -> String -> R.Element
effectLink eff msg = H.a {onClick: mkEffectFn1 eff} [H.text msg]

pagination :: R.State TableParams -> Int -> R.Element
pagination tp@({currentPage} /\ setTableParams) totalPages = R.createElement el {} []
  where
    el = R.hooksComponent "TablePagination" cpt
    cpt {} _ =
      pure $ H.span {} $
        [ H.text " ", prev, first, ldots]
        <>
        lnums
        <>
        [H.b {} [H.text $ " " <> show currentPage <> " "]]
        <>
        rnums
        <>
        [ rdots, last, next ]
      where
        prev = if currentPage == 1 then
                H.text " Prev. "
              else
                changePageLink (currentPage - 1) "Prev."
        next = if currentPage == totalPages then
                H.text " Next "
              else
                changePageLink (currentPage + 1) "Next"
        first = if currentPage == 1 then
                  H.text ""
                else
                  changePageLink' 1
        last = if currentPage == totalPages then
                H.text ""
              else
                changePageLink' totalPages
        ldots = if currentPage >= 5 then
                  H.text " ... "
                  else
                  H.text ""
        rdots = if currentPage + 3 < totalPages then
                  H.text " ... "
                  else
                  H.text ""
        lnums = map changePageLink' $ filter (1  < _) [currentPage - 2, currentPage - 1]
        rnums = map changePageLink' $ filter (totalPages > _) [currentPage + 1, currentPage + 2]

        changePageLink :: Int -> String -> R.Element
        changePageLink i s = H.span {}
            [ H.text " "
            , effectLink (\_ -> setTableParams $ \tp -> tp { currentPage = i }) s
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

pageSizes2Int :: PageSizes -> Int
pageSizes2Int PS10  = 10
pageSizes2Int PS20  = 20
pageSizes2Int PS50  = 50
pageSizes2Int PS100 = 100
pageSizes2Int PS200 = 200

aryPS :: Array PageSizes
aryPS = [PS10, PS20, PS50, PS100, PS200]

string2PageSize :: String -> PageSizes
string2PageSize "10" = PS10
string2PageSize "20" = PS20
string2PageSize "50" = PS50
string2PageSize "100" = PS100
string2PageSize "200" = PS200
string2PageSize _    = PS10

optps :: PageSizes -> PageSizes -> R.Element
optps cv val = H.option { selected: (cv == val), value: show val } [H.text $ show val]
