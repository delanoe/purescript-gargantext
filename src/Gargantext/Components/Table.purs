module Gargantext.Components.Table where

import Data.Array (filter)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Class (liftEffect)
import React (ReactElement, ReactClass, Children, createElement)
import React.DOM (a, b, b', p, i, h3, hr, div, option, select, span, table, tbody, td, text, th, thead, tr)
import React.DOM.Props (className, href, onChange, onClick, scope, selected, value, style)
import Thermite (PerformAction, Render, Spec, modifyState_, simpleSpec, StateCoTransformer, createClass)
import Unsafe.Coerce (unsafeCoerce)

import Gargantext.Prelude

type TableContainerProps =
  { pageSizeControl     :: ReactElement
  , pageSizeDescription :: ReactElement
  , paginationLinks     :: ReactElement
  , tableHead           :: ReactElement
  , tableBody           :: Array ReactElement
  }

type Rows = Array { row    :: Array ReactElement
                  , delete :: Boolean
                  }

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

type Props' =
  ( colNames     :: Array ColumnName
  , totalRecords :: Int
  , setParams    :: Params -> Effect Unit
  , rows         :: Rows
  , container    :: TableContainerProps -> Array ReactElement
  )

type Props = Record Props'

type State =
  { currentPage :: Int
  , pageSize    :: PageSizes
  , orderBy     :: OrderBy
  }

initialState :: State
initialState =
  { currentPage  : 1
  , pageSize     : PS10
  , orderBy      : Nothing
  }

initialParams :: Params
initialParams = stateParams initialState

data Action
  = ChangePageSize PageSizes
  | ChangePage     Int
  | ChangeOrderBy  OrderBy

type ChangePageAction = Int -> Effect Unit

-- | Action
-- ChangePageSize
changePageSize :: PageSizes -> State -> State
changePageSize ps td =
  td { pageSize      = ps
     , currentPage   = 1
     }

-- TODO: Not sure this is the right place for this function.
renderTableHeaderLayout :: { title :: String
                           , desc  :: String
                           , query :: String
                           , date  :: String
                           , user  :: String
                           } -> Array ReactElement
renderTableHeaderLayout {title, desc, query, date, user} =
  [ div [className "row"]
    [ div [className "col-md-3"] [ h3 [] [text title] ]
    , div [className "col-md-9"] [ hr [style {height : "2px",backgroundColor : "black"}] ]
    ]
  , div [className "row"] [ div [className "jumbotron1", style {padding : "12px 0px 20px 12px"}]
        [ div [ className "col-md-8 content"]
              [ p [] [ i [className "fa fa-globe"] []
                     , text $ " " <> desc
                     ]
              , p [] [ i [className "fab fa-searchengin"] []
                     , text $ " " <> query
                     ]
              ]
        , div [ className "col-md-4 content"]
              [ p [] [ i [className "fa fa-calendar"] []
                     , text $ " " <> date
                     ]
              , p [] [ i [className "fa fa-user"] []
                     , text $ " " <> user
                     ]
              ]
        ]
    ]
  ]

tableSpec :: Spec State Props Action
tableSpec = simpleSpec performAction render
  where
    modifyStateAndReload :: (State -> State) -> Props -> State -> StateCoTransformer State Unit
    modifyStateAndReload f {setParams} state = do
      logs "modifyStateAndReload" -- TODO rename
      modifyState_ f
      liftEffect $ setParams $ stateParams $ f state

    performAction :: PerformAction State Props Action
    performAction (ChangePageSize ps) =
      modifyStateAndReload $ changePageSize ps
    performAction (ChangePage p) =
      modifyStateAndReload $ _ { currentPage = p }
    performAction (ChangeOrderBy mc) =
      modifyStateAndReload $ _ { orderBy = mc }

    renderColHeader :: (OrderBy -> Effect Unit)
                    -> OrderBy
                    -> ColumnName -> ReactElement
    renderColHeader changeOrderBy currentOrderBy c =
      th [scope "col"] [ b' cs ]
      where
        lnk mc = effectLink (changeOrderBy mc)
        cs :: Array ReactElement
        cs =
          case currentOrderBy of
            Just (ASC d)  | c == d -> [lnk (Just (DESC c)) "DESC ",  lnk Nothing (columnName c)]
            Just (DESC d) | c == d -> [lnk (Just (ASC  c)) "ASC ", lnk Nothing (columnName c)]
            _ -> [lnk (Just (ASC c)) (columnName c)]

    render :: Render State Props Action
    render dispatch {container, colNames, totalRecords, rows}
                    {pageSize, currentPage, orderBy} _ =
      container
        { pageSizeControl: sizeDD pageSize dispatch
        , pageSizeDescription: textDescription currentPage pageSize totalRecords
        , paginationLinks: pagination (dispatch <<< ChangePage) totalPages currentPage
        , tableHead:
            tr [] (renderColHeader (dispatch <<< ChangeOrderBy) orderBy <$> colNames)
        , tableBody:
            map (tr [] <<< map (\c -> td [] [c]) <<< _.row) rows
        }
      where
        ps = pageSizes2Int pageSize
        totalPages = (totalRecords / ps) + min 1 (totalRecords `mod` ps)

defaultContainer :: {title :: String} -> TableContainerProps -> Array ReactElement
defaultContainer {title} props =
  [ div [className "row"]
    [ div [className "col-md-4"] [props.pageSizeDescription]
    , div [className "col-md-4"] [props.paginationLinks]
    , div [className "col-md-4"] [props.pageSizeControl]
    ]
  , table [ className "table"]
    [ thead [className "thead-dark"] [ props.tableHead ]
    , tbody [] props.tableBody
    ]
  ]

-- TODO: this needs to be in Gargantext.Pages.Corpus.Graph.Tabs
graphContainer :: {title :: String} -> TableContainerProps -> Array ReactElement
graphContainer {title} props =
  [ -- TODO title in tabs name (above)
    table [ className "table"]
    [ thead [className "thead-dark"] [ props.tableHead ]
    , tbody [] props.tableBody
    ]
   -- TODO better rendering of the paginationLinks
   -- , props.pageSizeControl
   -- , props.pageSizeDescription
   -- , props.paginationLinks
  ]



stateParams :: State -> Params
stateParams {pageSize, currentPage, orderBy} = {offset, limit, orderBy}
  where
    limit = pageSizes2Int pageSize
    offset = limit * (currentPage - 1)

tableClass :: ReactClass {children :: Children | Props'}
tableClass = createClass "Table" tableSpec (const initialState)

tableElt :: Props -> ReactElement
tableElt props = createElement tableClass props []

sizeDD :: PageSizes -> (Action -> Effect Unit) -> ReactElement
sizeDD ps d
  = span []
    [ select [ className "form-control"
             , onChange (\e -> d (ChangePageSize $ string2PageSize $ (unsafeCoerce e).target.value))
             ] $ map (optps ps) aryPS
    ]

textDescription :: Int -> PageSizes -> Int -> ReactElement
textDescription currPage pageSize totalRecords
  =  div [className "row1"]
          [ div [className ""] -- TODO or col-md-6 ?
                [ text $ "Showing " <> show start <> " to " <> show end <> " of " <> show totalRecords ]
          ]
    where
      start = (currPage - 1) * pageSizes2Int pageSize + 1
      end' = currPage * pageSizes2Int pageSize
      end  = if end' > totalRecords then totalRecords else end'

effectLink :: Effect Unit -> String -> ReactElement
effectLink eff msg = a [onClick $ const eff] [text msg]

pagination :: ChangePageAction -> Int -> Int -> ReactElement
pagination changePage tp cp
  = span [] $
    [ text " ", prev, first, ldots]
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
               text " Prev. "
             else
               changePageLink (cp - 1) "Prev."
      next = if cp == tp then
               text " Next "
             else
               changePageLink (cp + 1) "Next"
      first = if cp == 1 then
                text ""
              else
                changePageLink' 1
      last = if cp == tp then
               text ""
             else
               changePageLink' tp
      ldots = if cp >= 5 then
                text " ... "
                else
                text ""
      rdots = if cp + 3 < tp then
                text " ... "
                else
                text ""
      lnums = map changePageLink' $ filter (1  < _) [cp - 2, cp - 1]
      rnums = map changePageLink' $ filter (tp > _) [cp + 1, cp + 2]

      changePageLink :: Int -> String -> ReactElement
      changePageLink i s = span []
          [ text " "
          , effectLink (changePage i) s
          , text " "
          ]

      changePageLink' :: Int -> ReactElement
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

optps :: PageSizes -> PageSizes -> ReactElement
optps cv val = option [ selected (cv == val), value $ show val ] [text $ show val]
